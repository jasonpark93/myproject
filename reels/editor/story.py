"""스토리(대본 + 장면 카드) → 목소리(녹음 또는 컴퓨터 음성)에 맞춘 시간표.

stories/이름.json
{
  "header": {"label": "요즘 해외에서 난리난", "title": "클로드 영상 자동편집"},
  "scenes": [
    {"say": "릴스 편집, [[클로드]]가 처음부터 끝까지 다 해줍니다.", "card": {"type": "checklist", ...}, "pills": [...], "stamp": {...}},
    ...
  ]
}
- say: 그 장면에서 읽을 말. [[ ]]는 자막 강조색. 녹음은 이 대본을 그대로 읽으면 된다.
- card 안의 at: 그 말이 나올 때 애니메이션(체크·등장)이 일어난다.
"""

from __future__ import annotations

import json
import re
from dataclasses import dataclass, field
from pathlib import Path

import numpy as np

from . import ROOT, SR, analyze, captions, timeline
from .audio import Envelope
from .cards import plain
from .media import decode_audio
from .transcript import clean, norm
from .util import EditorError, file_hash, read_json, stable_hash, write_json

STORIES = ROOT / "stories"
LEAD = 0.12  # 카드가 말보다 살짝 먼저 바뀐다


def find(ref: str) -> Path:
    path = Path(ref).expanduser()
    for candidate in (path, STORIES / ref, STORIES / f"{ref}.json"):
        if candidate.exists() and candidate.is_file():
            return candidate.resolve()
    raise EditorError(f"스토리 파일을 찾지 못했습니다: {ref} (reels/stories/ 에 이름.json)")


def load(path: Path) -> dict:
    try:
        spec = json.loads(Path(path).read_text(encoding="utf-8"))
    except json.JSONDecodeError as exc:
        raise EditorError(f"스토리 파일 형식 오류 ({path.name} {exc.lineno}번째 줄): {exc.msg}") from exc
    scenes = spec.get("scenes")
    if not scenes:
        raise EditorError("scenes가 비어 있습니다.")
    for i, sc in enumerate(scenes, 1):
        if not str(sc.get("say", "")).strip():
            raise EditorError(f"{i}번째 장면에 say(읽을 말)가 없습니다.")
    spec.setdefault("name", Path(path).stem)
    return spec


def script_of(spec: dict) -> tuple:
    """(대본 전체 글자, 장면별 [시작, 끝] 글자 위치, 강조 단어)"""
    parts, ranges, pos = [], [], 0
    keywords = list(spec.get("keywords") or [])
    for sc in spec["scenes"]:
        say = str(sc["say"]).strip()
        keywords += re.findall(r"\[\[(.+?)\]\]", say)
        text = plain(say)
        ranges.append((pos, pos + len(text)))
        parts.append(text)
        pos += len(text) + 1
    return "\n".join(parts), ranges, keywords


def _tokens(script: str) -> list:
    """대본 → [(단어, 시작 위치, 끝 위치)]"""
    return [(m.group(0), m.start(), m.end()) for m in re.finditer(r"\S+", script)]


def _sentences(text: str) -> list:
    out, start = [], 0
    for m in re.finditer(r"[.?!…]+(\s+|$)|\n", text):
        end = m.end()
        if text[start:end].strip():
            out.append((start, end))
        start = end
    if text[start:].strip():
        out.append((start, len(text)))
    return out


@dataclass
class Narration:
    audio: np.ndarray  # 잘라 붙인 목소리 (음량 정리 전)
    words: list  # [{w, s, e, sp}] 완성본 시간
    source: str
    cuts: list = field(default_factory=list)
    stats: dict = field(default_factory=dict)

    @property
    def duration(self) -> float:
        return self.audio.size / SR


# ---------- 목소리 ----------
def from_tts(spec: dict, engine: str | None = None, voice: str | None = None, log=print) -> Narration:
    from . import tts

    engine = tts.pick(engine, voice, log)
    script, ranges, _ = script_of(spec)
    log(f"미리보기 음성 만드는 중… ({tts.describe(engine)})")
    pieces, words = [np.zeros(int(0.15 * SR), np.float32)], []
    t = 0.15
    for k, (a, b) in enumerate(ranges):
        scene_text = script[a:b]
        for sa, sb in _sentences(scene_text):
            sent = scene_text[sa:sb].strip()
            if not norm(sent):
                continue
            clip = tts.speak(sent, engine, voice)
            env = Envelope.from_samples(clip)
            toks = [(w, a + sa + s0 + (len(scene_text[sa:sb]) - len(scene_text[sa:sb].lstrip())), 0) for w, s0, _ in _tokens(sent)]
            spans = env.spans() or [(0.0, clip.size / SR)]
            total = sum(e - s for s, e in spans)
            weights = np.array([len(norm(w)) + 1 for w, _, _ in toks], float)
            bounds = np.concatenate(([0], np.cumsum(weights) / weights.sum())) * total

            def at(off: float) -> float:
                for s, e in spans:
                    if off <= e - s + 1e-9:
                        return s + off
                    off -= e - s
                return spans[-1][1]

            for (w, pos, _), lo, hi in zip(toks, bounds[:-1], bounds[1:]):
                words.append({"w": w, "s": round(t + at(lo), 3), "e": round(t + at(hi) - 0.01, 3), "sp": [pos, pos + len(w)]})
            pieces.append(clip)
            t += clip.size / SR
            gap = 0.12
            pieces.append(np.zeros(int(gap * SR), np.float32))
            t += gap
        pieces.append(np.zeros(int(0.1 * SR), np.float32))
        t += 0.1
    pieces.append(np.zeros(int(0.3 * SR), np.float32))
    return Narration(audio=np.concatenate(pieces), words=words, source=f"미리보기 음성({tts.describe(engine)})")


def from_recording(spec: dict, path: Path, work: Path, settings: dict, model: str | None = None, log=print) -> Narration:
    """녹음 → 음성 인식 → 군말·말더듬·NG·쉼 컷 → 대본에 맞춰 단어마다 대본 위치(sp)를 붙인다."""
    from . import transcript
    from .render import cut_audio

    script, _, _ = script_of(spec)
    samples = decode_audio(path)
    duration = samples.size / SR
    env = Envelope.from_samples(samples)
    words_path = work / "words.json"
    source_id = {"path": str(path), "hash": file_hash(path)}
    cached = read_json(words_path) if words_path.exists() else None
    if cached and cached.get("source") == source_id:
        words = cached["words"]
        log("음성 인식: 이전 결과 재사용")
    else:
        log("음성 인식 중…")
        audio16 = decode_audio(path, sr=16000)
        hot = " ".join(sorted({w for w in re.findall(r"[가-힣A-Za-z0-9]{2,}", script)}, key=len, reverse=True)[:30])
        words, _ = transcript.transcribe(audio16, model or transcript.DEFAULT_MODEL, hotwords=hot, log=log)
        write_json(words_path, {"source": source_id, "words": words})
    words = clean(words)
    stats = {
        "filler": analyze.mark_fillers(words),
        "stutter": analyze.mark_stutters(words),
        "ng": analyze.mark_restarts(words, long_pause=float(settings.get("ng_pause", 1.0))),
    }
    stats.update(analyze.align_script(words, script))
    segs = timeline.build(words, env, settings, duration)
    if not segs:
        raise EditorError("녹음에서 대본과 맞는 말을 찾지 못했습니다. 대본대로 읽었는지 확인해 주세요.")
    tl = timeline.Timeline(segs)
    out = []
    for w in words:
        if w.get("cut") or "sp" not in w:
            continue
        out.append({"w": w["w"], "s": round(tl.to_out(w["s"]), 3), "e": round(tl.to_out(w["e"]), 3), "sp": w["sp"]})
    return Narration(audio=cut_audio(samples, segs), words=out, source=f"녹음 {Path(path).name}", cuts=tl.cuts(words, duration), stats=stats)


# ---------- 장면 시간 ----------
class SceneTiming:
    """카드 애니메이션이 쓰는 시계: at('단어') → 장면 시작 기준 몇 초에 그 말이 나오는지."""

    def __init__(self, index: int, start: float, end: float, words: list, script: str, char_range: tuple):
        self.index = index
        self.start = start  # 카드가 화면에 뜨는 시각(완성본)
        self.end = end
        self.duration = max(0.3, end - start)
        self.words = words
        self.script = script
        self.range = char_range

    def at(self, phrase, default: float) -> float:
        if not phrase:
            return default
        a, b = self.range
        hay = self.script[a:b]
        target = norm(phrase)
        if not target:
            return default
        normed, idx = "", []
        for i, ch in enumerate(hay):
            n = norm(ch)
            if n:
                normed += n
                idx.append(a + i)
        pos = normed.find(target)
        if pos < 0:
            return default
        char = idx[pos]
        for w in self.words:
            if w["sp"][0] <= char < max(w["sp"][1], w["sp"][0] + 1):
                return max(0.0, w["s"] - self.start)
        later = [w for w in self.words if w["sp"][0] >= char]
        return max(0.0, later[0]["s"] - self.start) if later else default


def schedule(spec: dict, nar: Narration) -> list:
    """장면마다 SceneTiming. 장면 k는 그 장면 첫 단어 직전부터 다음 장면 직전까지 화면에 있다."""
    script, ranges, _ = script_of(spec)
    by_scene = [[] for _ in ranges]
    for w in nar.words:
        for k, (a, b) in enumerate(ranges):
            if a <= w["sp"][0] < b + 1:
                by_scene[k].append(w)
                break
    starts = []
    for k, ws in enumerate(by_scene):
        if ws:
            starts.append(max(0.0, min(w["s"] for w in ws) - LEAD))
        else:
            starts.append(None)
    for k in range(len(starts)):  # 말이 하나도 안 잡힌 장면은 앞뒤 사이에 끼워 넣는다
        if starts[k] is None:
            prev = starts[k - 1] if k else 0.0
            nxt = next((s for s in starts[k + 1 :] if s is not None), nar.duration)
            starts[k] = (prev + nxt) / 2
    starts[0] = 0.0
    out = []
    for k, ws in enumerate(by_scene):
        end = starts[k + 1] if k + 1 < len(starts) else nar.duration
        out.append(SceneTiming(k, starts[k], end, ws, script, ranges[k]))
    return out


def caption_list(spec: dict, nar: Narration, max_chars: int) -> list:
    """대본 글자 그대로(오타 없는) 자막. 문장 단위로 나눈 뒤 12자 안팎으로 끊는다."""
    script, ranges, keywords = script_of(spec)
    words = sorted(nar.words, key=lambda w: w["s"])
    caps = []
    for k, (a, b) in enumerate(ranges):
        idx = [i for i, w in enumerate(words) if a <= w["sp"][0] < b + 1]
        if not idx:
            continue
        sent_bounds = [(a + s0, a + s1) for s0, s1 in _sentences(script[a:b])]
        for s0, s1 in sent_bounds:
            sel = [i for i in idx if s0 <= words[i]["sp"][0] < s1]
            if not sel:
                continue
            for cap in captions.auto_captions(words, sel[0], sel[-1], max_chars, keywords, script):
                caps.append({"scene": k + 1, "text": cap["text"], "hl": cap["hl"], "t0": words[cap["from"]]["s"], "t1": words[cap["to"]]["e"]})
    return captions.schedule(caps, nar.duration)


def narration_key(spec: dict, voice_path: Path | None, engine: str | None) -> str:
    script, _, _ = script_of(spec)
    src = {"voice": str(voice_path), "hash": file_hash(voice_path)} if voice_path else {"tts": engine}
    return stable_hash(script, src, "nar1")


# ---------- 업로드 문구 ----------
def upload_text(spec: dict) -> str | None:
    """stories JSON의 upload → 업로드할 때 복사해 쓰는 문구 (제목 3개 중 추천 1개, 설명, 해시태그 5개)."""
    up = spec.get("upload")
    if not up:
        return None
    titles = [str(t).strip() for t in up.get("titles") or [] if str(t).strip()][:3]
    pick = int(up.get("pick", 0)) if titles else 0
    pick = min(max(pick, 0), max(0, len(titles) - 1))
    tags = []
    for tag in up.get("hashtags") or []:
        tag = str(tag).strip().replace(" ", "")
        if tag:
            tags.append(tag if tag.startswith("#") else f"#{tag}")
    lines = [f"# 업로드 문구 — {spec.get('name', '')}", "", "## 제목 (★ 추천)"]
    for i, t in enumerate(titles):
        warn = "  ← 40자 넘음, 잘릴 수 있음" if len(t) > 40 else ""
        lines.append(f"{i + 1}. {'★ ' if i == pick else ''}{t}{warn}")
    lines += ["", "## 설명", "", str(up.get("description", "")).strip(), "", "## 해시태그", "", " ".join(tags[:5]), ""]
    return "\n".join(lines)


# ---------- 참고 영상과 얼마나 비슷한지 ----------
def similarity(spec: dict, reference: str, min_run: int = 10) -> dict:
    """우리 대본과 참고 영상 대본의 겹침. 구조는 따라 해도 문장은 새로 써야 한다(그대로 쓰면 표절·중복 콘텐츠 위험)."""
    from difflib import SequenceMatcher

    script, _, _ = script_of(spec)
    a, b = norm(script), norm(reference)
    if not a or not b:
        return {"ratio": 0.0, "phrases": [], "verdict": "비교할 글자가 없습니다."}
    sm = SequenceMatcher(None, a, b, autojunk=False)
    phrases = [a[blk.a : blk.a + blk.size] for blk in sm.get_matching_blocks() if blk.size >= min_run]
    ratio = sm.ratio()
    if ratio >= 0.5 or len(phrases) >= 3:
        verdict = "너무 비슷합니다. 소재·문장·예시를 바꿔 다시 쓰세요."
    elif ratio >= 0.3 or phrases:
        verdict = "조금 겹칩니다. 똑같은 구절은 다른 표현으로 바꾸는 게 안전합니다."
    else:
        verdict = "괜찮습니다. 구조만 참고하고 문장은 새로 썼습니다."
    return {"ratio": round(ratio, 3), "phrases": phrases, "verdict": verdict}
