"""편집 계획(plan.json): 만들기, 기본값, 계산(컷·자막·줌·효과음의 완성본 시간), 고치기."""

from __future__ import annotations

import copy
import re
from dataclasses import dataclass, field

from . import analyze, captions, sfx as sfx_mod, timeline, zoom
from .util import EditorError, parse_time

VERSION = 1

# 검증해서 고른 기본값 (근거는 reels/README.md '편집 기준' 참고)
DEFAULTS = {
    "gap": 0.15,  # 잘라낸 자리에 남길 쉼(초). 0.1~0.15초가 '빠르지만 숨 막히지 않는' 속도
    "silence": 0.4,  # 이보다 길게 쉬면 자른다
    "lead": 0.1,  # 첫 단어 앞 여백 (0.3초 안에 말이 시작되게)
    "tail": 0.35,  # 마지막 단어 뒤 여백 (말끝이 잘리지 않게)
    "ng_pause": 1.0,  # 이만큼 멈췄다가 같은 말을 다시 하면 NG로 본다
    "caption": {
        "max_chars": 12,
        "size": 1.0,
        "bottom": 0.38,  # 화면 아래에서 38% 높이 (인스타·유튜브 하단 UI를 피함)
        "color": "#FFFFFF",
        "highlight": "#FFE14D",
        "outline": "#000000",
        "pop": True,
        "font": "",
    },
    "title": {"text": "", "duration": 3.0, "size": 1.0, "top": 0.19, "color": "#FFFFFF", "highlight": "#FFE14D"},
    "zoom": {"enabled": True, "punch": 1.15, "slow": 1.08, "max_punch": None, "mask_cuts": True, "mask_step": 1.06},
    "sfx": {"enabled": True, "volume": 1.0, "max_per_10s": 3, "whoosh": True, "ding": True, "pop": True},
    "audio": {"loudness": -14.0, "true_peak": -1.5},
    "video": {"crf": 18, "preset": "fast"},
}


def merged_settings(custom: dict | None) -> dict:
    out = copy.deepcopy(DEFAULTS)
    for key, value in (custom or {}).items():
        if isinstance(value, dict) and isinstance(out.get(key), dict):
            out[key].update(value)
        else:
            out[key] = value
    return out


def build(name: str, probe, words: list, transcript_meta: dict, script_raw: str | None = None, settings: dict | None = None) -> dict:
    settings = merged_settings(settings)
    words = copy.deepcopy(words)
    stats = {
        "filler": analyze.mark_fillers(words),
        "stutter": analyze.mark_stutters(words),
        "ng": analyze.mark_restarts(words, long_pause=float(settings["ng_pause"])),
    }
    script, keywords = (None, [])
    if script_raw and script_raw.strip():
        script, keywords = analyze.parse_script(script_raw)
        stats.update(analyze.align_script(words, script))
    spans = analyze.split_sentences(words)
    kept_spans = [k for k, (a, b) in enumerate(spans) if any(not words[i].get("cut") for i in range(a, b + 1))]
    sentences = []
    for k, (a, b) in enumerate(spans):
        entry = {"id": k + 1, "from": a, "to": b, "role": "explain", "zoom": "auto", "sfx": "auto", "captions": "auto"}
        if k in kept_spans:
            text = analyze.sentence_text(words, a, b)
            entry["role"] = analyze.guess_role(text, kept_spans.index(k), len(kept_spans))
            entry["captions"] = captions.auto_captions(words, a, b, int(settings["caption"]["max_chars"]), keywords, script)
        sentences.append(entry)
    return {
        "version": VERSION,
        "name": name,
        "source": probe.to_dict(),
        "transcript": transcript_meta,
        "script": script,
        "keywords": keywords,
        "settings": settings,
        "words": words,
        "sentences": sentences,
        "keep": [],
        "drop": [],
        "stats": stats,
    }


# ---------- 계산 ----------
@dataclass
class Resolved:
    settings: dict
    words: list
    segments: list
    timeline: timeline.Timeline
    cuts: list
    sentences: list  # 남은 문장 (완성본 시간, 줌 종류 포함)
    captions: list
    bounds: list
    zoom_events: list
    sfx_events: list
    duration: float
    notes: list = field(default_factory=list)


def _cap_times(words: list, tl, cap: dict):
    kept = [i for i in range(cap["from"], cap["to"] + 1) if not words[i].get("cut")]
    if not kept:
        return None
    return tl.to_out(words[kept[0]]["s"]), tl.to_out(words[kept[-1]]["e"])


def resolve(plan: dict, env) -> Resolved:
    settings = merged_settings(plan.get("settings"))
    words = copy.deepcopy(plan["words"])
    source_duration = float(plan["source"]["duration"])
    segs = timeline.build(words, env, settings, source_duration, plan.get("keep") or [], plan.get("drop") or [])
    if not segs:
        raise EditorError("남길 말이 하나도 없습니다. plan.json의 cut 표시나 drop 구간을 확인하세요.")
    tl = timeline.Timeline(segs)
    script = plan.get("script")
    keywords = plan.get("keywords") or []
    max_chars = int(settings["caption"]["max_chars"])

    sentences, caps = [], []
    for s in plan["sentences"]:
        kept = [i for i in range(s["from"], s["to"] + 1) if not words[i].get("cut")]
        if not kept:
            continue
        info = {
            "id": s["id"],
            "role": s.get("role", "explain"),
            "zoom": s.get("zoom", "auto"),
            "sfx": s.get("sfx", "auto"),
            "start": tl.to_out(words[kept[0]]["s"]),
            "end": tl.to_out(words[kept[-1]]["e"]),
            "src": [words[kept[0]]["s"], words[kept[-1]]["e"]],
            "text": analyze.sentence_text(words, s["from"], s["to"]),
        }
        chunks = s.get("captions")
        if chunks == "auto" or chunks is None:
            chunks = captions.auto_captions(words, s["from"], s["to"], max_chars, keywords, script)
        for cap in chunks:
            times = _cap_times(words, tl, cap)
            text = (cap.get("text") or "").strip()
            if times is None or not text:
                continue
            hl = cap.get("hl")
            if hl is None or hl == "auto":
                hl = captions.find_highlights(text, keywords)
            caps.append({"sentence": s["id"], "text": text, "hl": [h for h in hl if h and h in text], "t0": times[0], "t1": times[1]})
        sentences.append(info)

    caps.sort(key=lambda c: c["t0"])
    for i, cap in enumerate(caps):  # 다음 자막이 뜰 때까지 유지 (깜빡임 방지)
        nxt = caps[i + 1]["t0"] if i + 1 < len(caps) else None
        end = cap["t1"] + 0.3
        if nxt is not None and nxt - cap["t1"] < 0.6:
            end = nxt
        if nxt is not None:
            end = min(end, nxt)
        cap["show"] = [round(cap["t0"], 3), round(min(tl.duration, max(end, cap["t0"] + 0.2)), 3)]
    if caps and caps[0]["t0"] < 0.6:  # 첫 화면부터 자막이 보이게 (피드에서 멈추게 하는 첫 프레임)
        caps[0]["show"][0] = 0.0

    kinds = zoom.resolve_types(sentences, settings["zoom"])
    for info, kind in zip(sentences, kinds):
        info["kind"] = kind
    bounds = zoom.boundaries(sentences, tl.joins(), tl.duration)
    for info, b in zip(sentences, bounds):
        info["bound"] = b

    events = _sfx_events(sentences, caps, bounds, words, tl, settings["sfx"])
    return Resolved(
        settings=settings,
        words=words,
        segments=segs,
        timeline=tl,
        cuts=tl.cuts(words, source_duration),
        sentences=sentences,
        captions=caps,
        bounds=bounds,
        zoom_events=[],
        sfx_events=events,
        duration=tl.duration,
    )


def _sfx_events(sentences: list, caps: list, bounds: list, words: list, tl, cfg: dict) -> list:
    if not cfg.get("enabled", True):
        return []
    events = []
    for k, s in enumerate(sentences):
        b = bounds[k]
        if isinstance(s["sfx"], list):  # 직접 지정
            for item in s["sfx"]:
                kind = item.get("type")
                if kind not in sfx_mod.KINDS:
                    continue
                at = item.get("at", "start")
                t = b if at == "start" else tl.to_out(words[int(at)]["s"])
                events.append({"type": kind, "t": round(t, 3), "sentence": s["id"], "why": "직접 지정", "fixed": True})
            continue
        if s["sfx"] in ("none", "off"):
            continue
        if s["kind"] == "punch" and b > 0.3 and cfg.get("whoosh", True):
            events.append({"type": "whoosh", "t": round(b, 3), "sentence": s["id"], "why": "펀치 줌"})
        elif s["role"] in ("twist", "conclusion", "cta") and b > 0.3 and cfg.get("pop", True):
            events.append({"type": "pop", "t": round(b, 3), "sentence": s["id"], "why": "전환"})
        if cfg.get("ding", True):
            for cap in caps:  # 첫 0.5초는 비운다 (첫마디를 가리지 않게)
                if cap["sentence"] == s["id"] and cap["hl"] and cap["t0"] >= 0.5:
                    events.append({"type": "ding", "t": round(cap["t0"], 3), "sentence": s["id"], "why": f"'{cap['hl'][0]}'"})
                    break
    return sfx_mod.thin(events, int(cfg.get("max_per_10s", 3)))


# ---------- 고치기 ----------
SETTING_ALIASES = {
    "자막크기": "caption.size",
    "강조색": "caption.highlight",
    "자막색": "caption.color",
    "자막위치": "caption.bottom",
    "문장사이": "gap",
    "펀치줌": "zoom.punch",
    "펀치횟수": "zoom.max_punch",
    "효과음볼륨": "sfx.volume",
    "제목": "title.text",
}


def _coerce(old, value: str):
    text = str(value).strip()
    if text.lower() in ("none", "null", "없음", "auto"):
        return None
    if isinstance(old, bool):
        return text.lower() in ("1", "true", "yes", "on", "켜기", "예")
    if isinstance(old, (int, float)) or old is None:
        pct = re.fullmatch(r"(-?\d+(?:\.\d+)?)%", text)
        if pct:
            return float(pct.group(1)) / 100
        try:
            number = float(text)
            if number.is_integer() and (old is None or (isinstance(old, int) and not isinstance(old, bool))):
                return int(number)
            return number
        except ValueError:
            if old is None:
                return text
            raise EditorError(f"숫자가 필요합니다: {value}")
    return text.replace("\\n", "\n")


def set_value(plan: dict, key: str, value: str) -> tuple:
    key = SETTING_ALIASES.get(key, key)
    settings = merged_settings(plan.get("settings"))
    node = settings
    parts = key.split(".")
    for part in parts[:-1]:
        if not isinstance(node.get(part), dict):
            raise EditorError(f"없는 설정입니다: {key}")
        node = node[part]
    leaf = parts[-1]
    if leaf not in node:
        raise EditorError(f"없는 설정입니다: {key} (가능: {', '.join(_keys(DEFAULTS))})")
    old = node[leaf]
    new = _coerce(old, value)
    if key.endswith(("color", "highlight", "outline")) and new:
        from .graphics import color

        color(new)  # 형식 확인
    node[leaf] = new
    plan["settings"] = settings
    return old, new


def _keys(tree: dict, prefix: str = "") -> list:
    out = []
    for k, v in tree.items():
        if isinstance(v, dict):
            out.extend(_keys(v, prefix + k + "."))
        else:
            out.append(prefix + k)
    return out


def fix_text(plan: dict, old: str, new: str) -> int:
    """오타 고치기: 단어·자막·대본·키워드·제목에서 모두 바꾼다."""
    count = 0
    for w in plan["words"]:
        if old in w["w"]:
            w["w"] = w["w"].replace(old, new)
            count += 1
    for s in plan["sentences"]:
        if isinstance(s.get("captions"), list):
            for cap in s["captions"]:
                if old in cap.get("text", ""):
                    cap["text"] = cap["text"].replace(old, new)
                    cap["hl"] = [h.replace(old, new) for h in cap.get("hl") or []]
                    count += 1
    if plan.get("script") and old in plan["script"]:
        # 대본 위치(sp)가 어긋나지 않도록 대본은 길이가 같을 때만 바꾸고, 아니면 자막 쪽 수정으로 충분하다
        if len(old) == len(new):
            plan["script"] = plan["script"].replace(old, new)
    plan["keywords"] = [k.replace(old, new) for k in plan.get("keywords") or []]
    title = (plan.get("settings") or {}).get("title") or {}
    if old in (title.get("text") or ""):
        title["text"] = title["text"].replace(old, new)
        count += 1
    return count


def sentence(plan: dict, ref: str) -> dict:
    m = re.fullmatch(r"[Ss]?(\d+)", ref.strip())
    if not m:
        raise EditorError(f"문장 번호 형식: S3 또는 3 (받은 값: {ref})")
    sid = int(m.group(1))
    for s in plan["sentences"]:
        if s["id"] == sid:
            return s
    raise EditorError(f"문장 S{sid}이(가) 없습니다.")


def restore(plan: dict, res: Resolved, target: str) -> dict:
    """잘린 부분 되살리기. target: 'C3'(컷 번호), '0:12'(완성본 시간), 'src:0:12'(원본 시간)."""
    target = target.strip()
    cut = None
    m = re.fullmatch(r"[Cc](\d+)", target)
    if m:
        k = int(m.group(1)) - 1
        if not 0 <= k < len(res.cuts):
            raise EditorError(f"컷 C{k + 1}이(가) 없습니다 (C1~C{len(res.cuts)}).")
        cut = res.cuts[k]
    elif target.lower().startswith(("src:", "원본:", "원본")):
        t = parse_time(re.sub(r"^(src:|원본:|원본)", "", target, flags=re.I))
        inside = [c for c in res.cuts if c["src"][0] - 0.05 <= t <= c["src"][1] + 0.05]
        cut = inside[0] if inside else min(res.cuts, key=lambda c: min(abs(c["src"][0] - t), abs(c["src"][1] - t)), default=None)
    else:
        t = parse_time(target)
        cut = min(res.cuts, key=lambda c: abs(c["out"] - t), default=None)
        if cut is not None and abs(cut["out"] - t) > 2.0:
            raise EditorError(f"{target} 근처(±2초)에 잘린 곳이 없습니다. 보고서의 컷 목록에서 번호(C3)로 지정해 주세요.")
    if cut is None:
        raise EditorError("되살릴 컷을 찾지 못했습니다.")
    a, b = cut["src"]
    plan.setdefault("keep", []).append([round(a, 3), round(b, 3)])
    for w in plan["words"]:
        if w.get("cut") and w["s"] >= a - 0.02 and w["e"] <= b + 0.02:
            w.pop("cut", None)
    for s in plan["sentences"]:  # 되살린 문장에 자막이 없으면 자동으로 만들게 한다
        if any(a - 0.02 <= plan["words"][i]["s"] <= b + 0.02 for i in range(s["from"], s["to"] + 1)) and not s.get("captions"):
            s["captions"] = "auto"
    return cut


def drop_range(plan: dict, res: Resolved, text: str) -> tuple:
    """'0:12-0:14'(완성본) 또는 'src:12.0-14.0'(원본) 구간을 지운다."""
    src = text.lower().startswith(("src:", "원본"))
    body = re.sub(r"^(src:|원본:|원본)", "", text.strip(), flags=re.I)
    if "-" not in body and "~" not in body:
        raise EditorError("구간 형식: 0:12-0:14 (완성본) 또는 src:12.0-14.0 (원본)")
    lo, hi = (parse_time(x) for x in re.split(r"[-~]", body, maxsplit=1))
    if not src:
        lo, hi = res.timeline.to_src(lo), res.timeline.to_src(hi)
    if hi <= lo:
        raise EditorError("구간의 끝이 시작보다 앞입니다.")
    plan.setdefault("drop", []).append([round(lo, 3), round(hi, 3)])
    return lo, hi
