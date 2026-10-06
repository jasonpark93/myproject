"""음성 인식 → 단어별 시간표.

- faster-whisper(내 컴퓨터에서 무료로 실행)로 단어 시간까지 뽑는다.
- 설치가 안 되면 Vrew·CapCut에서 뽑은 SRT 자막을 받아 단어 시간을 추정한다.
- 테스트·수동 보정용으로 words.json([{w, s, e}])도 받는다.
"""

from __future__ import annotations

import re
from pathlib import Path

import numpy as np

from .util import EditorError, read_json

# 군말을 받아 적도록 유도하는 프롬프트 (Whisper는 기본적으로 '음', '어'를 지워 버린다)
FILLER_PROMPT = "음, 어, 그러니까요. 음… 이게 진짜 중요한데요, 어, 그 다음에 아, 그리고요."
DEFAULT_MODEL = "large-v3-turbo"

_NORM = re.compile(r"[^0-9a-z가-힣]")


def norm(text: str) -> str:
    """비교용: 공백·문장부호 제거, 소문자."""
    return _NORM.sub("", str(text).lower())


def clean(words: list) -> list:
    """시간 순 정렬, 겹침·역전 수정, 빈 단어 제거."""
    out = []
    for w in sorted(words, key=lambda w: (float(w["s"]), float(w["e"]))):
        text = str(w["w"]).strip()
        if not text:
            continue
        s, e = float(w["s"]), float(w["e"])
        if out and s < out[-1]["e"]:
            if s < out[-1]["s"] + 0.02:  # 거의 같은 시작 → 앞 단어를 줄인다
                s = out[-1]["s"] + 0.02
            out[-1]["e"] = max(out[-1]["s"] + 0.02, min(out[-1]["e"], s))
        if e <= s:
            e = s + 0.05
        item = {"w": text, "s": round(s, 3), "e": round(e, 3)}
        if "p" in w:
            item["p"] = round(float(w["p"]), 3)
        out.append(item)
    return out


def load_words(path: Path) -> list:
    data = read_json(path)
    if isinstance(data, dict):
        data = data.get("words") or []
    words = []
    for item in data:
        text = item.get("w", item.get("word", item.get("text")))
        start = item.get("s", item.get("start"))
        end = item.get("e", item.get("end"))
        if text is None or start is None or end is None:
            raise EditorError(f"words 파일 형식 오류: {item}")
        words.append({"w": text, "s": start, "e": end})
    return clean(words)


_SRT_TIME = re.compile(r"(\d+):(\d+):(\d+)[,.](\d+)\s*-->\s*(\d+):(\d+):(\d+)[,.](\d+)")


def _sec(h, m, s, ms) -> float:
    return int(h) * 3600 + int(m) * 60 + int(s) + int(ms.ljust(3, "0")[:3]) / 1000


def parse_srt(text: str) -> list:
    """[(시작, 끝, 문장)] — SRT와 VTT 모두."""
    cues = []
    blocks = re.split(r"\n\s*\n", text.replace("\r\n", "\n").replace("﻿", ""))
    for block in blocks:
        lines = [ln.strip() for ln in block.strip().split("\n") if ln.strip()]
        for i, line in enumerate(lines):
            m = _SRT_TIME.search(line)
            if m:
                body = " ".join(lines[i + 1 :])
                body = re.sub(r"<[^>]+>", "", body).strip()
                if body:
                    cues.append((_sec(*m.groups()[:4]), _sec(*m.groups()[4:]), body))
                break
    return cues


def words_from_srt(path: Path, env=None) -> list:
    """자막 한 줄(큐)의 시간을 글자 수 비율로 단어에 나눠 준다. 소리 구간이 있으면 그 안에서만 나눈다."""
    cues = parse_srt(Path(path).read_text(encoding="utf-8", errors="replace"))
    if not cues:
        raise EditorError(f"SRT에서 자막을 찾지 못했습니다: {path}")
    words = []
    for start, end, body in cues:
        tokens = body.split()
        if not tokens or end <= start:
            continue
        spans = [(max(s, start), min(e, end)) for s, e in (env.spans(start, end) if env is not None else [])]
        spans = [(s, e) for s, e in spans if e - s > 0.05] or [(start, end)]
        total = sum(e - s for s, e in spans)
        weights = np.array([len(norm(t)) + 1 for t in tokens], float)
        bounds = np.concatenate(([0], np.cumsum(weights) / weights.sum())) * total

        def at(offset: float) -> float:  # 말소리 구간을 이어 붙인 축의 위치 → 실제 시간
            for s, e in spans:
                if offset <= e - s + 1e-9:
                    return s + offset
                offset -= e - s
            return spans[-1][1]

        for tok, a, b in zip(tokens, bounds[:-1], bounds[1:]):
            words.append({"w": tok, "s": at(a), "e": max(at(a) + 0.04, at(b) - 0.02)})
    return clean(words)


def whisper_available() -> bool:
    try:
        import faster_whisper  # noqa: F401
    except ImportError:
        return False
    return True


def _device() -> tuple[str, str]:
    try:
        import ctranslate2

        if ctranslate2.get_cuda_device_count() > 0:
            return "cuda", "float16"
    except Exception:
        pass
    return "cpu", "int8"


def transcribe(audio_16k: np.ndarray, model_name: str = DEFAULT_MODEL, hotwords: str | None = None, log=print) -> tuple[list, dict]:
    """faster-whisper로 단어 시간표를 만든다. audio_16k: 16kHz 모노 float32."""
    try:
        from faster_whisper import WhisperModel
    except ImportError as exc:
        raise EditorError(
            "음성 인식(faster-whisper)이 설치되어 있지 않습니다. 'python3 -m pip install -r reels/requirements.txt'로 설치하거나, "
            "Vrew·CapCut에서 자막(SRT)을 내보내 --srt 로 넘겨 주세요."
        ) from exc
    device, compute = _device()
    log(f"음성 인식 모델 불러오는 중: {model_name} ({device}) — 처음 한 번은 내려받느라 몇 분 걸립니다")
    model = WhisperModel(model_name, device=device, compute_type=compute)
    segments, info = model.transcribe(
        audio_16k,
        language="ko",
        beam_size=5,
        word_timestamps=True,
        vad_filter=True,
        vad_parameters={"min_silence_duration_ms": 400, "speech_pad_ms": 200},
        condition_on_previous_text=False,  # 같은 문장을 다시 말한 NG 테이크를 건너뛰지 않도록
        initial_prompt=FILLER_PROMPT,
        hotwords=hotwords or None,
    )
    words = []
    for seg in segments:
        for w in seg.words or []:
            if w.word.strip():
                words.append({"w": w.word.strip(), "s": float(w.start), "e": float(w.end), "p": float(w.probability)})
    meta = {"engine": f"faster-whisper {model_name}", "device": device, "language": getattr(info, "language", "ko")}
    return clean(words), meta
