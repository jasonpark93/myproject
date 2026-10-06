"""미리보기용 컴퓨터 음성 (무료·오프라인). 올릴 영상은 내 목소리 녹음을 권장한다.

- Mac: say (기본 한국어 목소리 Yuna, 설정에서 고음질 버전을 받으면 더 자연스럽다)
- Windows: 기본 음성 합성(SAPI, 한국어 음성 Heami)
- Linux: espeak-ng
- (선택) edge-tts: 설치돼 있으면 자연스러운 한국어 목소리 — 마이크로소프트 온라인 낭독 서비스를 쓴다
"""

from __future__ import annotations

import shutil
import sys
import tempfile
from pathlib import Path

import numpy as np

from . import SR
from .media import decode_audio
from .util import EditorError, run


def available() -> list:
    out = []
    if sys.platform == "darwin" and shutil.which("say"):
        out.append("say")
    if sys.platform.startswith("win") and shutil.which("powershell"):
        out.append("sapi")
    try:
        import edge_tts  # noqa: F401

        out.append("edge")
    except ImportError:
        pass
    if shutil.which("espeak-ng"):
        out.append("espeak")
    return out


def _trim(x: np.ndarray, thr: float = 0.01) -> np.ndarray:
    idx = np.flatnonzero(np.abs(x) > thr)
    if idx.size == 0:
        return x
    a = max(0, idx[0] - int(0.01 * SR))
    b = min(x.size, idx[-1] + int(0.03 * SR))
    return x[a:b]


def speak(text: str, engine: str | None = None, voice: str | None = None) -> np.ndarray:
    """한 문장을 읽어 48kHz 모노 float32로 돌려준다 (앞뒤 무음 제거)."""
    engines = available()
    engine = engine or (engines[0] if engines else None)
    if engine is None:
        raise EditorError("이 컴퓨터에서 쓸 수 있는 음성 합성이 없습니다. 목소리를 녹음해서 --voice로 넘겨 주세요.")
    with tempfile.TemporaryDirectory() as tmp:
        tmp = Path(tmp)
        src = tmp / "in.txt"
        src.write_text(text, encoding="utf-8")
        if engine == "say":
            out = tmp / "out.aiff"
            run(["say", "-v", voice or "Yuna", "-r", "205", "-o", out, "-f", src])
        elif engine == "sapi":
            out = tmp / "out.wav"
            script = (
                "Add-Type -AssemblyName System.Speech;"
                "$s = New-Object System.Speech.Synthesis.SpeechSynthesizer;"
                "$v = $s.GetInstalledVoices() | Where-Object { $_.VoiceInfo.Culture.Name -eq 'ko-KR' } | Select-Object -First 1;"
                "if ($v) { $s.SelectVoice($v.VoiceInfo.Name) };"
                "$s.Rate = 1;"
                f"$s.SetOutputToWaveFile('{out}');"
                f"$s.Speak([IO.File]::ReadAllText('{src}', [Text.Encoding]::UTF8));"
                "$s.Dispose()"
            )
            run(["powershell", "-NoProfile", "-Command", script])
        elif engine == "edge":
            out = tmp / "out.mp3"
            run([sys.executable, "-m", "edge_tts", "--voice", voice or "ko-KR-SunHiNeural", "--rate", "+12%", "-f", src, "--write-media", out])
        elif engine == "espeak":
            out = tmp / "out.wav"
            run(["espeak-ng", "-v", "ko", "-s", "185", "-w", out, "-f", src])
        else:
            raise EditorError(f"모르는 음성 합성: {engine} (가능: {', '.join(engines) or '없음'})")
        x = decode_audio(out)
    return _trim(x)


def describe(engine: str | None) -> str:
    return {"say": "Mac 내장 음성", "sapi": "Windows 내장 음성", "edge": "edge-tts (온라인)", "espeak": "espeak-ng"}.get(engine or "", "컴퓨터 음성")


def _check() -> None:  # pragma: no cover - 수동 점검용
    print(available())
    if available():
        x = speak("안녕하세요. 미리보기 음성입니다.")
        print(x.size / SR, "초")


if __name__ == "__main__":  # pragma: no cover
    _check()
