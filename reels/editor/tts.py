"""미리보기용 컴퓨터 음성 (무료). 올릴 영상은 내 목소리 녹음을 권장한다.

쓸 수 있는 것부터 차례로 시도한다:
1. edge  — 마이크로소프트 신경망 음성(자연스러움, 인터넷 필요, `pip install edge-tts`)
2. say   — Mac 내장 음성 (설정에서 'Yuna(프리미엄)'을 받으면 더 자연스럽다)
3. sapi  — Windows 내장 음성
4. kss   — 오프라인 AI 음성 (처음 한 번 67MB 내려받음, `pip install sherpa-onnx`)
5. espeak — 아주 기계적인 음성 (최후의 수단)

라이선스 주의: Mac 음성과 kss(KSS 데이터)는 비상업용이라 수익 영상에는 쓰지 않는다. 미리보기 전용.
"""

from __future__ import annotations

import shutil
import sys
import tarfile
import tempfile
import urllib.request
from pathlib import Path

import numpy as np

from . import ROOT, SR
from .media import decode_audio, write_wav
from .util import EditorError, run

ORDER = ["edge", "say", "sapi", "kss", "espeak"]
LABEL = {
    "edge": "마이크로소프트 AI 음성",
    "say": "Mac 내장 음성",
    "sapi": "Windows 내장 음성",
    "kss": "오프라인 AI 음성",
    "espeak": "기계 음성(espeak)",
}
KSS_URL = "https://github.com/k2-fsa/sherpa-onnx/releases/download/tts-models/vits-mimic3-ko_KO-kss_low.tar.bz2"
MODEL_DIR = ROOT / "models" / "tts"
_working = {}
_kss_engine = None


def installed() -> list:
    out = []
    try:
        import edge_tts  # noqa: F401

        out.append("edge")
    except ImportError:
        pass
    if sys.platform == "darwin" and shutil.which("say"):
        out.append("say")
    if sys.platform.startswith("win") and shutil.which("powershell"):
        out.append("sapi")
    try:
        import sherpa_onnx  # noqa: F401

        out.append("kss")
    except ImportError:
        pass
    if shutil.which("espeak-ng"):
        out.append("espeak")
    return out


def available() -> list:
    return installed()


def describe(engine: str | None) -> str:
    return LABEL.get(engine or "", "컴퓨터 음성")


def _trim(x: np.ndarray, thr: float = 0.01) -> np.ndarray:
    idx = np.flatnonzero(np.abs(x) > thr)
    if idx.size == 0:
        return x
    a = max(0, idx[0] - int(0.01 * SR))
    b = min(x.size, idx[-1] + int(0.03 * SR))
    return x[a:b]


def _kss_dir(log=print) -> Path:
    d = MODEL_DIR / "vits-mimic3-ko_KO-kss_low"
    if (d / "ko_KO-kss_low.onnx").exists():
        return d
    MODEL_DIR.mkdir(parents=True, exist_ok=True)
    log("오프라인 AI 음성 모델 내려받는 중 (67MB, 처음 한 번만)…")
    archive = MODEL_DIR / "kss.tar.bz2"
    urllib.request.urlretrieve(KSS_URL, archive)
    with tarfile.open(archive) as tar:
        base = MODEL_DIR.resolve()
        for member in tar.getmembers():  # 압축 안의 경로가 폴더 밖으로 나가지 않는지 확인
            if not (base / member.name).resolve().is_relative_to(base) or member.issym() or member.islnk():
                raise EditorError("음성 모델 압축 파일이 이상합니다.")
        tar.extractall(base)
    archive.unlink()
    return d


def _kss(text: str, speed: float = 1.12) -> np.ndarray:
    global _kss_engine
    import sherpa_onnx

    if _kss_engine is None:
        d = _kss_dir()
        cfg = sherpa_onnx.OfflineTtsConfig(
            model=sherpa_onnx.OfflineTtsModelConfig(
                vits=sherpa_onnx.OfflineTtsVitsModelConfig(model=str(d / "ko_KO-kss_low.onnx"), tokens=str(d / "tokens.txt"), data_dir=str(d / "espeak-ng-data")),
                num_threads=2,
            )
        )
        _kss_engine = sherpa_onnx.OfflineTts(cfg)
    audio = _kss_engine.generate(text, sid=0, speed=speed)
    with tempfile.TemporaryDirectory() as tmp:
        path = Path(tmp) / "kss.wav"
        write_wav(path, np.asarray(audio.samples, np.float32), audio.sample_rate)
        return decode_audio(path)


def _synth(engine: str, text: str, voice: str | None) -> np.ndarray:
    if engine == "kss":
        return _kss(text)
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
            run([sys.executable, "-m", "edge_tts", "--voice", voice or "ko-KR-SunHiNeural", "--rate", "+10%", "-f", src, "--write-media", out])
        elif engine == "espeak":
            out = tmp / "out.wav"
            run(["espeak-ng", "-v", "ko", "-s", "185", "-w", out, "-f", src])
        else:
            raise EditorError(f"모르는 음성: {engine} (가능: {', '.join(ORDER)})")
        if not out.exists() or out.stat().st_size == 0:
            raise EditorError(f"{describe(engine)}이 소리를 만들지 못했습니다.")
        return decode_audio(out)


def pick(engine: str | None = None, voice: str | None = None, log=print) -> str:
    """실제로 소리가 나는 음성을 고른다 (인터넷이 막힌 곳에선 edge를 건너뜀)."""
    if engine:
        return engine
    for name in [e for e in ORDER if e in installed()]:
        if name in _working:
            if _working[name]:
                return name
            continue
        try:
            _synth(name, "테스트", voice)
            _working[name] = True
            return name
        except Exception:
            _working[name] = False
            log(f"{describe(name)}을 쓸 수 없어 다음 음성으로 넘어갑니다.")
    raise EditorError("이 컴퓨터에서 쓸 수 있는 음성 합성이 없습니다. 'pip install edge-tts' 후 다시 해 보거나, 녹음 파일을 --voice로 넘겨 주세요.")


def speak(text: str, engine: str | None = None, voice: str | None = None) -> np.ndarray:
    """한 문장을 읽어 48kHz 모노 float32로 돌려준다 (앞뒤 무음 제거)."""
    return _trim(_synth(pick(engine, voice), text, voice))
