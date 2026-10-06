import shutil
import subprocess
import sys
from pathlib import Path

import numpy as np
import pytest

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

from editor import SR  # noqa: E402
from editor.transcript import clean  # noqa: E402

HAS_FFMPEG = shutil.which("ffmpeg") is not None and shutil.which("ffprobe") is not None
needs_ffmpeg = pytest.mark.skipif(not HAS_FFMPEG, reason="ffmpeg가 필요합니다")


def W(spec):
    """[(단어, 시작, 끝)] → 단어 시간표."""
    return clean([{"w": w, "s": s, "e": e} for w, s, e in spec])


def voice(words: list, duration: float, extra=()) -> np.ndarray:
    """단어 시간마다 목소리 비슷한 소리(배음 + 떨림)를 넣은 합성 음성. extra: 인식 안 된 소리 구간."""
    rng = np.random.default_rng(0)
    x = rng.standard_normal(int(duration * SR)).astype(np.float32) * 10 ** (-60 / 20)
    for s, e in [(w["s"], w["e"]) for w in words] + list(extra):
        a, b = int(s * SR), int(e * SR)
        t = np.arange(b - a) / SR
        f0 = 140 + 20 * np.sin(2 * np.pi * 3 * t)
        phase = 2 * np.pi * np.cumsum(f0) / SR
        tone = sum(np.sin(k * phase) / k for k in range(1, 12))
        env = np.minimum(1, np.minimum(t / 0.02, (t[-1] - t) / 0.02 + 1e-9)) if t.size else t
        x[a:b] += (0.25 * tone * env).astype(np.float32)
    return x


def make_video(path: Path, words: list, duration: float, size=(1080, 1920), extra=()) -> None:
    """얼굴 대신 움직이는 원을 그린 테스트 영상 + 합성 음성."""
    w, h = size
    audio = voice(words, duration, extra)
    wav = path.with_suffix(".f32")
    wav.write_bytes(audio.tobytes())
    n = int(duration * 30)
    proc = subprocess.Popen(
        ["ffmpeg", "-v", "error", "-y", "-f", "rawvideo", "-pix_fmt", "gray", "-s", f"{w}x{h}", "-r", "30", "-i", "-",
         "-f", "f32le", "-ar", str(SR), "-ac", "1", "-i", str(wav),
         "-c:v", "libx264", "-preset", "ultrafast", "-pix_fmt", "yuv420p", "-c:a", "aac", "-shortest", str(path)],
        stdin=subprocess.PIPE,
    )
    yy, xx = np.mgrid[0:h, 0:w]
    for i in range(n):
        cx, cy = w / 2 + 40 * np.sin(i / 15), h * 0.4
        frame = np.where((xx - cx) ** 2 + (yy - cy) ** 2 < (w * 0.2) ** 2, 200, 60).astype(np.uint8)
        frame[20:60, 20 + (i % 30) * 10 : 60 + (i % 30) * 10] = 255  # 프레임 번호 표시용 막대
        proc.stdin.write(frame.tobytes())
    proc.stdin.close()
    proc.wait()
    wav.unlink()


@pytest.fixture
def workdirs(tmp_path, monkeypatch):
    from editor import cli, render

    monkeypatch.setattr(cli, "WORK_DIR", tmp_path / "work")
    monkeypatch.setattr(cli, "STYLE", tmp_path / "style.json")
    monkeypatch.setattr(render, "OUT_DIR", tmp_path / "out")
    return tmp_path
