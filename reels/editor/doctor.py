"""설치 상태 점검: 빠진 것과 설치 명령을 알려 준다."""

from __future__ import annotations

import platform
import shutil
import sys

from . import ROOT


def doctor(log=print) -> int:
    mac = sys.platform == "darwin"
    win = sys.platform.startswith("win")
    pip = "python -m pip" if win else "python3 -m pip"
    problems = 0

    def ok(msg):
        log(f"  ✓ {msg}")

    def bad(msg, fix):
        nonlocal problems
        problems += 1
        log(f"  ✗ {msg}\n      → {fix}")

    log(f"점검: {platform.system()} {platform.machine()}, Python {platform.python_version()}")
    if sys.version_info < (3, 9):
        bad("Python 3.9 이상이 필요합니다", "https://www.python.org/downloads/ 에서 최신 Python 설치")
    else:
        ok("Python")

    if shutil.which("ffmpeg") and shutil.which("ffprobe"):
        from .media import has_encoder, has_filter

        ok("ffmpeg")
        if not has_encoder("libx264"):
            bad("ffmpeg에 H.264 인코더가 없습니다", "brew reinstall ffmpeg (Mac) / winget install Gyan.FFmpeg (Windows)")
        if not has_filter("zscale"):
            log("  · 참고: ffmpeg에 HDR 변환(zscale)이 없습니다. 아이폰은 'HDR 비디오'를 끄고 찍으세요.")
    else:
        fix = "brew install ffmpeg" if mac else "winget install Gyan.FFmpeg (설치 후 터미널 다시 열기)" if win else "sudo apt-get install ffmpeg"
        bad("ffmpeg가 없습니다", fix)

    for module, name in (("numpy", "numpy"), ("PIL", "Pillow"), ("cv2", "opencv-python-headless")):
        try:
            __import__(module)
            ok(name)
        except ImportError:
            bad(f"{name}가 없습니다", f"{pip} install -r reels/requirements.txt")
    try:
        import cv2

        if not hasattr(cv2, "FaceDetectorYN"):
            bad("OpenCV가 오래돼 얼굴 인식을 못 씁니다", f"{pip} install -U opencv-python-headless")
    except ImportError:
        pass
    try:
        import faster_whisper  # noqa: F401

        ok("음성 인식(faster-whisper) — 첫 실행 때 모델(약 1.6GB)을 한 번 내려받습니다")
    except ImportError:
        bad("음성 인식(faster-whisper)이 없습니다", f"{pip} install -r reels/requirements.txt  (안 되면 Vrew·CapCut 자막 SRT로 대신 가능)")

    font = ROOT / "fonts" / "Pretendard-ExtraBold.otf"
    if font.exists():
        ok("자막 글꼴(Pretendard)")
    else:
        bad("자막 글꼴이 없습니다", "저장소를 다시 받거나 reels/fonts/ 에 굵은 한글 글꼴(.otf/.ttf)을 넣으세요")
    log("")
    log("모두 준비됐습니다." if problems == 0 else f"고칠 것 {problems}개 — 위 명령을 실행한 뒤 다시 점검하세요.")
    return 0 if problems == 0 else 1
