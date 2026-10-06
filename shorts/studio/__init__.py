"""AI 쇼츠 스튜디오: 대본(Claude) → 음성(TTS) → 영상(ffmpeg) → 업로드까지 이어지는 파이프라인."""

from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
RENDER_VERSION = "1"  # 렌더 결과가 바뀌는 수정을 하면 올린다 (기존 영상 재렌더 판단에 쓰임)
