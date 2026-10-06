"""말하는 영상(토킹헤드) 원본 → 릴스/쇼츠 자동 편집기.

흐름: analyze(음성 인식·컷·자막·줌·효과음 계획 → plan.json) → render(계획대로 렌더 → 완성본 + 보고서)
plan.json을 고치고 render를 다시 돌리면 바뀐 단계만 다시 만든다.
"""

from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent  # reels/
FPS = 30
W, H = 1080, 1920
SR = 48000

__all__ = ["ROOT", "FPS", "W", "H", "SR"]
