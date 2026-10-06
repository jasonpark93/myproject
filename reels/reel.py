#!/usr/bin/env python3
"""릴스 편집기 실행 파일. 어느 폴더에서든: python reels/reel.py edit 영상.mp4"""

import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

from editor.cli import main  # noqa: E402

if __name__ == "__main__":
    sys.exit(main())
