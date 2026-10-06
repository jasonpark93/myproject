"""배경 영상(B-roll). Pexels의 무료 세로 영상을 검색해 내려받는다 (상업적 이용 가능, 출처 표시 권장).

Pexels API 제한: 시간당 200회, 월 2만 회. 같은 영상은 cache/broll에 저장해 다시 받지 않는다.
"""

from __future__ import annotations

import hashlib
import json
import random
import urllib.error
import urllib.parse
import urllib.request
from pathlib import Path
from typing import Callable

SEARCH_URL = "https://api.pexels.com/videos/search"
USER_AGENT = "shorts-studio/1.0"


class BrollError(RuntimeError):
    pass


class Pexels:
    def __init__(self, api_key: str, cache_dir: Path, *, opener: Callable = urllib.request.urlopen):
        if not api_key:
            raise BrollError("PEXELS_API_KEY가 없습니다.")
        self._key = api_key
        self.dir = Path(cache_dir)
        self._open = opener

    def search(self, query: str) -> list[dict]:
        params = urllib.parse.urlencode({"query": query, "orientation": "portrait", "size": "medium", "per_page": 15})
        request = urllib.request.Request(f"{SEARCH_URL}?{params}", headers={"Authorization": self._key, "User-Agent": USER_AGENT})
        try:
            with self._open(request, timeout=30) as response:
                return json.loads(response.read()).get("videos", [])
        except urllib.error.HTTPError as e:
            raise BrollError(f"Pexels 검색 실패: HTTP {e.code}") from None
        except (urllib.error.URLError, TimeoutError, ConnectionError) as e:
            raise BrollError(f"Pexels 연결 실패: {getattr(e, 'reason', e)}") from None

    def find(self, query: str, *, seconds: float, seed: str, exclude: set[int]) -> tuple[int, Path] | None:
        """세로 HD 영상을 골라 내려받는다. 마땅한 게 없으면 None (그라디언트 배경으로 대체)."""
        candidates = []
        for video in self.search(query):
            if video.get("id") in exclude:
                continue
            best = pick_file(video.get("video_files") or [])
            if best:
                long_enough = float(video.get("duration") or 0) >= min(seconds, 8)
                candidates.append((not long_enough, video["id"], best))
        if not candidates:
            return None
        candidates.sort(key=lambda c: c[0])
        top = [c for c in candidates if c[0] == candidates[0][0]][:5]
        _, video_id, file = random.Random(f"{seed}:{query}").choice(top)
        path = self.dir / f"{video_id}-{file['id']}.mp4"
        if not path.exists():
            self.dir.mkdir(parents=True, exist_ok=True)
            request = urllib.request.Request(file["link"], headers={"User-Agent": USER_AGENT})
            try:
                with self._open(request, timeout=120) as response:
                    data = response.read()
            except (urllib.error.URLError, TimeoutError, ConnectionError):
                return None
            path.write_bytes(data)
        return video_id, path


def pick_file(files: list[dict]) -> dict | None:
    """세로(높이>너비) mp4 중 1920에 가장 가까운 해상도."""
    portrait = [
        f
        for f in files
        if f.get("file_type") == "video/mp4" and (f.get("height") or 0) > (f.get("width") or 0) and (f.get("height") or 0) >= 1280
    ]
    if not portrait:
        return None
    return min(portrait, key=lambda f: abs((f.get("height") or 0) - 1920))


def default_query(seed: str) -> str:
    """대본에 B-roll 검색어가 없을 때 쓰는 무난한 배경."""
    options = ["city night", "office work", "money", "apartment building", "calculator finance", "korea street"]
    index = int(hashlib.sha256(seed.encode("utf-8")).hexdigest(), 16) % len(options)
    return options[index]
