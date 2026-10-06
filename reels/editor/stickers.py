"""스티커: 마이크로소프트 Fluent 이모지 3D 그림(MIT 라이선스, 상업 이용 가능).

카드에 "sticker": "house" 처럼 쓰면 assets/stickers/house.png 를 쓰고, 없으면 처음 한 번 GitHub에서 내려받는다.
목록에 없는 이름도 Fluent 이모지 영어 이름("Money with wings")이면 받아 온다. 인터넷이 막혀 있으면 스티커만 빠진다.
"""

from __future__ import annotations

import urllib.parse
import urllib.request
from pathlib import Path

from . import ROOT

DIR = ROOT / "assets" / "stickers"
BASE = "https://raw.githubusercontent.com/microsoft/fluentui-emoji/main/assets"
# 짧은 이름 → Fluent 이모지 이름
NAMES = {
    "house": "House", "houses": "Houses", "apartment": "Office building", "building": "Office building",
    "money": "Money bag", "moneybag": "Money bag", "coin": "Coin", "cash": "Dollar banknote", "flying_money": "Money with wings",
    "down": "Chart decreasing", "chart_down": "Chart decreasing", "up": "Chart increasing", "chart_up": "Chart increasing",
    "hammer": "Hammer", "phone": "Mobile phone", "bank": "Bank", "calendar": "Calendar", "hourglass": "Hourglass not done",
    "clock": "Alarm clock", "bulb": "Light bulb", "idea": "Light bulb", "warning": "Warning", "stop": "Stop sign",
    "check": "Check mark button", "x": "Cross mark", "pin": "Pushpin", "memo": "Memo", "bookmark": "Bookmark", "key": "Key",
    "gift": "Wrapped gift", "think": "Thinking face", "scream": "Face screaming in fear", "sparkles": "Sparkles",
    "alert": "Red exclamation mark", "fire": "Fire", "party": "Party popper", "receipt": "Receipt", "lock": "Locked",
}
_failed = set()


def slug(name: str) -> str:
    return NAMES.get(name, name).lower().replace(" ", "_").replace("-", "_")


def path(name: str, log=None) -> Path | None:
    """스티커 PNG 경로 (없으면 내려받기 시도, 실패하면 None)."""
    if not name:
        return None
    s = slug(name)
    local = DIR / f"{s}.png"
    if local.exists():
        return local
    if s in _failed:
        return None
    folder = NAMES.get(name, name)
    folder = folder[:1].upper() + folder[1:].replace("_", " ")
    q = urllib.parse.quote(folder)
    for url in (f"{BASE}/{q}/3D/{s}_3d.png", f"{BASE}/{q}/Default/3D/{s}_3d_default.png"):
        try:
            with urllib.request.urlopen(url, timeout=15) as r:
                data = r.read()
            if data[:8] != b"\x89PNG\r\n\x1a\n":
                continue
            DIR.mkdir(parents=True, exist_ok=True)
            local.write_bytes(data)
            if log:
                log(f"스티커 내려받음: {s}")
            return local
        except Exception:
            continue
    _failed.add(s)
    if log:
        log(f"스티커 '{name}'을 찾지 못해 빼고 만듭니다.")
    return None


def available() -> list:
    return sorted(NAMES)
