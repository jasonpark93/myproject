"""config.json 로드와 값 검증."""

from __future__ import annotations

import json
import re
from pathlib import Path
from urllib.parse import urlsplit

from . import ROOT

DEFAULTS = {
    "site_name": "오늘의 청약",
    "tagline": "전국 아파트 청약 일정·분양가를 매일 자동으로 정리합니다",
    "base_url": "http://localhost:8000",
    "contact_email": "",
    "adsense_client": "",
    "adsense_slots": {},
    "ga4_measurement_id": "",
    "google_site_verification": "",
    "naver_site_verification": "",
    "telegram_channel_url": "",
    "kinds": ["apt", "remndr", "urbty"],
    "backfill_days": 365,
    "refresh_days": 45,
    "max_model_calls_per_run": 300,
}

# 템플릿의 <script> 안에 들어가는 값은 HTML 이스케이프만으로는 안전하지 않으므로 형식을 강제한다.
_PATTERNS = {
    "adsense_client": re.compile(r"ca-pub-\d{10,20}"),
    "ga4_measurement_id": re.compile(r"G-[A-Z0-9]{4,16}"),
    "google_site_verification": re.compile(r"[A-Za-z0-9_\-]{10,100}"),
    "naver_site_verification": re.compile(r"[A-Za-z0-9_\-]{10,100}"),
}
_SLOT = re.compile(r"\d{6,20}")


class ConfigError(ValueError):
    pass


def load(path: Path | None = None, *, base_url: str | None = None) -> dict:
    path = path or ROOT / "config.json"
    with open(path, encoding="utf-8") as f:
        cfg = {**DEFAULTS, **json.load(f)}
    if base_url:
        cfg["base_url"] = base_url
    return validate(cfg)


def validate(cfg: dict) -> dict:
    for key, pattern in _PATTERNS.items():
        value = (cfg.get(key) or "").strip()
        if value and not pattern.fullmatch(value):
            raise ConfigError(f"config.json의 {key} 형식이 올바르지 않습니다: {value!r}")
        cfg[key] = value

    slots = {k: str(v).strip() for k, v in (cfg.get("adsense_slots") or {}).items() if str(v).strip()}
    for name, slot in slots.items():
        if not _SLOT.fullmatch(slot):
            raise ConfigError(f"adsense_slots.{name} 값은 숫자 광고 단위 ID여야 합니다: {slot!r}")
    cfg["adsense_slots"] = slots

    base = cfg["base_url"].strip().rstrip("/")
    parts = urlsplit(base)
    if parts.scheme not in ("http", "https") or not parts.netloc:
        raise ConfigError(f"base_url은 http(s)://로 시작해야 합니다: {base!r}")
    cfg["base_url"] = base
    cfg["base_path"] = parts.path.rstrip("/")  # 예: GitHub Pages 프로젝트 사이트면 "/myproject"
    cfg["host"] = parts.netloc

    channel = (cfg.get("telegram_channel_url") or "").strip()
    if channel and not channel.startswith("https://t.me/"):
        raise ConfigError("telegram_channel_url은 https://t.me/ 로 시작해야 합니다.")
    cfg["telegram_channel_url"] = channel

    from .applyhome import KINDS

    unknown = [k for k in cfg["kinds"] if k not in KINDS]
    if unknown:
        raise ConfigError(f"알 수 없는 kinds 값: {unknown} (가능한 값: {list(KINDS)})")
    for key in ("backfill_days", "refresh_days", "max_model_calls_per_run"):
        cfg[key] = int(cfg[key])
    return cfg
