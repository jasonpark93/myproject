"""channel.json 로드와 검증."""

from __future__ import annotations

import copy
import json
import re
from pathlib import Path

from . import ROOT

WEEKDAYS = ("mon", "tue", "wed", "thu", "fri", "sat", "sun")
FORMATS = ("narrated", "list")
_HEX = re.compile(r"#[0-9A-Fa-f]{6}")
_TIME = re.compile(r"([01]\d|2[0-3]):[0-5]\d")

DEFAULTS = {
    "channel": {"name": "", "tagline": "", "audience": "", "tone": "", "links": [], "pexels_credit": True},
    "brand": {"primary": "#2F6BFF", "accent": "#FFD43B", "bg_top": "#0B1220", "bg_bottom": "#1A2C55", "text": "#FFFFFF"},
    "voice": {
        "provider": "google",
        "google_voice": "ko-KR-Chirp3-HD-Charon",
        "elevenlabs_voice_id": "",
        "elevenlabs_model": "eleven_multilingual_v2",
        "speed": 1.1,
    },
    "writer": {"model": "claude-opus-5-5", "effort": "medium", "research": True, "max_searches": 5},
    "schedule": {"publish_time": "19:00", "days": {}},
    "series": {},
    "review": "pr",
    "upload": "manual",
    "broll": "none",
    "youtube": {"category_id": "27", "made_for_kids": False, "contains_synthetic_media": False, "default_language": "ko"},
}


class ConfigError(ValueError):
    pass


def load(path: Path | None = None) -> dict:
    path = path or ROOT / "channel.json"
    with open(path, encoding="utf-8") as f:
        raw = json.load(f)
    return validate(_merge(copy.deepcopy(DEFAULTS), raw))


def _merge(base: dict, override: dict) -> dict:
    for key, value in override.items():
        if isinstance(value, dict) and isinstance(base.get(key), dict):
            base[key] = _merge(base[key], value)
        else:
            base[key] = value
    return base


def validate(cfg: dict) -> dict:
    if not cfg["channel"]["name"].strip():
        raise ConfigError("channel.name(채널 이름)이 비어 있습니다.")
    for key, value in cfg["brand"].items():
        if not _HEX.fullmatch(str(value)):
            raise ConfigError(f"brand.{key}는 #RRGGBB 형식이어야 합니다: {value!r}")
    voice = cfg["voice"]
    if voice["provider"] not in ("google", "elevenlabs", "silent"):
        raise ConfigError("voice.provider는 google, elevenlabs, silent 중 하나여야 합니다.")
    if voice["provider"] == "elevenlabs" and not voice["elevenlabs_voice_id"]:
        raise ConfigError("ElevenLabs를 쓰려면 voice.elevenlabs_voice_id가 필요합니다.")
    voice["speed"] = float(voice["speed"])
    if not 0.8 <= voice["speed"] <= 1.4:
        raise ConfigError("voice.speed는 0.8~1.4 사이로 설정하세요.")
    if cfg["writer"]["effort"] not in ("low", "medium", "high", "xhigh", "max"):
        raise ConfigError("writer.effort는 low, medium, high, xhigh, max 중 하나여야 합니다.")
    if not _TIME.fullmatch(cfg["schedule"]["publish_time"]):
        raise ConfigError("schedule.publish_time은 HH:MM 형식이어야 합니다.")

    series = cfg["series"]
    if not series:
        raise ConfigError("series에 시리즈를 하나 이상 정의하세요.")
    for name, spec in series.items():
        if spec.get("format") not in FORMATS:
            raise ConfigError(f"series.{name}.format은 {FORMATS} 중 하나여야 합니다.")
        spec.setdefault("label", name)
        spec.setdefault("target_seconds", 40)
        fallback = spec.get("fallback")
        if fallback and fallback not in series:
            raise ConfigError(f"series.{name}.fallback({fallback})이 정의되지 않은 시리즈입니다.")
    for day, name in cfg["schedule"]["days"].items():
        if day not in WEEKDAYS:
            raise ConfigError(f"schedule.days의 요일 키는 {WEEKDAYS} 중 하나여야 합니다: {day}")
        if name and name not in series:
            raise ConfigError(f"schedule.days.{day}의 시리즈({name})가 series에 없습니다.")

    if cfg["review"] not in ("pr", "auto"):
        raise ConfigError("review는 pr 또는 auto여야 합니다.")
    if cfg["upload"] not in ("manual", "api"):
        raise ConfigError("upload는 manual 또는 api여야 합니다.")
    if cfg["broll"] not in ("pexels", "none"):
        raise ConfigError("broll은 pexels 또는 none이어야 합니다.")
    for link in cfg["channel"]["links"]:
        if not str(link.get("url", "")).startswith("https://"):
            raise ConfigError(f"channel.links의 url은 https://로 시작해야 합니다: {link}")
    return cfg
