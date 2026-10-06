"""에피소드(영상 1편) 파일 형식과 검증.

episodes/<id>.json 하나가 영상 하나다. script 안의 내용만 사람이 고치면 되고,
나머지(id, series, publish_at, meta)는 파이프라인이 채운다.
"""

from __future__ import annotations

import hashlib
import json
import re
from datetime import datetime
from pathlib import Path

from . import RENDER_VERSION, ROOT, textutil

EPISODES = ROOT / "episodes"
VISUAL_TYPES = ("title", "number", "list", "versus", "check", "quote")
CHARS_PER_SECOND = 7.0  # 한국어 TTS 기본 속도에서 1초에 읽는 글자 수(공백 제외) 근사치
SCENE_GAP = 0.25  # 장면마다 끝에 두는 여백(초)
MAX_SECONDS = 58


def load(path: Path | str) -> dict:
    with open(path, encoding="utf-8") as f:
        return json.load(f)


def save(path: Path | str, episode: dict) -> None:
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(episode, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")


def all_episodes(directory: Path = EPISODES) -> list[tuple[Path, dict]]:
    return [(p, load(p)) for p in sorted(directory.glob("*.json"))]


def make_id(publish_at: str, series: str, slug: str) -> str:
    day = publish_at[:10]
    clean = re.sub(r"[^a-z0-9-]+", "-", slug.lower()).strip("-")[:40] or "ep"
    return f"{day}-{series}-{clean}"


def content_hash(episode: dict, cfg: dict) -> str:
    """영상 결과에 영향을 주는 값만 묶어 해시한다. 대본·목소리·색상·렌더러가 바뀌면 달라진다."""
    payload = {
        "format": episode["format"],
        "script": episode["script"],
        "series_label": cfg["series"].get(episode["series"], {}).get("label", ""),
        "voice": cfg["voice"],
        "brand": cfg["brand"],
        "channel": cfg["channel"]["name"],
        "broll": cfg["broll"],
        "render": RENDER_VERSION,
    }
    raw = json.dumps(payload, ensure_ascii=False, sort_keys=True).encode("utf-8")
    return hashlib.sha256(raw).hexdigest()[:16]


def publish_datetime(episode: dict) -> datetime:
    return datetime.fromisoformat(episode["publish_at"])


def estimate_seconds(script: dict, fmt: str, speed: float = 1.0) -> float:
    if fmt == "list":
        from .render import list_durations

        return sum(list_durations(script))
    total = 0.0
    for scene in script["scenes"]:
        total += textutil.spoken_length(scene["narration"]) / (CHARS_PER_SECOND * speed) + SCENE_GAP
    return round(total, 1)


def validate(script: dict, fmt: str, *, speed: float = 1.0) -> tuple[list[str], list[str]]:
    """(오류, 경고)를 돌려준다. 오류가 있으면 렌더하지 않는다."""
    errors: list[str] = []
    warnings: list[str] = []
    title = str(script.get("title", "")).strip()
    if not title:
        errors.append("title(영상 제목)이 비어 있습니다.")
    elif len(title) > 70:
        errors.append(f"제목이 너무 깁니다({len(title)}자). 70자 이하로 줄이세요.")
    elif len(title) > 45:
        warnings.append(f"제목이 깁니다({len(title)}자). 모바일에서는 앞 40자 정도만 보입니다.")

    if fmt == "narrated":
        scenes = script.get("scenes") or []
        if not 3 <= len(scenes) <= 10:
            errors.append(f"장면 수는 3~10개여야 합니다(현재 {len(scenes)}개).")
        for i, scene in enumerate(scenes, start=1):
            narration = str(scene.get("narration", "")).strip()
            if not narration:
                errors.append(f"{i}번 장면의 내레이션이 비어 있습니다.")
            elif len(textutil.strip_marks(narration)) > 120:
                warnings.append(f"{i}번 장면 내레이션이 깁니다. 두 장면으로 나누면 보기 편합니다.")
            if narration.count("[[") != narration.count("]]"):
                errors.append(f"{i}번 장면의 [[강조]] 괄호 짝이 맞지 않습니다.")
            headline = str(scene.get("headline", "")).strip()
            if len(headline) > 22:
                errors.append(f"{i}번 장면 headline이 너무 깁니다({len(headline)}자, 22자 이하).")
            visual = scene.get("visual") or {}
            kind = visual.get("type", "title")
            if kind not in VISUAL_TYPES:
                errors.append(f"{i}번 장면 visual.type이 올바르지 않습니다: {kind}")
            items = visual.get("items") or []
            if kind in ("list", "check") and not 1 <= len(items) <= 5:
                errors.append(f"{i}번 장면 {kind}는 항목이 1~5개여야 합니다.")
            if kind == "versus" and len(items) != 2:
                errors.append(f"{i}번 장면 versus는 항목이 정확히 2개여야 합니다.")
            if kind == "number" and not str(visual.get("value", "")).strip():
                errors.append(f"{i}번 장면 number에는 value(큰 숫자)가 필요합니다.")
        if scenes and not errors:
            seconds = estimate_seconds(script, fmt, speed)
            if seconds > MAX_SECONDS:
                errors.append(f"예상 길이 {seconds}초 — {MAX_SECONDS}초를 넘지 않게 줄이세요.")
            elif seconds < 15:
                warnings.append(f"예상 길이 {seconds}초 — 너무 짧습니다.")
    elif fmt == "list":
        items = script.get("items") or []
        if not 3 <= len(items) <= 10:
            errors.append(f"리스트 항목은 3~10개여야 합니다(현재 {len(items)}개).")
        for i, item in enumerate(items, start=1):
            text = str(item.get("text", "")).strip()
            if not text:
                errors.append(f"{i}번 항목이 비어 있습니다.")
            elif len(text) > 26:
                errors.append(f"{i}번 항목이 너무 깁니다({len(text)}자, 26자 이하).")
            if len(str(item.get("detail", ""))) > 40:
                warnings.append(f"{i}번 항목 설명이 깁니다(40자 이하 권장).")
        if not str(script.get("list_title", "")).strip():
            errors.append("list_title(첫 화면 큰 제목)이 비어 있습니다.")
    else:
        errors.append(f"알 수 없는 형식: {fmt}")

    tags = script.get("hashtags") or []
    if any(not str(t).startswith("#") for t in tags):
        errors.append("hashtags는 모두 #으로 시작해야 합니다.")
    if len(tags) > 8:
        warnings.append("해시태그가 많습니다(8개 이하 권장).")
    for source in script.get("sources") or []:
        if not str(source.get("url", "")).startswith(("https://", "http://")):
            warnings.append(f"출처 URL 형식 확인: {source}")
    return errors, warnings


def description(episode: dict, cfg: dict) -> str:
    """유튜브 설명란. 대본의 description + 출처 + 링크 + 해시태그."""
    script = episode["script"]
    parts = [str(script.get("description", "")).strip()]
    sources = script.get("sources") or []
    if sources:
        parts.append("📚 참고 자료\n" + "\n".join(f"- {s.get('title', '').strip()} {s.get('url', '').strip()}".rstrip() for s in sources))
    links = cfg["channel"].get("links") or []
    if links:
        parts.append("\n".join(f"🔗 {link['label']}: {link['url']}" for link in links))
    if cfg["broll"] == "pexels" and cfg["channel"].get("pexels_credit", True) and episode["format"] == "narrated":
        parts.append("배경 영상: Pexels")
    parts.append("※ 일반적인 정보 제공용 콘텐츠이며, 개인 상황에 따라 다를 수 있습니다. 중요한 결정 전에는 공식 기관 안내를 확인하세요.")
    tags = " ".join(script.get("hashtags") or [])
    if tags:
        parts.append(tags)
    return "\n\n".join(p for p in parts if p)
