"""data/notices.json 저장소. 한 번 수집한 공고는 지우지 않는다 (지난 공고 페이지도 검색 유입이 계속 생긴다)."""

from __future__ import annotations

import json
import os
from pathlib import Path

META_KEYS = ("first_seen", "updated_on", "models", "models_fetched_on")


def empty() -> dict:
    return {"updated_at": None, "notices": {}}


def load(path: Path | str) -> dict:
    path = Path(path)
    if not path.exists():
        return empty()
    with open(path, encoding="utf-8") as f:
        db = json.load(f)
    db.setdefault("updated_at", None)
    db.setdefault("notices", {})
    return db


def save(path: Path | str, db: dict) -> None:
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    data = {"updated_at": db.get("updated_at"), "notices": {k: db["notices"][k] for k in sorted(db["notices"])}}
    tmp = path.with_suffix(".tmp")
    with open(tmp, "w", encoding="utf-8") as f:
        json.dump(data, f, ensure_ascii=False, indent=1)
        f.write("\n")
    os.replace(tmp, path)


def _content(notice: dict) -> dict:
    return {k: v for k, v in notice.items() if k not in META_KEYS}


def merge(db: dict, notices: list[dict], today: str) -> list[str]:
    """수집한 공고를 합치고 새로 생긴 공고 id 목록을 돌려준다. 내용이 바뀌면 updated_on을 갱신한다."""
    new_ids = []
    for notice in notices:
        old = db["notices"].get(notice["id"])
        if old is None:
            db["notices"][notice["id"]] = {
                **notice,
                "first_seen": today,
                "updated_on": today,
                "models": [],
                "models_fetched_on": None,
            }
            new_ids.append(notice["id"])
            continue
        merged = {**old, **notice}
        if _content(old) != _content(merged):
            merged["updated_on"] = today
        db["notices"][notice["id"]] = merged
    return new_ids
