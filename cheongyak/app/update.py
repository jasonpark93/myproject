"""공공데이터 API에서 공고를 받아 data/notices.json을 갱신한다."""

from __future__ import annotations

from datetime import date, timedelta

from . import store
from .applyhome import KINDS, ApiError, Client
from .dates import now_kst
from .normalize import normalize_model, normalize_notice

MODEL_REFRESH_DAYS = 3  # 접수 마감 전 공고는 정정공고가 잦아서 주택형 정보를 다시 받는다
MODEL_FAILURES_PER_KIND = 3  # 같은 종류에서 연달아 실패하면 이번 실행에서는 건너뛴다


def run(cfg: dict, client: Client, db_path, today: date) -> dict:
    db = store.load(db_path)
    seed = not db["notices"]
    today_s = today.isoformat()
    since = (today - timedelta(days=cfg["backfill_days"] if seed else cfg["refresh_days"])).isoformat()
    summary = {"seed": seed, "since": since, "kinds": {}, "errors": [], "new": [], "models_fetched": 0}

    for kind in cfg["kinds"]:
        try:
            rows = _fetch_notices(client, KINDS[kind]["detail"], since)
        except ApiError as e:
            if e.auth:
                raise
            summary["errors"].append(f"{KINDS[kind]['label']} 공고 목록: {e}")
            continue
        notices = [n for n in (normalize_notice(kind, r) for r in rows) if n]
        if rows and not notices:
            summary["errors"].append(
                f"{KINDS[kind]['label']}: {len(rows)}건을 받았지만 단지명·관리번호 필드를 찾지 못했습니다. "
                "API 필드 이름이 바뀌었을 수 있으니 'python -m app inspect'로 확인하세요."
            )
        new_ids = store.merge(db, notices, today_s)
        summary["kinds"][kind] = {"fetched": len(rows), "valid": len(notices), "new": len(new_ids)}
        summary["new"].extend(new_ids)

    if not summary["kinds"]:
        raise ApiError("모든 공고 목록 호출이 실패했습니다: " + " / ".join(summary["errors"]))

    _fetch_models(cfg, client, db, today_s, summary)

    # 처음 실행(과거 공고 일괄 수집) 때는 알림을 보내지 않는다.
    summary["notify"] = [] if seed else [i for i in summary["new"] if is_current(db["notices"][i], today_s)]
    db["updated_at"] = now_kst().isoformat(timespec="seconds")
    store.save(db_path, db)
    summary["total"] = len(db["notices"])
    return summary


def _fetch_notices(client: Client, operation: str, since: str) -> list[dict]:
    try:
        return list(client.iter_records(operation, cond={"RCRIT_PBLANC_DE::GTE": since}))
    except ApiError as e:
        if e.status != 400:
            raise
    # 날짜 조건 검색을 지원하지 않는 오퍼레이션이면(HTTP 400) 전부 받아서 직접 거른다.
    rows = client.iter_records(operation, per_page=500)
    return [r for r in rows if str(r.get("RCRIT_PBLANC_DE") or "") >= since]


def _fetch_models(cfg: dict, client: Client, db: dict, today_s: str, summary: dict) -> None:
    pending = [n for n in db["notices"].values() if needs_models(n, today_s)]
    pending.sort(key=lambda n: n.get("announce_date") or "", reverse=True)
    pending.sort(key=lambda n: n.get("models_fetched_on") is not None)  # 한 번도 안 받은 공고 먼저

    budget = cfg["max_model_calls_per_run"]
    failures: dict[str, int] = {}
    for notice in pending[:budget]:
        kind = notice["kind"]
        if failures.get(kind, 0) >= MODEL_FAILURES_PER_KIND:
            continue
        try:
            rows = list(client.iter_records(KINDS[kind]["model"], cond={"HOUSE_MANAGE_NO::EQ": notice["house_manage_no"]}))
        except ApiError as e:
            if e.auth:
                raise
            failures[kind] = failures.get(kind, 0) + 1
            summary["errors"].append(f"{notice['name']} 주택형 정보: {e}")
            continue
        failures[kind] = 0
        if notice.get("pblanc_no"):
            rows = [r for r in rows if str(r.get("PBLANC_NO") or notice["pblanc_no"]) == notice["pblanc_no"]]
        models = [normalize_model(r) for r in rows]
        if models != notice.get("models"):
            notice["models"] = models
            notice["updated_on"] = today_s
        notice["models_fetched_on"] = today_s
        summary["models_fetched"] += 1
    summary["models_pending"] = max(0, len(pending) - budget)


def needs_models(notice: dict, today_s: str) -> bool:
    fetched = notice.get("models_fetched_on")
    if not fetched:
        return True
    end = notice.get("apply_end") or notice.get("announce_date")
    if end and end >= today_s:
        return (date.fromisoformat(today_s) - date.fromisoformat(fetched)).days >= MODEL_REFRESH_DAYS
    return False


def is_current(notice: dict, today_s: str) -> bool:
    """알림을 보낼 가치가 있는 (아직 접수나 발표가 남은) 공고인가."""
    for key in ("apply_end", "winner_date"):
        if notice.get(key) and notice[key] >= today_s:
            return True
    if not notice.get("apply_end") and notice.get("announce_date"):
        return (date.fromisoformat(today_s) - date.fromisoformat(notice["announce_date"])).days <= 7
    return False
