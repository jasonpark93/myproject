"""오늘의 청약 CLI (cheongyak/ 디렉터리에서 실행)

  python -m app update    공공데이터 API에서 공고 수집 → data/notices.json   (DATA_GO_KR_API_KEY 필요)
  python -m app build     data/notices.json → dist/ 정적 사이트
  python -m app notify    이번 update에서 새로 찾은 공고를 텔레그램으로 전송    (TELEGRAM_BOT_TOKEN, TELEGRAM_CHAT_ID 필요)
  python -m app demo      샘플 데이터로 dist-demo/ 미리보기 생성 (API 키 없이 디자인 확인용)
  python -m app inspect   API 응답 필드 이름 확인 (필드가 바뀌었는지 점검할 때)
"""

from __future__ import annotations

import argparse
import json
import os
import sys
from pathlib import Path

from . import ROOT, config, dates, notify, site, store, update
from .applyhome import KINDS, ApiError, Client
from .normalize import normalize_model, normalize_notice

DB_PATH = ROOT / "data" / "notices.json"
NEW_IDS_PATH = ROOT / "tmp" / "new_ids.json"
FIXTURES = ROOT / "tests" / "fixtures"


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(prog="python -m app", description="오늘의 청약 사이트 자동화")
    sub = parser.add_subparsers(dest="command", required=True)
    sub.add_parser("update", help="공고 수집")
    build_p = sub.add_parser("build", help="사이트 빌드")
    build_p.add_argument("--out", default=str(ROOT / "dist"))
    build_p.add_argument("--base-url", help="config.json의 base_url 대신 쓸 주소 (GitHub Actions가 넣어 준다)")
    notify_p = sub.add_parser("notify", help="텔레그램 알림")
    notify_p.add_argument("--base-url")
    demo_p = sub.add_parser("demo", help="샘플 데이터 미리보기")
    demo_p.add_argument("--out", default=str(ROOT / "dist-demo"))
    demo_p.add_argument("--base-url", default="http://localhost:8000")
    sub.add_parser("inspect", help="API 필드 점검")
    args = parser.parse_args(argv)

    try:
        return COMMANDS[args.command](args)
    except config.ConfigError as e:
        print(f"설정 오류: {e}", file=sys.stderr)
        return 2
    except ApiError as e:
        print(f"API 오류: {e}", file=sys.stderr)
        return 1


def cmd_update(args) -> int:
    cfg = config.load()
    client = Client(os.environ.get("DATA_GO_KR_API_KEY", ""))
    summary = update.run(cfg, client, DB_PATH, dates.today_kst())
    NEW_IDS_PATH.parent.mkdir(parents=True, exist_ok=True)
    NEW_IDS_PATH.write_text(json.dumps(summary["notify"], ensure_ascii=False), encoding="utf-8")

    lines = [
        f"수집 기준: 모집공고일 {summary['since']} 이후" + (" (첫 실행: 과거 공고 일괄 수집)" if summary["seed"] else ""),
        *(f"- {KINDS[k]['label']}: 받은 {v['fetched']}건 (정상 {v['valid']}건), 새 공고 {v['new']}건" for k, v in summary["kinds"].items()),
        f"- 주택형 정보 조회 {summary['models_fetched']}건, 다음 실행으로 넘긴 {summary['models_pending']}건",
        f"- 저장된 전체 공고 {summary['total']}건, 알림 대상 {len(summary['notify'])}건, API 호출 {client.calls}회",
    ]
    if summary["errors"]:
        lines.append(f"- 경고 {len(summary['errors'])}건:")
        lines += [f"  - {e}" for e in summary["errors"][:20]]
    print("\n".join(lines))
    _step_summary("### 청약 데이터 수집\n\n" + "\n".join(lines))
    return 0


def cmd_build(args) -> int:
    cfg = config.load(base_url=args.base_url)
    result = site.build(cfg, store.load(DB_PATH), args.out, today=dates.today_kst(), now=dates.now_kst())
    c = result["counts"]
    text = (
        f"페이지 {result['pages']}개 생성 (공고 {result['notices']}건: 접수 중 {c['open']}, 예정 {c['upcoming']}, "
        f"발표 대기 {c['winner_wait']}) → {args.out}  주소: {cfg['base_url']}/"
    )
    print(text)
    _step_summary("### 사이트 빌드\n\n" + text)
    return 0


def cmd_notify(args) -> int:
    token, chat_id = os.environ.get("TELEGRAM_BOT_TOKEN", ""), os.environ.get("TELEGRAM_CHAT_ID", "")
    if not (token and chat_id):
        print("TELEGRAM_BOT_TOKEN / TELEGRAM_CHAT_ID가 없어 알림을 건너뜁니다.")
        return 0
    ids = json.loads(NEW_IDS_PATH.read_text(encoding="utf-8")) if NEW_IDS_PATH.exists() else []
    if not ids:
        print("새로 알릴 공고가 없습니다.")
        return 0
    cfg = config.load(base_url=args.base_url)
    db = store.load(DB_PATH)
    today = dates.today_kst()
    messages = [notify.message(cfg, db["notices"][i], today) for i in ids if i in db["notices"]]
    sent, errors = notify.send_all(token, chat_id, messages)
    print(f"텔레그램 전송 {sent}/{len(messages)}건")
    for error in errors:
        print(f"  - {error}")
    return 0


def cmd_demo(args) -> int:
    """tests/fixtures의 가짜 API 응답으로 사이트를 만든다. 실제 배포에는 쓰지 않는다."""
    fixture = json.loads((FIXTURES / "sample_api.json").read_text(encoding="utf-8"))
    today = dates.d(fixture["today"])
    db = store.empty()
    for kind, records in fixture["details"].items():
        notices = [n for n in (normalize_notice(kind, r) for r in records) if n]
        store.merge(db, notices, fixture["today"])
    for notice in db["notices"].values():
        rows = fixture["models"].get(notice["house_manage_no"], [])
        notice["models"] = [normalize_model(r) for r in rows]
        notice["models_fetched_on"] = fixture["today"]
    db["updated_at"] = f"{fixture['today']}T06:20:00+09:00"
    cfg = config.load(base_url=args.base_url)
    now = dates.now_kst()
    result = site.build(cfg, db, args.out, today=today, now=now, demo=True)
    print(f"미리보기 {result['pages']}페이지 → {args.out}")
    print(f"  보기: cd {args.out} && python3 -m http.server 8000  →  {args.base_url}/")
    return 0


def cmd_inspect(args) -> int:
    client = Client(os.environ.get("DATA_GO_KR_API_KEY", ""))
    for kind, spec in KINDS.items():
        for role in ("detail", "model"):
            operation = spec[role]
            try:
                payload = client.get(operation, {"page": 1, "perPage": 1})
            except ApiError as e:
                print(f"{operation}: 실패 — {e}")
                continue
            rows = payload.get("data") or []
            keys = sorted(rows[0]) if rows else []
            print(f"{operation}: 전체 {payload.get('totalCount')}건, 필드 {len(keys)}개\n  {', '.join(keys)}")
    return 0


def _step_summary(markdown: str) -> None:
    path = os.environ.get("GITHUB_STEP_SUMMARY")
    if path:
        with open(path, "a", encoding="utf-8") as f:
            f.write(markdown + "\n\n")


COMMANDS = {
    "update": cmd_update,
    "build": cmd_build,
    "notify": cmd_notify,
    "demo": cmd_demo,
    "inspect": cmd_inspect,
}

if __name__ == "__main__":
    sys.exit(main())
