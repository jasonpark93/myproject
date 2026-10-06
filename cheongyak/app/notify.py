"""새 청약 공고를 텔레그램 채널로 보낸다. 봇 토큰은 로그에 남기지 않는다."""

from __future__ import annotations

import html
import json
import time
import urllib.error
import urllib.request
from datetime import date
from typing import Callable

from . import view

MAX_MESSAGES = 15  # 한 번에 너무 많이 보내면 채널 구독자가 떠난다
SEND_INTERVAL = 3.0  # 텔레그램 채널 전송 제한(분당 20건)보다 넉넉하게


def message(cfg: dict, notice: dict, today: date) -> str:
    v = view.enrich(notice, today)
    lines = [f"🏠 <b>[{html.escape(v['region'])}] {html.escape(v['name'])}</b>"]
    meta = [v["kind_label"]]
    if v.get("total_units"):
        meta.append(f"총 {v['total_units']:,}세대")
    if v.get("supply_type"):
        meta.append(v["supply_type"])
    lines.append(html.escape(" · ".join(meta)))
    steps = [s for s in v["schedule_view"] if s["key"] in ("special", "rank1_local", "receipt", "general", "winner")]
    if steps:
        lines.append("📅 " + html.escape(" · ".join(f"{s['label']} {s['short']}" for s in steps)))
    if v.get("price_range"):
        lines.append(f"💰 최고 분양가 {html.escape(v['price_range'])}")
    lines.append(f"👉 {html.escape(cfg['base_url'] + v['path'])}")
    return "\n".join(lines)


def send_all(
    token: str,
    chat_id: str,
    messages: list[str],
    *,
    opener: Callable = urllib.request.urlopen,
    sleep: Callable[[float], None] = time.sleep,
) -> tuple[int, list[str]]:
    """(보낸 개수, 오류 목록)을 돌려준다. 알림은 부가 기능이라 실패해도 예외를 올리지 않는다."""
    sent, errors = 0, []
    for i, text in enumerate(messages[:MAX_MESSAGES]):
        if i:
            sleep(SEND_INTERVAL)
        body = json.dumps({"chat_id": chat_id, "text": text, "parse_mode": "HTML"}).encode("utf-8")
        request = urllib.request.Request(
            f"https://api.telegram.org/bot{token}/sendMessage",
            data=body,
            headers={"Content-Type": "application/json"},
            method="POST",
        )
        try:
            with opener(request, timeout=20) as response:
                result = json.loads(response.read())
            if result.get("ok"):
                sent += 1
            else:
                errors.append(str(result.get("description", "알 수 없는 오류")))
        except urllib.error.HTTPError as e:
            try:
                detail = json.loads(e.read()).get("description", "")
            except Exception:
                detail = ""
            errors.append(f"HTTP {e.code} {detail}".strip())
        except (urllib.error.URLError, TimeoutError, ConnectionError) as e:
            errors.append(f"연결 실패 ({getattr(e, 'reason', e)})")
    if len(messages) > MAX_MESSAGES:
        errors.append(f"{len(messages) - MAX_MESSAGES}건은 한도({MAX_MESSAGES}건)를 넘어 보내지 않았습니다.")
    return sent, errors
