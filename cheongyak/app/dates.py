"""날짜 파싱·표시 도우미. 사이트의 '오늘'은 항상 한국 시간 기준이다."""

from __future__ import annotations

import os
import re
from datetime import date, datetime
from zoneinfo import ZoneInfo

KST = ZoneInfo("Asia/Seoul")
WEEKDAYS = "월화수목금토일"

_DATE = re.compile(r"(\d{4})[-./]?(\d{1,2})[-./]?(\d{1,2})(?:[ T].*)?")
_YM = re.compile(r"(\d{4})[-./]?(\d{1,2})")


def now_kst() -> datetime:
    return datetime.now(KST)


def today_kst() -> date:
    """CHEONGYAK_TODAY=YYYY-MM-DD 로 덮어쓸 수 있다 (테스트·미리보기용)."""
    override = os.environ.get("CHEONGYAK_TODAY")
    if override:
        return date.fromisoformat(override)
    return now_kst().date()


def parse_date(value) -> str | None:
    """'2026-10-14', '20261014', '2026.10.14' → '2026-10-14'. 형식이 틀리면 None."""
    if value in (None, ""):
        return None
    match = _DATE.fullmatch(str(value).strip())
    if not match:
        return None
    try:
        return date(*map(int, match.groups())).isoformat()
    except ValueError:
        return None


def parse_ym(value) -> str | None:
    """입주예정월 '202903' / '2029-03' → '2029-03'."""
    if value in (None, ""):
        return None
    match = _YM.fullmatch(str(value).strip())
    if not match:
        return None
    year, month = map(int, match.groups())
    if not 1 <= month <= 12:
        return None
    return f"{year:04d}-{month:02d}"


def d(value: str | None) -> date | None:
    return date.fromisoformat(value) if value else None


def md(value: str | None) -> str:
    """'2026-10-14' → '10/14(수)'"""
    if not value:
        return ""
    x = date.fromisoformat(value)
    return f"{x.month}/{x.day}({WEEKDAYS[x.weekday()]})"


def full(value: str | None) -> str:
    """'2026-10-14' → '2026년 10월 14일(수)'"""
    if not value:
        return ""
    x = date.fromisoformat(value)
    return f"{x.year}년 {x.month}월 {x.day}일({WEEKDAYS[x.weekday()]})"


def month_day(value: str | None) -> str:
    """'2026-10-14' → '10월 14일(수)'"""
    if not value:
        return ""
    x = date.fromisoformat(value)
    return f"{x.month}월 {x.day}일({WEEKDAYS[x.weekday()]})"


def span(start: str | None, end: str | None, fmt=md) -> str:
    if not start:
        return fmt(end) if end else ""
    if not end or end == start:
        return fmt(start)
    return f"{fmt(start)} ~ {fmt(end)}"


def ym_text(value: str | None) -> str:
    """'2029-03' → '2029년 3월'"""
    if not value:
        return ""
    year, month = value.split("-")
    return f"{int(year)}년 {int(month)}월"
