"""iCalendar(.ics) 생성. 구글·애플·아웃룩 캘린더에서 '구독'하거나 내려받아 쓸 수 있다."""

from __future__ import annotations

from datetime import date, datetime, timedelta, timezone


def escape(text: str) -> str:
    return (
        text.replace("\\", "\\\\").replace(";", "\\;").replace(",", "\\,").replace("\r\n", "\\n").replace("\n", "\\n")
    )


def fold(line: str) -> str:
    """RFC 5545: 한 줄은 75옥텟 이하. 멀티바이트 문자는 쪼개지 않는다."""
    chunks, current, size = [], "", 0
    for ch in line:
        width = len(ch.encode("utf-8"))
        limit = 75 if not chunks else 74  # 이어지는 줄은 앞에 공백 1옥텟이 붙는다
        if size + width > limit:
            chunks.append(current)
            current, size = ch, width
        else:
            current += ch
            size += width
    chunks.append(current)
    return "\r\n ".join(chunks)


def calendar(name: str, events: list[dict], *, now: datetime) -> str:
    """events: {uid, start, end, summary, description, url} — start/end는 'YYYY-MM-DD' (end 포함)."""
    stamp = now.astimezone(timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    lines = [
        "BEGIN:VCALENDAR",
        "VERSION:2.0",
        f"PRODID:-//{escape(name)}//KO",
        "CALSCALE:GREGORIAN",
        "METHOD:PUBLISH",
        f"X-WR-CALNAME:{escape(name)}",
        "X-WR-TIMEZONE:Asia/Seoul",
        "REFRESH-INTERVAL;VALUE=DURATION:PT12H",
        "X-PUBLISHED-TTL:PT12H",
    ]
    for event in events:
        start = date.fromisoformat(event["start"])
        end = date.fromisoformat(event.get("end") or event["start"]) + timedelta(days=1)  # DTEND는 다음 날(미포함)
        lines += [
            "BEGIN:VEVENT",
            f"UID:{event['uid']}",
            f"DTSTAMP:{stamp}",
            f"DTSTART;VALUE=DATE:{start:%Y%m%d}",
            f"DTEND;VALUE=DATE:{end:%Y%m%d}",
            f"SUMMARY:{escape(event['summary'])}",
        ]
        if event.get("description"):
            lines.append(f"DESCRIPTION:{escape(event['description'])}")
        if event.get("url"):
            lines.append(f"URL:{event['url']}")
        lines += ["TRANSP:TRANSPARENT", "END:VEVENT"]
    lines.append("END:VCALENDAR")
    return "\r\n".join(fold(line) for line in lines) + "\r\n"
