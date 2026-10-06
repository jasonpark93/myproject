"""저장된 공고 dict에 화면 표시용 값(상태, D-day, 요약 문장, 가격 등)을 붙인다."""

from __future__ import annotations

from datetime import date

from . import dates, regions

STATUS_LABELS = {
    "open": "접수 중",
    "upcoming": "접수 예정",
    "winner_wait": "발표 대기",
    "contract": "계약 진행",
    "closed": "마감",
    "unknown": "일정 확인",
}
SHORT_LABELS = {
    "special": "특공",
    "rank1_local": "1순위",
    "receipt": "접수",
    "general": "접수",
    "winner": "발표",
}
RECEIPT_KEYS = {"special", "rank1_local", "rank1_gg", "rank1_etc", "rank2_local", "rank2_gg", "rank2_etc", "general", "receipt"}

FLAG_HELP = {
    "투기과열지구": "1순위 자격, 재당첨 제한, 전매제한 등 청약 규제가 가장 강하게 적용되는 지역입니다.",
    "조정대상지역": "1순위 자격 요건과 대출·세금 규제가 강화되는 지역입니다.",
    "분양가상한제": "분양가가 상한 이하로 정해지는 대신 전매제한·실거주 의무가 붙을 수 있습니다.",
    "정비사업": "재건축·재개발 같은 정비사업으로 공급되는 단지입니다.",
    "공공주택지구": "공공주택 특별법에 따라 지정된 공공주택지구에서 공급됩니다.",
    "대규모 택지개발지구": "대규모 택지개발지구에서 공급되는 단지입니다.",
    "수도권 내 민영 공공주택지구": "수도권 공공주택지구 안에서 공급되는 민영주택입니다.",
}

PYEONG = 3.305785


def status_of(n: dict, today: date) -> str:
    start, end = dates.d(n.get("apply_start")), dates.d(n.get("apply_end"))
    winner = dates.d(n.get("winner_date"))
    contract_end = dates.d(n.get("contract_end"))
    if start and today < start:
        return "upcoming"
    if start and today <= (end or start):
        return "open"
    if winner and today <= winner:
        return "winner_wait"
    if contract_end and today <= contract_end:
        return "contract"
    if not (start or winner or contract_end):
        return "unknown"
    return "closed"


def dday_of(n: dict, status: str, today: date) -> str:
    if status == "upcoming":
        return f"D-{(dates.d(n['apply_start']) - today).days}"
    if status == "open":
        days = (dates.d(n.get("apply_end") or n["apply_start"]) - today).days
        return "오늘 마감" if days == 0 else f"마감 D-{days}"
    if status == "winner_wait":
        days = (dates.d(n["winner_date"]) - today).days
        return "오늘 발표" if days == 0 else f"발표 D-{days}"
    if status == "contract":
        return f"계약 ~{dates.md(n['contract_end'])}"
    return ""


def manwon(value: int | None) -> str:
    """98000(만원) → '9억 8,000만원'"""
    if not value:
        return ""
    eok, man = divmod(int(value), 10000)
    if eok and man:
        return f"{eok}억 {man:,}만원"
    if eok:
        return f"{eok}억원"
    return f"{man:,}만원"


def eok_short(value: int | None) -> str:
    """98000(만원) → '9.8억'"""
    if not value:
        return ""
    if value >= 10000:
        return f"{value / 10000:.1f}".rstrip("0").rstrip(".") + "억"
    return f"{value:,}만"


def josa(word: str, with_final: str, without_final: str) -> str:
    """받침 유무에 따라 은/는, 이/가 등을 고른다. 판단할 수 없으면 '은(는)' 꼴."""
    for ch in reversed(word):
        code = ord(ch)
        if 0xAC00 <= code <= 0xD7A3:
            return with_final if (code - 0xAC00) % 28 else without_final
        if ch.isdigit():
            return with_final if ch in "013678" else without_final
        if ch.isalpha():
            break
    return f"{with_final}({without_final})"


def _model_view(m: dict) -> dict:
    supply = m.get("supply_area")
    pyeong = round(supply / PYEONG) if supply else None
    per_pyeong = round(m["top_price"] / (supply / PYEONG)) if supply and m.get("top_price") else None
    units = (m.get("units_general") or 0) + (m.get("units_special") or 0)
    return {
        **m,
        "pyeong": pyeong,
        "price_text": manwon(m.get("top_price")),
        "per_pyeong_text": f"{per_pyeong:,}만원" if per_pyeong else "",
        "units_total": units or None,
    }


def enrich(n: dict, today: date) -> dict:
    status = status_of(n, today)
    today_s = today.isoformat()

    schedule = []
    for step in n.get("schedule", []):
        end = step.get("end") or step["start"]
        state = "done" if end < today_s else ("now" if step["start"] <= today_s else "next")
        schedule.append(
            {
                **step,
                "state": state,
                "text": dates.span(step["start"], step.get("end"), dates.month_day),
                "short": dates.span(step["start"], step.get("end"), dates.md),
                "receipt": step["key"] in RECEIPT_KEYS,
            }
        )

    key_dates, seen = [], set()
    for step in schedule:
        short = SHORT_LABELS.get(step["key"])
        if short and short not in seen:
            seen.add(short)
            key_dates.append({"label": short, "text": dates.md(step["start"])})

    models = [_model_view(m) for m in n.get("models") or []]
    prices = [m["top_price"] for m in models if m.get("top_price")]
    price_lo, price_hi = (min(prices), max(prices)) if prices else (None, None)
    if price_lo and price_lo != price_hi:
        price_range = f"{manwon(price_lo)} ~ {manwon(price_hi)}"
    else:
        price_range = manwon(price_hi)

    special_totals: dict[str, int] = {}
    for m in models:
        for label, count in (m.get("special") or {}).items():
            special_totals[label] = special_totals.get(label, 0) + count

    region = n.get("region") or regions.OTHER[0]
    view = {
        **n,
        "path": f"/{n['id']}/",
        "region": region,
        "region_slug": regions.slug(region),
        "status": status,
        "status_label": STATUS_LABELS[status],
        "dday": dday_of(n, status, today),
        "schedule_view": schedule,
        "key_dates": key_dates[:3],
        "models_view": models,
        "price_hi": price_hi,
        "price_short": eok_short(price_hi),
        "price_range": price_range,
        "special_totals": special_totals,
        "active_flags": [f for f, on in (n.get("flags") or {}).items() if on],
        "flag_help": FLAG_HELP,
        "move_in_text": dates.ym_text(n.get("move_in")),
        "announce_text": dates.full(n.get("announce_date")),
    }
    view["summary"] = summary_text(view)
    view["meta_description"] = meta_description(view)
    return view


def summary_text(v: dict) -> str:
    """검색엔진과 사람이 모두 읽기 좋은 요약 문단."""
    name = v["name"]
    place = v.get("address") or f"{v['region']} 지역"
    sentences = []
    if v.get("total_units"):
        sentences.append(f"{name}{josa(name, '은', '는')} {place}에 공급되는 총 {v['total_units']:,}세대 규모의 {v['kind_label']} 공고입니다.")
    else:
        sentences.append(f"{name}{josa(name, '은', '는')} {place}에 공급되는 {v['kind_label']} 공고입니다.")

    steps = {s["key"]: s for s in v["schedule_view"]}
    receipt = [s for s in v["schedule_view"] if s["receipt"]]
    if "special" in steps and "rank1_local" in steps:
        sentences.append(f"특별공급은 {steps['special']['text']}, 1순위 해당지역은 {steps['rank1_local']['text']}에 접수합니다.")
    elif receipt:
        sentences.append(f"청약 접수는 {dates.span(v['apply_start'], v['apply_end'], dates.month_day)}입니다.")
    tail = []
    if v.get("winner_date"):
        tail.append(f"당첨자 발표는 {dates.month_day(v['winner_date'])}")
    if v.get("contract_start"):
        tail.append(f"계약은 {dates.span(v['contract_start'], v.get('contract_end'), dates.month_day)}")
    if tail:
        sentences.append(", ".join(tail) + "입니다.")
    if v.get("price_range"):
        sentences.append(f"주택형별 최고 분양가는 {v['price_range']}입니다.")
    if v.get("move_in_text"):
        sentences.append(f"입주 예정 시기는 {v['move_in_text']}입니다.")
    return " ".join(sentences)


def meta_description(v: dict) -> str:
    parts = [f"{v['region']} {v['name']} {v['kind_label']}"]
    if v.get("total_units"):
        parts.append(f"총 {v['total_units']:,}세대")
    if v["key_dates"]:
        parts.append(", ".join(f"{k['label']} {k['text']}" for k in v["key_dates"]))
    if v.get("price_range"):
        parts.append(f"최고 분양가 {v['price_range']}")
    return " · ".join(parts) + ". 청약 일정, 주택형별 분양가, 공급 세대수를 정리했습니다."


SORT_KEYS = {
    "open": lambda v: (v.get("apply_end") or "", v["name"]),
    "upcoming": lambda v: (v.get("apply_start") or "", v["name"]),
    "winner_wait": lambda v: (v.get("winner_date") or "", v["name"]),
    "contract": lambda v: (v.get("contract_end") or "", v["name"]),
}


def recent_first(v: dict) -> tuple:
    return (v.get("apply_start") or v.get("announce_date") or "", v["id"])
