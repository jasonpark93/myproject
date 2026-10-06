from datetime import date

import pytest

from app import view
from app.normalize import normalize_notice


def enriched(api, kind, index, today, models=()):
    notice = normalize_notice(kind, api["details"][kind][index])
    return view.enrich({**notice, "models": list(models)}, today)


@pytest.mark.parametrize(
    "kind, index, status, dday",
    [
        ("apt", 0, "open", "마감 D-2"),
        ("apt", 1, "upcoming", "D-7"),
        ("apt", 2, "winner_wait", "발표 D-2"),
        ("apt", 3, "contract", "계약 ~10/10(토)"),
        ("apt", 4, "closed", ""),
        ("remndr", 0, "upcoming", "D-3"),
        ("urbty", 0, "open", "마감 D-1"),
    ],
)
def test_status_and_dday(api, today, kind, index, status, dday):
    v = enriched(api, kind, index, today)
    assert (v["status"], v["dday"]) == (status, dday)


def test_same_day_labels(api):
    assert enriched(api, "apt", 0, date(2026, 10, 8))["dday"] == "오늘 마감"
    assert enriched(api, "apt", 0, date(2026, 10, 14))["dday"] == "오늘 발표"


def test_notice_without_dates_is_unknown():
    v = view.enrich({"id": "apt/1", "name": "일정 없음", "kind_label": "APT 분양", "region": "서울", "schedule": []}, date(2026, 10, 6))
    assert (v["status"], v["dday"]) == ("unknown", "")


def test_schedule_states(api, today):
    states = {s["key"]: s["state"] for s in enriched(api, "apt", 0, today)["schedule_view"]}
    assert states["special"] == "done"
    assert states["rank1_local"] == "now"
    assert states["rank1_gg"] == "next"


def test_prices_and_summary(api, db, today):
    v = view.enrich(db["notices"]["apt/2099000001"], today)
    assert v["price_range"] == "9억 8,000만원 ~ 18억 2,000만원"
    assert v["price_short"] == "18.2억"
    model = v["models_view"][1]
    assert model["pyeong"] == 34
    assert model["per_pyeong_text"] == "3,969만원"
    assert v["special_totals"]["신혼부부"] == 60
    assert v["summary"].startswith("샘플 예시아파트 1단지는 서울특별시 가상구 예시동 123 일원에 공급되는 총 1,234세대 규모의 APT 분양 공고입니다.")
    assert "특별공급은 10월 5일(월), 1순위 해당지역은 10월 6일(화)에 접수합니다." in v["summary"]
    assert "당첨자 발표는 10월 14일(수), 계약은 10월 26일(월) ~ 10월 28일(수)입니다." in v["summary"]
    assert v["summary"].endswith("입주 예정 시기는 2029년 3월입니다.")
    assert [k["label"] for k in v["key_dates"]] == ["특공", "1순위", "발표"]


@pytest.mark.parametrize("value, expected", [(98000, "9억 8,000만원"), (100000, "10억원"), (8500, "8,500만원"), (None, "")])
def test_manwon(value, expected):
    assert view.manwon(value) == expected


@pytest.mark.parametrize("value, expected", [(98000, "9.8억"), (100000, "10억"), (8500, "8,500만"), (None, "")])
def test_eok_short(value, expected):
    assert view.eok_short(value) == expected


@pytest.mark.parametrize(
    "word, expected",
    [("레이크시티", "는"), ("센트럴", "은"), ("1단지", "는"), ("A동", "은"), ("무순위)", "는"), ("103", "은"), ("Xi", "은(는)")],
)
def test_josa(word, expected):
    assert view.josa(word, "은", "는") == expected
