import pytest

from app import regions
from app.dates import parse_date, parse_ym
from app.normalize import normalize_model, normalize_notice, pretty_house_type, safe_url


def test_apt_notice(api):
    n = normalize_notice("apt", api["details"]["apt"][0])
    assert n["id"] == "apt/2099000001"
    assert n["region"] == "서울"
    assert n["total_units"] == 1234
    assert (n["house_type"], n["supply_type"]) == ("APT", "민영")
    assert (n["apply_start"], n["apply_end"]) == ("2026-10-05", "2026-10-08")
    assert [s["key"] for s in n["schedule"]] == [
        "special",
        "rank1_local",
        "rank1_gg",
        "rank1_etc",
        "rank2_local",
        "winner",
        "contract",
    ]
    assert n["winner_date"] == "2026-10-14"
    assert (n["contract_start"], n["contract_end"]) == ("2026-10-26", "2026-10-28")
    assert n["move_in"] == "2029-03"
    assert n["homepage"] == "http://www.sample-apartment.example"
    assert n["flags"]["투기과열지구"] is True
    assert n["flags"]["공공주택지구"] is False
    assert n["notice_url"].startswith("https://www.applyhome.co.kr/")


def test_remndr_uses_overall_receipt_and_compact_dates(api):
    n = normalize_notice("remndr", api["details"]["remndr"][0])
    assert n["id"] == "remndr/2099100001"
    assert n["region"] == "서울"
    assert n["schedule"][0] == {"key": "receipt", "label": "청약 접수", "start": "2026-10-09", "end": "2026-10-09"}
    assert n["winner_date"] == "2026-10-12"


def test_unsafe_homepage_is_dropped(api):
    n = normalize_notice("urbty", api["details"]["urbty"][0])
    assert n["homepage"] is None
    assert n["region"] == "경기"


def test_records_without_usable_id_are_skipped():
    assert normalize_notice("apt", {"HOUSE_NM": "이름만 있는 공고"}) is None
    assert normalize_notice("apt", {"HOUSE_MANAGE_NO": "1/../2", "HOUSE_NM": "경로 조작"}) is None
    assert normalize_notice("apt", {"HOUSE_MANAGE_NO": "2099000001"}) is None


def test_slug_includes_pblanc_no_when_different():
    n = normalize_notice("remndr", {"HOUSE_MANAGE_NO": "2099100001", "PBLANC_NO": "2099100002", "HOUSE_NM": "재공고"})
    assert n["id"] == "remndr/2099100001-2099100002"


def test_apt_model(api):
    m = normalize_model(api["models"]["2099000001"][1])
    assert m["type"] == "84.98A"
    assert m["excl_area"] == 84.98
    assert m["supply_area"] == 112.45
    assert m["top_price"] == 135000
    assert m["special"] == {"다자녀가구": 15, "신혼부부": 30, "생애최초": 25, "노부모부양": 5, "기관추천": 15}


def test_officetel_model_uses_alternative_fields(api):
    m = normalize_model(api["models"]["2099200001"][0])
    assert (m["type"], m["excl_area"], m["top_price"]) == ("A", 49.8, 42000)


@pytest.mark.parametrize(
    "raw, expected",
    [
        ("2026-10-14", "2026-10-14"),
        ("20261014", "2026-10-14"),
        ("2026.10.14", "2026-10-14"),
        ("2026-10-14 00:00:00", "2026-10-14"),
        ("2026-02-30", None),
        ("미정", None),
        ("", None),
        (None, None),
    ],
)
def test_parse_date(raw, expected):
    assert parse_date(raw) == expected


@pytest.mark.parametrize("raw, expected", [("202903", "2029-03"), ("2029-3", "2029-03"), ("202913", None), ("", None)])
def test_parse_ym(raw, expected):
    assert parse_ym(raw) == expected


@pytest.mark.parametrize(
    "raw, expected", [("084.9800A", "84.98A"), ("059.9600", "59.96"), ("114.9200 ", "114.92"), ("A", "A"), (None, None)]
)
def test_pretty_house_type(raw, expected):
    assert pretty_house_type(raw) == expected


@pytest.mark.parametrize(
    "raw, expected",
    [
        ("www.example.com", "http://www.example.com"),
        ("https://example.com/a?b=1", "https://example.com/a?b=1"),
        ("javascript:alert(1)", None),
        ("JavaScript:alert(1)", None),
        ("javascript://%0aalert(1)", None),
        ("ftp://example.com", None),
        ("", None),
    ],
)
def test_safe_url(raw, expected):
    assert safe_url(raw) == expected


@pytest.mark.parametrize(
    "name, address, expected",
    [
        ("서울특별시", None, "서울"),
        ("강원특별자치도", None, "강원"),
        ("전라북도", None, "전북"),
        ("전북특별자치도", None, "전북"),
        (None, "경상남도 창원시 성산구", "경남"),
        ("광주", None, "광주"),
        ("", "", "기타"),
    ],
)
def test_region_canonical(name, address, expected):
    assert regions.canonical(name, address) == expected
