"""청약홈 공급지역명 → 사이트 지역(이름, URL slug)."""

from __future__ import annotations

REGIONS = [
    ("서울", "seoul"),
    ("경기", "gyeonggi"),
    ("인천", "incheon"),
    ("부산", "busan"),
    ("대구", "daegu"),
    ("광주", "gwangju"),
    ("대전", "daejeon"),
    ("울산", "ulsan"),
    ("세종", "sejong"),
    ("강원", "gangwon"),
    ("충북", "chungbuk"),
    ("충남", "chungnam"),
    ("전북", "jeonbuk"),
    ("전남", "jeonnam"),
    ("경북", "gyeongbuk"),
    ("경남", "gyeongnam"),
    ("제주", "jeju"),
]
SLUGS = dict(REGIONS)
OTHER = ("기타", "etc")

# 정식 명칭·옛 명칭 → 약칭. 약칭으로 시작하지 않는 이름만 적는다.
_ALIASES = {
    "충청북도": "충북",
    "충청남도": "충남",
    "전라북도": "전북",
    "전라남도": "전남",
    "경상북도": "경북",
    "경상남도": "경남",
}


def canonical(name: str | None, address: str | None = None) -> str:
    """'서울특별시', '강원특별자치도', '전라북도' 같은 이름을 '서울', '강원', '전북'으로 맞춘다."""
    for candidate in (name, address):
        if not candidate:
            continue
        text = candidate.strip()
        for long_name, short in _ALIASES.items():
            if text.startswith(long_name):
                return short
        for short, _ in REGIONS:
            if text.startswith(short):
                return short
    return OTHER[0]


def slug(region: str) -> str:
    return SLUGS.get(region, OTHER[1])


def ordered(names: set[str]) -> list[tuple[str, str]]:
    """사이트에 표시할 순서대로 (이름, slug) 목록을 돌려준다."""
    result = [(n, s) for n, s in REGIONS if n in names]
    if OTHER[0] in names:
        result.append(OTHER)
    return result
