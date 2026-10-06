"""청약홈 API 레코드(대문자 필드) → 사이트에서 쓰는 공고/주택형 dict.

공고 종류(APT·무순위·오피스텔)마다 필드 이름이 조금씩 달라서, 논리 필드마다 후보 이름을 차례로 확인한다.
API가 필드를 바꾸거나 빼먹어도 빌드가 깨지지 않도록 없는 값은 None으로 둔다.
"""

from __future__ import annotations

import re
from urllib.parse import urlsplit

from . import regions
from .applyhome import KINDS
from .dates import parse_date, parse_ym

# (key, 표시 이름, 시작일 후보, 종료일 후보). 위에서부터 진행 순서다.
RECEIPT_STEPS = [
    ("special", "특별공급", ("SPSPLY_RCEPT_BGNDE",), ("SPSPLY_RCEPT_ENDDE",)),
    ("rank1_local", "1순위 해당지역", ("GNRL_RNK1_CRSPAREA_RCPTDE", "GNRL_RNK1_CRSPAREA_RCEPT_BGNDE"), ("GNRL_RNK1_CRSPAREA_ENDDE",)),
    ("rank1_gg", "1순위 경기지역", ("GNRL_RNK1_ETC_GG_RCPTDE",), ("GNRL_RNK1_ETC_GG_ENDDE",)),
    ("rank1_etc", "1순위 기타지역", ("GNRL_RNK1_ETC_AREA_RCPTDE",), ("GNRL_RNK1_ETC_AREA_ENDDE",)),
    ("rank2_local", "2순위 해당지역", ("GNRL_RNK2_CRSPAREA_RCPTDE", "GNRL_RNK2_CRSPAREA_RCEPT_BGNDE"), ("GNRL_RNK2_CRSPAREA_ENDDE",)),
    ("rank2_gg", "2순위 경기지역", ("GNRL_RNK2_ETC_GG_RCPTDE",), ("GNRL_RNK2_ETC_GG_ENDDE",)),
    ("rank2_etc", "2순위 기타지역", ("GNRL_RNK2_ETC_AREA_RCPTDE",), ("GNRL_RNK2_ETC_AREA_ENDDE",)),
    ("general", "일반공급", ("GNRL_RCEPT_BGNDE",), ("GNRL_RCEPT_ENDDE",)),
]
# 단계별 일정이 없을 때만 쓰는 전체 접수 기간
OVERALL_RECEIPT = (("RCEPT_BGNDE", "SUBSCRPT_RCEPT_BGNDE"), ("RCEPT_ENDDE", "SUBSCRPT_RCEPT_ENDDE"))

FLAG_FIELDS = [
    ("SPECLT_RDN_EARTH_AT", "투기과열지구"),
    ("MDAT_TRGET_AREA_SECD", "조정대상지역"),
    ("PARCPRC_ULS_AT", "분양가상한제"),
    ("IMPRMN_BSNS_AT", "정비사업"),
    ("PUBLIC_HOUSE_EARTH_AT", "공공주택지구"),
    ("LRSCL_BLDLND_AT", "대규모 택지개발지구"),
    ("NPLN_PRVOPR_PUBLIC_HOUSE_AT", "수도권 내 민영 공공주택지구"),
]

SPECIAL_FIELDS = [
    ("MNYCH_HSHLDCO", "다자녀가구"),
    ("NWWDS_HSHLDCO", "신혼부부"),
    ("LFE_FRST_HSHLDCO", "생애최초"),
    ("YGMN_HSHLDCO", "청년"),
    ("NWBB_HSHLDCO", "신생아"),
    ("OLD_PARNTS_SUPORT_HSHLDCO", "노부모부양"),
    ("INSTT_RECOMEND_HSHLDCO", "기관추천"),
    ("TRANSR_INSTT_ENFSN_HSHLDCO", "이전기관"),
    ("ETC_HSHLDCO", "기타"),
]

_EMPTY = (None, "", "null", "-")


def first(record: dict, *keys: str):
    for key in keys:
        value = record.get(key)
        if isinstance(value, str):
            value = value.strip()
        if value not in _EMPTY:
            return value
    return None


def to_int(value) -> int | None:
    if value in _EMPTY:
        return None
    try:
        return int(float(str(value).replace(",", "")))
    except ValueError:
        return None


def to_float(value) -> float | None:
    if value in _EMPTY:
        return None
    try:
        return float(str(value).replace(",", ""))
    except ValueError:
        return None


def safe_url(value) -> str | None:
    """href에 넣을 수 있는 http(s) 주소만 통과시킨다. 'www.foo.com'처럼 스킴이 없으면 붙인다."""
    if value in _EMPTY:
        return None
    text = str(value).strip()
    if "://" not in text:
        if text.lower().startswith(("javascript:", "data:", "vbscript:")):
            return None
        text = "http://" + text
    parts = urlsplit(text)
    if parts.scheme.lower() not in ("http", "https") or not parts.netloc or " " in parts.netloc:
        return None
    return text


def pretty_house_type(value) -> str | None:
    """주택형 '084.9800A' → '84.98A'"""
    if value in _EMPTY:
        return None
    text = str(value).strip()
    match = re.match(r"^0*(\d+(?:\.\d+)?)\s*(.*)$", text)
    if not match:
        return text
    number, suffix = match.groups()
    pretty = f"{float(number):.4f}".rstrip("0").rstrip(".")
    return f"{pretty}{suffix.strip()}"


def area_from_type(value) -> float | None:
    """주택형 숫자 부분은 전용면적(㎡)이다."""
    if value in _EMPTY:
        return None
    match = re.match(r"^0*(\d+(?:\.\d+)?)", str(value).strip())
    return float(match.group(1)) if match else None


def slug_for(house_manage_no: str, pblanc_no: str | None) -> str:
    if not pblanc_no or pblanc_no == house_manage_no:
        return house_manage_no
    return f"{house_manage_no}-{pblanc_no}"


def normalize_notice(kind: str, record: dict) -> dict | None:
    """공고 상세 레코드 하나를 변환한다. 식별자나 단지명이 없으면 None."""
    house_no = first(record, "HOUSE_MANAGE_NO")
    name = first(record, "HOUSE_NM")
    if not house_no or not name:
        return None
    house_no = str(house_no)
    pblanc_no = first(record, "PBLANC_NO")
    pblanc_no = str(pblanc_no) if pblanc_no is not None else None
    if not re.fullmatch(r"[0-9A-Za-z_-]{1,40}", house_no) or (pblanc_no and not re.fullmatch(r"[0-9A-Za-z_-]{1,40}", pblanc_no)):
        return None  # URL 경로에 쓰므로 예상 밖의 문자는 받지 않는다

    schedule = []
    for key, label, start_keys, end_keys in RECEIPT_STEPS:
        start = parse_date(first(record, *start_keys))
        end = parse_date(first(record, *end_keys)) if end_keys else None
        if start or end:
            schedule.append({"key": key, "label": label, "start": start or end, "end": end or start})
    if not schedule:
        start = parse_date(first(record, *OVERALL_RECEIPT[0]))
        end = parse_date(first(record, *OVERALL_RECEIPT[1]))
        if start or end:
            schedule.append({"key": "receipt", "label": "청약 접수", "start": start or end, "end": end or start})

    receipt_starts = [s["start"] for s in schedule]
    receipt_ends = [s["end"] for s in schedule]
    winner = parse_date(first(record, "PRZWNER_PRESNATN_DE"))
    contract_start = parse_date(first(record, "CNTRCT_CNCLS_BGNDE"))
    contract_end = parse_date(first(record, "CNTRCT_CNCLS_ENDDE")) or contract_start
    if winner:
        schedule.append({"key": "winner", "label": "당첨자 발표", "start": winner, "end": winner})
    if contract_start:
        schedule.append({"key": "contract", "label": "계약", "start": contract_start, "end": contract_end})

    flags = {}
    for field, label in FLAG_FIELDS:
        value = first(record, field)
        if value is not None:
            flags[label] = str(value).upper() in ("Y", "1", "TRUE")

    address = first(record, "HSSPLY_ADRES")
    region = regions.canonical(first(record, "SUBSCRPT_AREA_CODE_NM"), address)
    slug = slug_for(house_no, pblanc_no)

    return {
        "id": f"{kind}/{slug}",
        "kind": kind,
        "kind_label": KINDS[kind]["label"],
        "house_manage_no": house_no,
        "pblanc_no": pblanc_no,
        "name": str(name),
        "house_type": first(record, "HOUSE_SECD_NM"),  # APT, 오피스텔, 도시형생활주택 …
        "supply_type": first(record, "HOUSE_DTL_SECD_NM"),  # 민영, 국민 …
        "rent_type": first(record, "RENT_SECD_NM"),
        "region": region,
        "address": address,
        "total_units": to_int(first(record, "TOT_SUPLY_HSHLDCO", "SUPLY_HSHLDCO")),
        "announce_date": parse_date(first(record, "RCRIT_PBLANC_DE")),
        "schedule": schedule,
        "apply_start": min(receipt_starts) if receipt_starts else None,
        "apply_end": max(receipt_ends) if receipt_ends else None,
        "winner_date": winner,
        "contract_start": contract_start,
        "contract_end": contract_end,
        "move_in": parse_ym(first(record, "MVN_PREARNGE_YM")),
        "developer": first(record, "BSNS_MBY_NM"),
        "builder": first(record, "CNSTRCT_ENTRPS_NM"),
        "phone": first(record, "MDHS_TELNO"),
        "homepage": safe_url(first(record, "HMPG_ADRES")),
        "notice_url": safe_url(first(record, "PBLANC_URL")),
        "flags": flags,
    }


def normalize_model(record: dict) -> dict:
    raw_type = first(record, "HOUSE_TY", "TP")
    special = {}
    for field, label in SPECIAL_FIELDS:
        count = to_int(first(record, field))
        if count:
            special[label] = count
    return {
        "model_no": first(record, "MODEL_NO"),
        "type": pretty_house_type(raw_type),
        "excl_area": to_float(first(record, "EXCLUSE_AR")) or area_from_type(raw_type),
        "supply_area": to_float(first(record, "SUPLY_AR")),
        "units_general": to_int(first(record, "SUPLY_HSHLDCO")),
        "units_special": to_int(first(record, "SPSPLY_HSHLDCO")),
        "top_price": to_int(first(record, "LTTOT_TOP_AMOUNT", "SUPLY_AMOUNT")),  # 만원
        "special": special,
    }
