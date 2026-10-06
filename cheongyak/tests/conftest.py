import json
from datetime import date, datetime
from pathlib import Path

import pytest

from app import config, dates, store
from app.normalize import normalize_model, normalize_notice

FIXTURES = Path(__file__).parent / "fixtures"


@pytest.fixture
def api():
    return json.loads((FIXTURES / "sample_api.json").read_text(encoding="utf-8"))


@pytest.fixture
def today():
    return date(2026, 10, 6)


@pytest.fixture
def now():
    return datetime(2026, 10, 6, 6, 20, tzinfo=dates.KST)


@pytest.fixture
def make_cfg():
    def make(**overrides):
        return config.validate({**config.DEFAULTS, "base_url": "https://example.github.io/myproject", **overrides})

    return make


@pytest.fixture
def cfg(make_cfg):
    return make_cfg()


@pytest.fixture
def db(api, today):
    """샘플 API 응답을 수집까지 마친 상태로 만든 저장소."""
    result = store.empty()
    for kind, records in api["details"].items():
        store.merge(result, [n for n in (normalize_notice(kind, r) for r in records) if n], today.isoformat())
    for notice in result["notices"].values():
        notice["models"] = [normalize_model(r) for r in api["models"].get(notice["house_manage_no"], [])]
        notice["models_fetched_on"] = today.isoformat()
    result["updated_at"] = "2026-10-06T06:20:00+09:00"
    return result
