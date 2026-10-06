import copy
from datetime import date

import pytest

from app import store, update
from app.applyhome import KINDS, ApiError


class FakeClient:
    """실제 API 대신 sample_api.json을 돌려주는 클라이언트."""

    def __init__(self, api, *, fail=(), bad_cond=(), auth_fail=False):
        self.api = api
        self.fail = set(fail)
        self.bad_cond = set(bad_cond)
        self.auth_fail = auth_fail
        self.calls = []

    def iter_records(self, operation, cond=None, per_page=None):
        self.calls.append((operation, dict(cond or {})))
        if self.auth_fail:
            raise ApiError("인증 실패", auth=True, status=401)
        if operation in self.fail:
            raise ApiError(f"{operation}: HTTP 500", status=500)
        if operation in self.bad_cond and cond:
            raise ApiError(f"{operation}: HTTP 400", status=400)
        for kind, spec in KINDS.items():
            if operation == spec["detail"]:
                since = (cond or {}).get("RCRIT_PBLANC_DE::GTE", "")
                return iter([r for r in self.api["details"].get(kind, []) if r["RCRIT_PBLANC_DE"] >= since])
            if operation == spec["model"]:
                return iter(self.api["models"].get(cond["HOUSE_MANAGE_NO::EQ"], []))
        raise AssertionError(operation)


def test_first_run_collects_everything_without_notifications(api, cfg, today, tmp_path):
    path = tmp_path / "notices.json"
    summary = update.run(cfg, FakeClient(api), path, today)
    assert summary["seed"] is True
    assert summary["since"] == "2025-10-06"
    assert summary["total"] == 7
    assert summary["notify"] == []
    assert summary["models_fetched"] == 7
    saved = store.load(path)
    assert saved["notices"]["apt/2099000001"]["first_seen"] == "2026-10-06"
    assert len(saved["notices"]["apt/2099000001"]["models"]) == 3


def test_next_run_notifies_only_new_current_notices(api, cfg, today, tmp_path):
    path = tmp_path / "notices.json"
    update.run(cfg, FakeClient(api), path, today)

    fresh = copy.deepcopy(api["details"]["apt"][1])
    fresh.update(HOUSE_MANAGE_NO="2099000009", PBLANC_NO="2099000009", HOUSE_NM="새로 올라온 샘플단지", RCRIT_PBLANC_DE="2026-10-06")
    stale = copy.deepcopy(api["details"]["apt"][4])
    stale.update(HOUSE_MANAGE_NO="2099000010", PBLANC_NO="2099000010", HOUSE_NM="늦게 잡힌 지난 공고", RCRIT_PBLANC_DE="2026-09-01")
    api["details"]["apt"] += [fresh, stale]

    client = FakeClient(api)
    summary = update.run(cfg, client, path, today)
    assert summary["seed"] is False
    assert summary["since"] == "2026-08-22"  # refresh_days=45
    assert sorted(summary["new"]) == ["apt/2099000009", "apt/2099000010"]
    assert summary["notify"] == ["apt/2099000009"]
    # 이미 받은 주택형 정보는 다시 받지 않고, 새 공고 것만 받는다
    model_calls = [c for c in client.calls if c[0] == KINDS["apt"]["model"]]
    assert sorted(c[1]["HOUSE_MANAGE_NO::EQ"] for c in model_calls) == ["2099000009", "2099000010"]


def test_updated_on_changes_only_when_content_changes(api, cfg, today, tmp_path):
    path = tmp_path / "notices.json"
    update.run(cfg, FakeClient(api), path, today)
    api["details"]["apt"][0]["PRZWNER_PRESNATN_DE"] = "2026-10-15"  # 정정공고
    update.run(cfg, FakeClient(api), path, date(2026, 10, 7))
    notices = store.load(path)["notices"]
    assert notices["apt/2099000001"]["updated_on"] == "2026-10-07"
    assert notices["apt/2099000001"]["winner_date"] == "2026-10-15"
    assert notices["apt/2099000001"]["first_seen"] == "2026-10-06"
    assert notices["apt/2099000005"]["updated_on"] == "2026-10-06"


def test_model_budget_is_respected(api, make_cfg, today, tmp_path):
    summary = update.run(make_cfg(max_model_calls_per_run=2), FakeClient(api), tmp_path / "n.json", today)
    assert summary["models_fetched"] == 2
    assert summary["models_pending"] == 5


def test_one_kind_failing_does_not_stop_others(api, cfg, today, tmp_path):
    summary = update.run(cfg, FakeClient(api, fail={KINDS["urbty"]["detail"]}), tmp_path / "n.json", today)
    assert set(summary["kinds"]) == {"apt", "remndr"}
    assert len(summary["errors"]) == 1
    assert summary["total"] == 6


def test_all_kinds_failing_raises(api, cfg, today, tmp_path):
    client = FakeClient(api, fail={spec["detail"] for spec in KINDS.values()})
    with pytest.raises(ApiError):
        update.run(cfg, client, tmp_path / "n.json", today)
    assert not (tmp_path / "n.json").exists()


def test_auth_error_aborts_immediately(api, cfg, today, tmp_path):
    with pytest.raises(ApiError) as info:
        update.run(cfg, FakeClient(api, auth_fail=True), tmp_path / "n.json", today)
    assert info.value.auth


def test_falls_back_when_date_filter_is_rejected(api, cfg, today, tmp_path):
    client = FakeClient(api, bad_cond={KINDS["remndr"]["detail"]})
    summary = update.run(cfg, client, tmp_path / "n.json", today)
    assert summary["kinds"]["remndr"]["valid"] == 1


def test_model_failures_stop_after_limit(api, cfg, today, tmp_path):
    client = FakeClient(api, fail={KINDS["apt"]["model"]})
    summary = update.run(cfg, client, tmp_path / "n.json", today)
    apt_model_calls = [c for c in client.calls if c[0] == KINDS["apt"]["model"]]
    assert len(apt_model_calls) == update.MODEL_FAILURES_PER_KIND
    assert summary["models_fetched"] == 2  # 무순위·오피스텔은 정상


def test_needs_models():
    assert update.needs_models({"models_fetched_on": None}, "2026-10-06")
    open_notice = {"models_fetched_on": "2026-10-02", "apply_end": "2026-10-08"}
    assert update.needs_models(open_notice, "2026-10-06")
    assert not update.needs_models({**open_notice, "models_fetched_on": "2026-10-05"}, "2026-10-06")
    assert not update.needs_models({"models_fetched_on": "2026-01-01", "apply_end": "2026-02-01"}, "2026-10-06")


def test_is_current():
    assert update.is_current({"apply_end": "2026-10-06"}, "2026-10-06")
    assert update.is_current({"apply_end": "2026-10-01", "winner_date": "2026-10-08"}, "2026-10-06")
    assert not update.is_current({"apply_end": "2026-10-01", "winner_date": "2026-10-05"}, "2026-10-06")
    assert update.is_current({"announce_date": "2026-10-01"}, "2026-10-06")
    assert not update.is_current({"announce_date": "2026-09-01"}, "2026-10-06")


def test_warns_when_field_names_change(api, cfg, today, tmp_path):
    api["details"]["urbty"] = [{"UNKNOWN_NAME": "x", "RCRIT_PBLANC_DE": "2026-10-01"}]
    summary = update.run(cfg, FakeClient(api), tmp_path / "n.json", today)
    assert summary["kinds"]["urbty"] == {"fetched": 1, "valid": 0, "new": 0}
    assert any("python -m app inspect" in e for e in summary["errors"])
