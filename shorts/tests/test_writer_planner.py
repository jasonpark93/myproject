"""Claude 대본 작성기와 주간 계획을 가짜 클라이언트로 검증한다 (실제 API 호출 없음)."""

import copy
import json
from datetime import date
from types import SimpleNamespace as NS

import pytest

from studio import episode, planner, writer


def text_block(text, citations=None):
    return NS(type="text", text=text, citations=citations)


def response(blocks, stop="end_turn", searches=0):
    usage = NS(input_tokens=1000, output_tokens=500, cache_read_input_tokens=0, cache_creation_input_tokens=0, server_tool_use=NS(web_search_requests=searches))
    return NS(content=blocks, stop_reason=stop, stop_details=None, usage=usage)


class FakeClient:
    def __init__(self, responses):
        self.responses = list(responses)
        self.requests = []
        self.beta = NS(messages=NS(create=self._create))

    def _create(self, **kwargs):
        self.requests.append(copy.deepcopy({k: v for k, v in kwargs.items() if k != "messages"}) | {"messages": kwargs["messages"]})
        return self.responses.pop(0)


TODAY = date(2026, 10, 6)


def test_research_uses_web_search_and_fallback(cfg):
    cite = NS(url="https://www.molit.go.kr/x", title="국토교통부")
    client = FakeClient([
        response([NS(type="server_tool_use")], stop="pause_turn"),
        response([text_block("핵심 사실 1", [cite]), text_block("핵심 사실 2", [cite])], searches=3),
    ])
    w = writer.Writer(cfg, client)
    notes, sources = w.research({"title": "청약통장", "angle": "", "notes": ""}, TODAY)
    assert notes == "핵심 사실 1\n핵심 사실 2"
    assert sources == [{"title": "국토교통부", "url": "https://www.molit.go.kr/x"}]
    first = client.requests[0]
    assert first["tools"] == [{"type": "web_search_20260209", "name": "web_search", "max_uses": 5}]
    assert first["model"] == "claude-opus-5-5" and first["fallbacks"] == "default"
    assert first["betas"] == ["server-side-fallback-2026-07-01"]
    assert first["output_config"] == {"effort": "medium"}
    assert len(client.requests[1]["messages"]) == 2  # pause_turn 후 이어서 요청
    assert w.usage["web_search_requests"] == 3 and w.usage["calls"] == 2


def test_write_retries_with_feedback(cfg, narrated):
    bad = copy.deepcopy(narrated["script"])
    bad["scenes"][0]["headline"] = "가" * 30
    good = copy.deepcopy(narrated["script"])
    good["hashtags"] = ["청약", " #내집 마련 "]
    client = FakeClient([response([text_block(json.dumps(bad, ensure_ascii=False))]), response([text_block(json.dumps(good, ensure_ascii=False))])])
    script = writer.Writer(cfg, client).write(topic={"title": "가점", "angle": ""}, series_name="explainer", today=TODAY, material="자료")
    assert script["hashtags"] == ["#청약", "#내집마련"]
    fmt = client.requests[0]["output_config"]["format"]
    assert fmt == {"type": "json_schema", "schema": writer.NARRATED_SCHEMA}
    assert "[자료]\n자료" in client.requests[0]["messages"][0]["content"]
    assert "headline이 너무 깁니다" in client.requests[1]["messages"][0]["content"]


def test_write_gives_up_after_attempts(cfg, narrated):
    bad = copy.deepcopy(narrated["script"])
    bad["title"] = ""
    client = FakeClient([response([text_block(json.dumps(bad))])] * 2)
    with pytest.raises(writer.WriterError, match="검증 실패"):
        writer.Writer(cfg, client).write(topic={"title": "t"}, series_name="explainer", today=TODAY, material="")


def test_refusal_is_an_error(cfg):
    refused = response([], stop="refusal")
    refused.stop_details = NS(category="general_harms", explanation="")
    with pytest.raises(writer.WriterError, match="거절"):
        writer.Writer(cfg, FakeClient([refused])).research({"title": "t"}, TODAY)


def test_list_format_uses_list_schema(cfg, listed):
    client = FakeClient([response([text_block(json.dumps(listed["script"], ensure_ascii=False))])])
    writer.Writer(cfg, client).write(topic={"title": "체크리스트"}, series_name="topn", today=TODAY, material="")
    assert client.requests[0]["output_config"]["format"]["schema"] == writer.LIST_SCHEMA
    assert "[형식: 리스트]" in client.requests[0]["messages"][0]["content"]


def test_schemas_follow_structured_output_rules():
    def walk(node):
        if node.get("type") == "object":
            assert node["additionalProperties"] is False
            assert set(node["required"]) == set(node["properties"])
            for child in node["properties"].values():
                walk(child)
        if node.get("type") == "array":
            walk(node["items"])

    for schema in (writer.NARRATED_SCHEMA, writer.LIST_SCHEMA, writer.topics_schema(["explainer"])):
        walk(schema)


def test_suggest_topics_sanitizes(cfg):
    payload = {"topics": [
        {"id": "Year End Tax!!", "series": "explainer", "title": "연말정산", "angle": "a", "notes": "n"},
        {"id": "x", "series": "cheongyak", "title": "데이터 시리즈는 제외", "angle": "", "notes": ""},
        {"id": "", "series": "topn", "title": "id 없음", "angle": "", "notes": ""},
    ]}
    client = FakeClient([response([text_block(json.dumps(payload, ensure_ascii=False))])])
    topics = writer.Writer(cfg, client).suggest_topics(today=TODAY, count=3, used_titles=["기존"], popular_titles=[])
    assert [t["id"] for t in topics] == ["year-end-tax"]
    assert topics[0]["used_on"] is None
    assert client.requests[0]["output_config"]["format"]["schema"]["properties"]["topics"]["items"]["properties"]["series"]["enum"] == ["explainer", "topn"]


# ---------- 계획 ----------
class FakeWriter:
    def __init__(self, narrated, listed):
        self.narrated, self.listed = narrated, listed
        self.calls = []

    def research(self, topic, today):
        self.calls.append(("research", topic["id"]))
        return "확인된 사실", [{"title": "기관", "url": "https://example.go.kr"}]

    def write(self, *, topic, series_name, today, material):
        self.calls.append(("write", series_name, topic["id"], material[:20]))
        return copy.deepcopy(self.listed["script"] if series_name == "topn" else self.narrated["script"])

    def suggest_topics(self, **kwargs):
        self.calls.append(("suggest",))
        return [{"id": "ai-topic", "series": "explainer", "title": "AI 주제", "angle": "", "notes": "", "used_on": None}]


@pytest.fixture
def bank(tmp_path):
    path = tmp_path / "topics.json"
    planner.save_topics(
        {"topics": [
            {"id": "gajeom", "series": "explainer", "title": "가점", "angle": "", "notes": "", "used_on": None},
            {"id": "checklist", "series": "topn", "title": "체크리스트", "angle": "", "notes": "", "used_on": None},
            {"id": "old", "series": "explainer", "title": "이미 씀", "angle": "", "notes": "", "used_on": "2026-01-01"},
        ]},
        path,
    )
    return path


CHEONGYAK = {"notices": {
    "apt/1": {"name": "가상 아파트", "region": "서울", "kind_label": "APT 분양", "total_units": 1200, "apply_start": "2026-10-13",
              "schedule": [{"key": "special", "start": "2026-10-13"}, {"key": "rank1_local", "start": "2026-10-14"}, {"key": "winner", "start": "2026-10-21"}],
              "models": [{"top_price": 98000}, {"top_price": 135000}]},
    "apt/2": {"name": "다음달 단지", "region": "경기", "kind_label": "APT 분양", "total_units": 500, "apply_start": "2026-11-20", "schedule": []},
}}


def test_cheongyak_digest():
    text = planner.cheongyak_digest(CHEONGYAK, date(2026, 10, 12))
    assert "[서울] 가상 아파트 (APT 분양, 1,200세대, 최고 분양가 13억 5,000만원) — 특별공급 10/13(화), 1순위 10/14(수), 당첨자 발표 10/21(수)" in text
    assert "다음달 단지" not in text
    assert planner.cheongyak_digest(CHEONGYAK, date(2026, 12, 1)) is None


def test_plan_week(tmp_path, cfg, bank, narrated, listed):
    episodes_dir = tmp_path / "episodes"
    episodes_dir.mkdir()
    fake = FakeWriter(narrated, listed)
    created = planner.plan(cfg, start=date(2026, 10, 12), days=4, writer=fake, today=TODAY, episodes_dir=episodes_dir,
                           topics_path=bank, cheongyak=CHEONGYAK, log=lambda *_: None)
    names = [p.name for p in created]
    assert names[0] == "2026-10-12-cheongyak-cheongyak-week-2026-10-12.json"  # 월요일: 청약 데이터 편
    assert names[1] == "2026-10-13-explainer-gajeom.json"  # 화요일: 주제 은행
    assert names[2] == "2026-10-14-topn-checklist.json"  # 수요일: 리스트
    assert names[3] == "2026-10-15-explainer-ai-topic.json"  # 목요일: 주제가 떨어져 AI 추천
    mon = episode.load(created[0])
    assert mon["publish_at"] == "2026-10-12T19:00:00+09:00" and "가상 아파트" in mon["research_notes"]
    assert ("research", "gajeom") in fake.calls and ("suggest",) in fake.calls
    assert not any(c[0] == "research" and c[1].startswith("cheongyak") for c in fake.calls)
    topics = {t["id"]: t for t in planner.load_topics(bank)["topics"]}
    assert topics["gajeom"]["used_on"] == "2026-10-13" and topics["ai-topic"]["used_on"] == "2026-10-15"

    again = planner.plan(cfg, start=date(2026, 10, 12), days=4, writer=fake, today=TODAY, episodes_dir=episodes_dir,
                         topics_path=bank, cheongyak=CHEONGYAK, log=lambda *_: None)
    assert again == []  # 같은 날짜는 덮어쓰지 않는다


def test_plan_falls_back_without_data(tmp_path, cfg, bank, narrated, listed):
    episodes_dir = tmp_path / "episodes"
    episodes_dir.mkdir()
    created = planner.plan(cfg, start=date(2026, 10, 12), days=1, writer=FakeWriter(narrated, listed), today=TODAY,
                           episodes_dir=episodes_dir, topics_path=bank, cheongyak=None, log=lambda *_: None)
    assert created[0].name == "2026-10-12-explainer-gajeom.json"


def test_review_markdown(tmp_path, cfg, narrated, listed):
    a, b = tmp_path / "a.json", tmp_path / "b.json"
    narrated["script"]["fact_check"] = ["숫자 확인"]
    episode.save(a, narrated)
    episode.save(b, listed)
    text = planner.review_markdown([a, b], cfg)
    assert "청약 가점 84점, 이렇게 계산합니다" in text and "- [ ] 숫자 확인" in text
    assert "1. 모집공고문 자격 조건 — 지역·소득·자산 기준" in text
    assert "예상 길이 약" in text
