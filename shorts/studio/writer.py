"""Claude로 쇼츠 대본을 쓴다.

1단계 research: 웹 검색으로 최신 사실을 확인하고 출처와 함께 메모한다 (제도·숫자가 자주 바뀌는 분야라 필수).
2단계 write: 메모만 근거로 구조화된 JSON 대본을 만든다 (output_config.format으로 형식 보장).
검증에서 오류가 나오면 오류 내용을 알려 주고 한 번 더 쓰게 한다.

거절(stop_reason == "refusal") 대비로 server-side fallback("default")을 켜 둔다.
"""

from __future__ import annotations

import json
import re
from datetime import date

from . import episode as episode_mod

FALLBACK_BETA = "server-side-fallback-2026-07-01"
VISUALS = list(episode_mod.VISUAL_TYPES)

_SOURCE = {
    "type": "object",
    "properties": {"title": {"type": "string"}, "url": {"type": "string"}},
    "required": ["title", "url"],
    "additionalProperties": False,
}
_COMMON = {
    "title": {"type": "string"},
    "description": {"type": "string"},
    "hashtags": {"type": "array", "items": {"type": "string"}},
    "sources": {"type": "array", "items": _SOURCE},
    "fact_check": {"type": "array", "items": {"type": "string"}},
    "pinned_comment": {"type": "string"},
}

NARRATED_SCHEMA = {
    "type": "object",
    "properties": {
        **_COMMON,
        "scenes": {
            "type": "array",
            "items": {
                "type": "object",
                "properties": {
                    "narration": {"type": "string"},
                    "headline": {"type": "string"},
                    "visual": {
                        "type": "object",
                        "properties": {
                            "type": {"type": "string", "enum": VISUALS},
                            "value": {"type": "string"},
                            "label": {"type": "string"},
                            "items": {"type": "array", "items": {"type": "string"}},
                        },
                        "required": ["type", "value", "label", "items"],
                        "additionalProperties": False,
                    },
                    "broll": {"type": "string"},
                },
                "required": ["narration", "headline", "visual", "broll"],
                "additionalProperties": False,
            },
        },
    },
    "required": ["title", "scenes", "description", "hashtags", "sources", "fact_check", "pinned_comment"],
    "additionalProperties": False,
}

LIST_SCHEMA = {
    "type": "object",
    "properties": {
        **_COMMON,
        "list_title": {"type": "string"},
        "subtitle": {"type": "string"},
        "items": {
            "type": "array",
            "items": {
                "type": "object",
                "properties": {"text": {"type": "string"}, "detail": {"type": "string"}},
                "required": ["text", "detail"],
                "additionalProperties": False,
            },
        },
    },
    "required": ["title", "list_title", "subtitle", "items", "description", "hashtags", "sources", "fact_check", "pinned_comment"],
    "additionalProperties": False,
}


def topics_schema(series_names: list[str]) -> dict:
    return {
        "type": "object",
        "properties": {
            "topics": {
                "type": "array",
                "items": {
                    "type": "object",
                    "properties": {
                        "id": {"type": "string"},
                        "series": {"type": "string", "enum": series_names},
                        "title": {"type": "string"},
                        "angle": {"type": "string"},
                        "notes": {"type": "string"},
                    },
                    "required": ["id", "series", "title", "angle", "notes"],
                    "additionalProperties": False,
                },
            }
        },
        "required": ["topics"],
        "additionalProperties": False,
    }


class WriterError(RuntimeError):
    pass


def system_prompt(cfg: dict, today: date) -> str:
    ch = cfg["channel"]
    return f"""당신은 유튜브 쇼츠 채널 「{ch['name']}」의 전속 작가입니다.
채널 한 줄 소개: {ch['tagline']}
주 시청자: {ch['audience']}
말투: {ch['tone']}
오늘 날짜(한국 시간): {today.isoformat()}

[대본 원칙]
1. 첫 장면 2초 안에 시청자가 멈추게 만드세요. 질문, 의외의 숫자, 흔한 오해로 시작하고 인사나 채널 소개로 시작하지 않습니다.
2. 영상 하나에는 핵심 메시지 하나. 장면마다 내레이션은 1~2문장, 귀로 듣기 쉬운 짧은 문장으로 씁니다.
3. 숫자·날짜·금액·자격 조건은 사용자 메시지의 [자료]에 근거가 있는 것만 씁니다. 근거가 없으면 숫자 없이 설명합니다. 자주 바뀌는 제도는 "2026년 10월 기준"처럼 기준 시점을 밝힙니다.
4. 과장, 공포 조장, 단정적인 투자 권유를 하지 않습니다("무조건", "100%", "안 하면 손해" 같은 표현 금지). 특정 종목이나 금융상품 가입을 권하지 않습니다.
5. 전문용어는 처음 나올 때 쉬운 말로 풀어 줍니다.
6. 내레이션에서 가장 중요한 단어 하나를 [[ ]]로 감쌉니다(장면당 최대 1곳). 화면 자막에서 강조색으로 보입니다.
7. 마지막 장면은 한 문장 요약 + 자연스러운 참여 유도(저장, 구독, 댓글 질문 중 하나)로 끝냅니다.
8. headline은 화면 상단 큰 제목으로 12자 안팎의 짧은 명사구입니다. 같은 문장을 내레이션과 똑같이 반복하지 않습니다.
9. visual은 장면을 한눈에 보여주는 형식을 고릅니다: number(큰 숫자 value + 설명 label), list/check(5개 이하 짧은 항목), versus(정확히 2개 비교), quote(핵심 한 줄 value), title(제목만). 쓰지 않는 칸은 빈 문자열·빈 배열로 둡니다.
10. broll은 배경 영상 검색어입니다. Pexels에서 찾을 수 있는 영어 2~3단어(예: "apartment building", "calculator money")로, 사람 얼굴 클로즈업과 특정 브랜드·로고는 피합니다.
11. title은 40자 이내로 호기심을 끌되 내용과 정확히 일치해야 합니다(낚시 제목 금지).
12. description은 영상 내용을 2~3문장으로 요약합니다. hashtags는 # 포함 3~6개.
13. sources에는 실제로 근거로 쓴 공식 출처(기관·법령·언론)만 넣습니다.
14. fact_check에는 업로드 전에 사람이 한 번 더 확인해야 할 수치·날짜·조건을 짧게 적습니다. 없으면 빈 배열.
15. pinned_comment는 시청자 댓글을 부르는 질문 한 줄입니다.

[유튜브 정책]
같은 틀에 단어만 바꾼 영상을 반복하면 '비진정성 콘텐츠'로 수익 창출이 막힙니다. 매번 새로운 관점, 구체적인 예시, 실제로 도움이 되는 정보를 넣으세요."""


def _format_rules(series: dict, speed: float) -> str:
    if series["format"] == "list":
        return (
            "[형식: 리스트]\n"
            "- list_title은 'OO TOP N' 같은 22자 안팎의 큰 제목, subtitle은 20자 안팎의 한 줄(예: 저장해두고 확인하세요).\n"
            "- items는 5~8개. text는 18자 안팎(최대 26자), detail은 20자 안팎의 보충 설명(없으면 빈 문자열).\n"
            "- 각 항목은 서로 겹치지 않게, 실제로 바로 써먹을 수 있는 내용으로."
        )
    seconds = int(series.get("target_seconds", 40))
    chars = int((seconds - 2) * episode_mod.CHARS_PER_SECOND * speed)
    return (
        "[형식: 내레이션 영상]\n"
        f"- 장면 4~7개, 전체 길이 약 {seconds}초. 내레이션 전체 글자 수(공백 제외)는 {chars - 30}~{chars + 20}자.\n"
        "- 장면 하나의 내레이션은 공백 제외 60자를 넘기지 않습니다."
    )


def _text_of(response) -> str:
    return "\n".join(block.text for block in response.content if getattr(block, "type", "") == "text").strip()


def _cited_sources(response) -> list[dict]:
    seen, sources = set(), []
    for block in response.content:
        for citation in getattr(block, "citations", None) or []:
            url = getattr(citation, "url", None)
            if url and url not in seen:
                seen.add(url)
                sources.append({"title": getattr(citation, "title", "") or "", "url": url})
    return sources


class Writer:
    def __init__(self, cfg: dict, client=None):
        self.cfg = cfg
        if client is None:
            import anthropic

            client = anthropic.Anthropic()
        self.client = client
        self.usage = {"input_tokens": 0, "output_tokens": 0, "web_search_requests": 0, "calls": 0}

    def _create(self, *, system: str, messages: list, max_tokens: int = 16000, output_format: dict | None = None, tools: list | None = None):
        output_config = {"effort": self.cfg["writer"]["effort"]}
        if output_format:
            output_config["format"] = {"type": "json_schema", "schema": output_format}
        kwargs = {
            "model": self.cfg["writer"]["model"],
            "max_tokens": max_tokens,
            "system": system,
            "messages": messages,
            "output_config": output_config,
            "betas": [FALLBACK_BETA],
            "fallbacks": "default",
            "cache_control": {"type": "ephemeral"},
        }
        if tools:
            kwargs["tools"] = tools
        response = self.client.beta.messages.create(**kwargs)
        self._track(response)
        if response.stop_reason == "refusal":
            details = getattr(response, "stop_details", None)
            raise WriterError(f"모델이 요청을 거절했습니다: {getattr(details, 'category', '')} {getattr(details, 'explanation', '')}".strip())
        if response.stop_reason == "max_tokens":
            raise WriterError("응답이 max_tokens에서 잘렸습니다.")
        return response

    def _track(self, response) -> None:
        usage = getattr(response, "usage", None)
        self.usage["calls"] += 1
        if usage is None:
            return
        self.usage["input_tokens"] += (getattr(usage, "input_tokens", 0) or 0) + (getattr(usage, "cache_read_input_tokens", 0) or 0) + (
            getattr(usage, "cache_creation_input_tokens", 0) or 0
        )
        self.usage["output_tokens"] += getattr(usage, "output_tokens", 0) or 0
        server = getattr(usage, "server_tool_use", None)
        self.usage["web_search_requests"] += getattr(server, "web_search_requests", 0) or 0

    # ---------- 1단계: 사실 확인 ----------
    def research(self, topic: dict, today: date) -> tuple[str, list[dict]]:
        prompt = f"""다음 쇼츠 주제의 사실관계를 웹 검색으로 확인해 주세요.

주제: {topic['title']}
방향: {topic.get('angle', '')}
메모: {topic.get('notes', '')}
오늘 날짜: {today.isoformat()}

정리 형식:
- 핵심 사실 5~8개. 사실마다 (내용 / 기준일·시행일 / 출처 기관명과 URL)
- 최근 1~2년 사이 바뀐 제도가 있으면 바뀐 날짜와 함께 강조
- 출처끼리 다르거나 확인되지 않는 내용은 '불확실'로 표시
정부·공공기관·법령 같은 공식 출처를 우선하고, 개인 블로그는 근거로 쓰지 마세요. 대본은 쓰지 말고 사실만 정리하세요."""
        tool = {"type": "web_search_20260209", "name": "web_search", "max_uses": int(self.cfg["writer"]["max_searches"])}
        messages = [{"role": "user", "content": prompt}]
        system = "당신은 한국 생활경제·부동산 제도의 사실을 확인하는 꼼꼼한 리서처입니다. 확인한 내용만 출처와 함께 정리합니다."
        response = self._create(system=system, messages=messages, tools=[tool])
        for _ in range(3):  # 서버 도구 루프가 길면 pause_turn으로 멈춘다 → 이어서 계속
            if response.stop_reason != "pause_turn":
                break
            messages = messages + [{"role": "assistant", "content": response.content}]
            response = self._create(system=system, messages=messages, tools=[tool])
        notes = _text_of(response)
        if not notes:
            raise WriterError("사실 확인 결과가 비어 있습니다.")
        return notes, _cited_sources(response)

    # ---------- 2단계: 대본 ----------
    def write(self, *, topic: dict, series_name: str, today: date, material: str, attempts: int = 2) -> dict:
        series = self.cfg["series"][series_name]
        fmt = series["format"]
        schema = LIST_SCHEMA if fmt == "list" else NARRATED_SCHEMA
        speed = self.cfg["voice"]["speed"]
        base = f"""[시리즈] {series['label']}
[주제] {topic['title']}
[방향] {topic.get('angle', '')}

{_format_rules(series, speed)}

[자료]
{material.strip() or '(별도 자료 없음 — 널리 알려진 일반 원칙만 쓰고, 구체적인 숫자·날짜는 넣지 마세요.)'}"""
        system = system_prompt(self.cfg, today)
        feedback = ""
        last_errors: list[str] = []
        for _ in range(attempts):
            content = base + (f"\n\n[이전 초안의 문제 — 고쳐서 다시 쓰세요]\n{feedback}" if feedback else "")
            response = self._create(system=system, messages=[{"role": "user", "content": content}], output_format=schema)
            try:
                script = json.loads(_text_of(response))
            except ValueError as e:
                raise WriterError(f"대본 JSON을 읽지 못했습니다: {e}") from None
            script = _clean(script)
            errors, _warnings = episode_mod.validate(script, fmt, speed=speed)
            if not errors:
                return script
            last_errors = errors
            feedback = "\n".join(f"- {e}" for e in errors)
        raise WriterError("대본 검증 실패: " + "; ".join(last_errors))

    # ---------- 주제 추천 ----------
    def suggest_topics(self, *, today: date, count: int, used_titles: list[str], popular_titles: list[str]) -> list[dict]:
        series = {k: v for k, v in self.cfg["series"].items() if not v.get("source")}
        lines = "\n".join(f"- {name}: {spec['label']} ({'리스트형' if spec['format'] == 'list' else '내레이션형'})" for name, spec in series.items())
        prompt = f"""채널에 올릴 새 쇼츠 주제 {count}개를 제안해 주세요.

[시리즈]
{lines}

[이미 다룬 주제 — 겹치지 않게]
{chr(10).join('- ' + t for t in used_titles[-80:]) or '- (없음)'}

[최근 조회수가 좋았던 영상 — 이런 방향을 더]
{chr(10).join('- ' + t for t in popular_titles[:10]) or '- (데이터 없음)'}

조건:
- 시청자가 실제로 검색하거나 궁금해할 생활경제·내 집 마련·세금·지원금·금융 생활 주제
- 시기성 있는 주제(연말정산, 신학기, 청약 일정 등)는 오늘 날짜 기준으로 1~2달 안에 맞는 것
- id는 영어 소문자와 하이픈으로 된 3~5단어 슬러그
- angle에는 첫 2초 훅 아이디어, notes에는 확인해야 할 사실을 적기"""
        response = self._create(
            system=system_prompt(self.cfg, today),
            messages=[{"role": "user", "content": prompt}],
            output_format=topics_schema(list(series)),
        )
        topics = json.loads(_text_of(response)).get("topics", [])
        result = []
        for t in topics:
            slug = re.sub(r"[^a-z0-9-]+", "-", str(t.get("id", "")).lower()).strip("-")[:50]
            if slug and t.get("series") in series and str(t.get("title", "")).strip():
                result.append({**t, "id": slug, "used_on": None, "origin": "ai"})
        return result


def _clean(script: dict) -> dict:
    """모델 출력의 사소한 형식 문제를 정리한다 (앞뒤 공백, # 없는 해시태그)."""
    script["hashtags"] = [t if t.startswith("#") else f"#{t}" for t in (s.strip().replace(" ", "") for s in script.get("hashtags", [])) if t]
    for scene in script.get("scenes", []):
        scene["narration"] = scene["narration"].strip()
        scene["headline"] = scene["headline"].strip()
        scene["visual"]["items"] = [i.strip() for i in scene["visual"].get("items", []) if i.strip()]
    for item in script.get("items", []):
        item["text"] = item["text"].strip()
        item["detail"] = item["detail"].strip()
    script["title"] = script.get("title", "").strip()
    return script
