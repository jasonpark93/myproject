import copy

import pytest

from studio import config, episode, textutil


def test_strip_marks():
    assert textutil.strip_marks("청약통장 [[월 25만원]]까지") == "청약통장 월 25만원까지"


def test_words_keep_partial_highlight():
    assert textutil.words("청약통장 [[월 25만원]]까지 넣기") == [
        [("청약통장", False)],
        [("월", True)],
        [("25만원", True), ("까지", False)],
        [("넣기", False)],
    ]


def test_chunks_respect_length_and_punctuation():
    text = "무주택기간은 1년 미만이 2점이고, 1년마다 2점씩 올라 15년 이상이면 [[32점]]입니다."
    chunks = textutil.chunk(text, max_chars=14)
    assert all(len(textutil.plain(c)) <= 14 for c in chunks)
    assert textutil.plain(chunks[-1]).endswith("32점입니다")  # 마지막 마침표는 지운다
    assert ("32점", True) in [seg for c in chunks for w in c for seg in w]


def test_chunk_breaks_after_question():
    chunks = textutil.chunk("청약 가점 아세요? 모르면 계산해 보세요.")
    assert textutil.plain(chunks[0]) == "청약 가점 아세요?"


def test_long_single_word_gets_its_own_chunk():
    chunks = textutil.chunk("가나다라마바사아자차카타파하가나다 끝", max_chars=8)
    assert textutil.plain(chunks[0]) == "가나다라마바사아자차카타파하가나다"


def test_fixtures_are_valid(cfg, narrated, listed):
    assert episode.validate(narrated["script"], "narrated")[0] == []
    assert episode.validate(listed["script"], "list")[0] == []


@pytest.mark.parametrize(
    "mutate, message",
    [
        (lambda s: s["scenes"].__delitem__(slice(1, None)), "장면 수"),
        (lambda s: s["scenes"][0].update(headline="가" * 23), "headline이 너무"),
        (lambda s: s["scenes"][0].update(narration="[[강조 안 닫힘"), "괄호 짝"),
        (lambda s: s["scenes"][3]["visual"].update(items=["하나"]), "versus"),
        (lambda s: s["scenes"][0]["visual"].update(value=""), "value"),
        (lambda s: s.update(hashtags=["청약"]), "#으로 시작"),
        (lambda s: s.update(title=""), "title"),
        (lambda s: s["scenes"][0].update(narration="가" * 500), "예상 길이"),
    ],
)
def test_validation_errors(narrated, mutate, message):
    script = copy.deepcopy(narrated["script"])
    mutate(script)
    errors, _ = episode.validate(script, "narrated")
    assert any(message in e for e in errors), errors


def test_list_validation(listed):
    script = copy.deepcopy(listed["script"])
    script["items"] = script["items"][:2]
    assert any("3~10개" in e for e in episode.validate(script, "list")[0])
    script = copy.deepcopy(listed["script"])
    script["items"][0]["text"] = "가" * 27
    assert any("너무 깁니다" in e for e in episode.validate(script, "list")[0])


def test_estimate_seconds(narrated):
    slow = episode.estimate_seconds(narrated["script"], "narrated", 1.0)
    fast = episode.estimate_seconds(narrated["script"], "narrated", 1.2)
    assert 10 < fast < slow < 30


def test_content_hash_tracks_script_and_voice(cfg, narrated):
    base = episode.content_hash(narrated, cfg)
    changed = copy.deepcopy(narrated)
    changed["script"]["scenes"][0]["narration"] += " 추가"
    assert episode.content_hash(changed, cfg) != base
    other_voice = copy.deepcopy(cfg)
    other_voice["voice"]["google_voice"] = "ko-KR-Chirp3-HD-Kore"
    assert episode.content_hash(narrated, other_voice) != base
    assert episode.content_hash(copy.deepcopy(narrated), cfg) == base


def test_make_id():
    assert episode.make_id("2026-10-13T19:00:00+09:00", "explainer", "Tongjang 25 만원!") == "2026-10-13-explainer-tongjang-25"
    assert episode.make_id("2026-10-13T19:00:00+09:00", "topn", "청약") == "2026-10-13-topn-ep"


def test_description(cfg, narrated):
    cfg["broll"] = "pexels"
    cfg["channel"]["links"] = [{"label": "청약 일정", "url": "https://example.com"}]
    text = episode.description(narrated, cfg)
    assert "📚 참고 자료" in text and "https://www.law.go.kr/" in text
    assert "🔗 청약 일정: https://example.com" in text
    assert "배경 영상: Pexels" in text
    assert "정보 제공용" in text
    assert text.endswith("#청약 #청약가점 #내집마련")


def test_config_validation(cfg):
    bad = copy.deepcopy(cfg)
    bad["brand"]["accent"] = "yellow"
    with pytest.raises(config.ConfigError):
        config.validate(bad)
    bad = copy.deepcopy(cfg)
    bad["schedule"]["days"]["mon"] = "없는시리즈"
    with pytest.raises(config.ConfigError):
        config.validate(bad)
    bad = copy.deepcopy(cfg)
    bad["voice"]["speed"] = 2
    with pytest.raises(config.ConfigError):
        config.validate(bad)
