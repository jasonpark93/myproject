from conftest import W

from editor import captions


def tokens(text):
    return [(t, captions.display(t)) for t in text.split()]


def test_chunks_stay_within_limit_and_break_at_meaning():
    toks = tokens("청약통장은 만 19세부터 은행에서 누구나 쉽게 만들 수 있는데요, 생각보다 모르는 분이 많아요.")
    chunks = captions.chunk(toks, 12)
    lines = [" ".join(captions.display(toks[i][0]) for i in range(a, b + 1)) for a, b in chunks]
    assert all(captions.visible_len(line) <= 12 for line in lines)
    assert "있는데요" in lines[[i for i, ln in enumerate(lines) if "있는데요" in ln][0]].split()[-1]


def test_number_and_unit_stay_together():
    toks = tokens("무려 3천만 원 아끼는 방법")
    chunks = captions.chunk(toks, 6)
    lines = [" ".join(toks[i][0] for i in range(a, b + 1)) for a, b in chunks]
    assert any("3천만 원" in line for line in lines)


def test_display_drops_periods_keeps_question():
    assert captions.display("정말요?") == "정말요?"
    assert captions.display("끝나요.") == "끝나요"
    assert captions.display("그런데... 음, 좋아요") == "그런데 음 좋아요"


def test_highlights():
    assert captions.find_highlights("통장 가입하고 2년이") == ["2년"]
    assert captions.find_highlights("지나야 1순위가 돼요") == ["1순위"]
    assert captions.find_highlights("세 가지만 기억하세요") == ["세 가지"]
    assert captions.find_highlights("이건 무조건 하세요") == ["무조건"]
    assert captions.find_highlights("클로드로 편집", ["클로드"]) == ["클로드"]
    assert captions.find_highlights("그냥 평범한 문장") == []


def test_runs_split_by_highlight():
    assert captions.runs("3천만 원 아끼는 법", ["3천만 원"]) == [("3천만 원", True), (" 아끼는 법", False)]


def test_auto_captions_skip_cut_words():
    words = W([("음", 0.0, 0.3), ("오늘은", 0.5, 0.9), ("청약", 0.95, 1.2), ("얘기", 1.25, 1.5)])
    words[0]["cut"] = "filler"
    caps = captions.auto_captions(words, 0, 3, 12)
    assert caps == [{"from": 1, "to": 3, "text": "오늘은 청약 얘기", "hl": []}]
