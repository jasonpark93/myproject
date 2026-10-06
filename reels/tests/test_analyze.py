from conftest import W

from editor import analyze


def kept(words):
    return " ".join(w["w"] for w in words if not w.get("cut"))


def test_fillers_and_soft_fillers():
    words = W([("음", 0.0, 0.4), ("그", 0.6, 0.7), ("사람이", 0.75, 1.1), ("그", 1.5, 1.9), ("어...", 2.3, 2.6), ("좋아요", 2.9, 3.3)])
    analyze.mark_fillers(words)
    # '그 사람이'의 그(바로 뒤에 말이 붙음)는 지시어라 남기고, 길게 끈 '그'는 군말
    assert [w.get("cut") for w in words] == ["filler", None, None, "filler", "filler", None]


def test_stutters():
    words = W([("그래서", 0.0, 0.3), ("그래서", 0.4, 0.7), ("청", 0.8, 0.9), ("청약통장은", 0.95, 1.5), ("진짜", 1.6, 1.8), ("진짜", 1.85, 2.1), ("이번", 2.3, 2.5), ("달에", 2.55, 2.8), ("이번", 2.9, 3.1), ("달에", 3.15, 3.4), ("가요", 3.5, 3.8)])
    analyze.mark_stutters(words)
    assert kept(words) == "그래서 청약통장은 진짜 진짜 이번 달에 가요"


def run_restart(spec):
    words = W(spec)
    analyze.mark_restarts(words)
    return kept(words)


def test_ng_keeps_last_take():
    assert run_restart([
        ("이번", 3.4, 3.6), ("달에", 3.62, 3.9), ("청약", 3.95, 4.3), ("신청하는", 4.35, 4.8), ("방법을", 4.85, 5.2),
        ("이번", 7.3, 7.5), ("달", 7.52, 7.7), ("청약", 7.75, 8.1), ("신청", 8.15, 8.5), ("방법을", 8.55, 8.9), ("알려드릴게요.", 8.95, 9.8),
    ]) == "이번 달 청약 신청 방법을 알려드릴게요."


def test_ng_chain_of_fragments():
    assert run_restart([
        ("청약통장은", 1.0, 1.6), ("만", 1.65, 1.8), ("19세부", 1.85, 2.3),
        ("청약통장은", 4.4, 5.0),
        ("청약통장은", 7.2, 7.8), ("만", 7.85, 8.0), ("19세부터", 8.05, 8.6), ("가입할", 8.65, 9.0), ("수", 9.02, 9.1), ("있어요.", 9.15, 9.6),
    ]) == "청약통장은 만 19세부터 가입할 수 있어요."


def test_ng_restart_from_middle_keeps_earlier_sentence():
    assert run_restart([
        ("안녕하세요.", 0.3, 1.0), ("오늘은", 1.3, 1.7), ("청약", 1.75, 2.1), ("이야기를", 2.15, 2.6), ("할", 2.65, 2.8),
        ("오늘은", 4.9, 5.3), ("청약", 5.35, 5.7), ("이야기를", 5.75, 6.2), ("해볼게요.", 6.25, 6.9),
    ]) == "안녕하세요. 오늘은 청약 이야기를 해볼게요."


def test_no_ng_for_new_sentence_with_same_topic():
    text = "청약통장은 꼭 만드세요. 청약통장은 은행에서 만들 수 있어요."
    assert run_restart([
        ("청약통장은", 11.75, 12.4), ("꼭", 12.45, 12.6), ("만드세요.", 12.65, 13.2),
        ("청약통장은", 15.4, 16.0), ("은행에서", 16.05, 16.6), ("만들", 16.65, 16.9), ("수", 16.92, 17.0), ("있어요.", 17.05, 17.5),
    ]) == text
    assert run_restart([("그래서", 0.3, 0.7), ("청약통장이", 0.75, 1.4), ("필요해요.", 1.45, 2.0), ("그래서요", 3.5, 4.0)]) == "그래서 청약통장이 필요해요. 그래서요"


def test_script_fixes_typos_and_cuts_adlibs():
    script, keywords = analyze.parse_script("[[청약통장]] 하나로 [[3천만 원]] 아끼는 법.\n# 메모는 무시\n클로드로 편집하면 끝나요.")
    assert keywords == ["청약통장", "3천만 원"]
    words = W([("청약", 0.5, 0.8), ("통장", 0.82, 1.0), ("하나로", 1.05, 1.4), ("삼천만", 1.5, 1.9), ("원", 1.92, 2.05), ("아끼는", 2.1, 2.5), ("법.", 2.55, 2.8),
               ("자", 3.0, 3.2), ("여러분", 3.25, 3.7), ("클라우드로", 4.2, 4.8), ("편집하면", 4.85, 5.3), ("끝나요.", 5.35, 5.8)])
    stats = analyze.align_script(words, script)
    assert stats["offscript"] == 2
    assert kept(words) == "청약 통장 하나로 3천만 원 아끼는 법. 클로드로 편집하면 끝나요."
    assert words[3]["asr"] == "삼천만"


def test_sentences_and_roles():
    words = W([("돈", 0.0, 0.2), ("버는", 0.25, 0.5), ("법.", 0.55, 0.8), ("근데", 1.2, 1.4), ("문제는", 1.45, 1.8), ("시간이에요", 1.85, 2.4),
               ("하루", 2.8, 3.0), ("15분이면", 3.05, 3.5), ("돼요", 3.55, 3.9), ("팔로우하세요.", 4.4, 5.0)])
    spans = analyze.split_sentences(words)
    texts = [analyze.sentence_text(words, a, b) for a, b in spans]
    assert texts == ["돈 버는 법.", "근데 문제는 시간이에요", "하루 15분이면 돼요", "팔로우하세요."]
    roles = [analyze.guess_role(t, i, len(texts)) for i, t in enumerate(texts)]
    assert roles == ["hook", "twist", "number", "cta"]
