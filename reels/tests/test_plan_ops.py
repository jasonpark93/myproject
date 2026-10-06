import pytest
from conftest import W, voice

from editor import plan as P
from editor.audio import Envelope
from editor.media import Probe
from editor.util import EditorError

SPEC = [
    ("청약통장", 1.0, 1.5), ("하나로", 1.55, 1.9), ("3천만", 2.0, 2.4), ("원", 2.42, 2.55), ("아끼는", 2.6, 3.0), ("법.", 3.05, 3.3),
    ("이번", 4.0, 4.2), ("달에", 4.22, 4.5), ("청약", 4.55, 4.9),
    ("이번", 7.0, 7.2), ("달", 7.22, 7.4), ("청약", 7.45, 7.8), ("방법을", 7.85, 8.2), ("알려드릴게요.", 8.25, 9.0),
    ("음", 9.3, 9.6), ("클라우드로", 9.9, 10.5), ("편집해요.", 10.55, 11.2),
]


def probe(duration=12.0):
    return Probe(path="/tmp/x.mp4", duration=duration, width=1080, height=1920, fps=30.0, rotation=0, has_audio=True,
                 video_start=0.0, audio_start=0.0, color_transfer="", vcodec="h264")


@pytest.fixture
def setup():
    words = W(SPEC)
    env = Envelope.from_samples(voice(words, 12.0))
    plan = P.build("t", probe(), words, {"engine": "test"})
    return plan, env


def test_build_marks_ng_filler_and_roles(setup):
    plan, env = setup
    res = P.resolve(plan, env)
    texts = [s["text"] for s in res.sentences]
    assert texts == ["청약통장 하나로 3천만 원 아끼는 법.", "이번 달 청약 방법을 알려드릴게요.", "클라우드로 편집해요."]
    assert [s["role"] for s in res.sentences] == ["hook", "explain", "conclusion"]
    reasons = {c["reason"] for c in res.cuts}
    assert {"ng", "filler"} <= reasons
    assert res.captions[0]["show"][0] == 0.0  # 첫 화면부터 자막
    assert any(c["hl"] == ["3천만 원"] for c in res.captions)


def test_set_value_coercions(setup):
    plan, _ = setup
    assert P.set_value(plan, "caption.size", "120%") == (1.0, 1.2)
    assert P.set_value(plan, "zoom.max_punch", "3") == (None, 3)
    assert P.set_value(plan, "강조색", "민트")[1] == "민트"
    assert P.set_value(plan, "zoom.mask_cuts", "끄기")[1] is False
    assert P.set_value(plan, "title.text", "첫 줄\\n둘째 줄")[1] == "첫 줄\n둘째 줄"
    with pytest.raises(EditorError):
        P.set_value(plan, "caption.highlight", "무지개")
    with pytest.raises(EditorError):
        P.set_value(plan, "없는.설정", "1")


def test_fix_text_updates_captions(setup):
    plan, env = setup
    assert P.fix_text(plan, "클라우드", "클로드") >= 1
    res = P.resolve(plan, env)
    assert any("클로드로" in c["text"] for c in res.captions)
    assert not any("클라우드" in c["text"] for c in res.captions)


def test_restore_cut_by_id_and_time(setup):
    plan, env = setup
    res = P.resolve(plan, env)
    ng = next(i for i, c in enumerate(res.cuts) if c["reason"] == "ng")
    cut = P.restore(plan, res, f"C{ng + 1}")
    res2 = P.resolve(plan, env)
    assert res2.duration > res.duration + 0.5
    assert "이번 달에 청약" in " ".join(s["text"] for s in res2.sentences)
    assert cut["reason"] == "ng"
    with pytest.raises(EditorError):
        P.restore(plan, res2, "C99")


def test_drop_range_by_output_time(setup):
    plan, env = setup
    res = P.resolve(plan, env)
    P.drop_range(plan, res, "0:00-0:01")
    res2 = P.resolve(plan, env)
    assert res2.duration < res.duration - 0.5
