import pytest
from conftest import W, voice

from editor import FPS, timeline
from editor.audio import Envelope
from editor.plan import DEFAULTS

SETTINGS = {k: DEFAULTS[k] for k in ("gap", "silence", "lead", "tail")}


def build(words, duration, extra=(), **kw):
    env = Envelope.from_samples(voice(words, duration, extra))
    return timeline.build(words, env, SETTINGS, duration, **kw), env


def test_long_pause_is_cut_to_short_gap():
    words = W([("하나", 1.0, 1.5), ("둘", 1.6, 2.0), ("셋", 3.5, 4.0)])
    segs, _ = build(words, 5.0)
    assert len(segs) == 2
    tl = timeline.Timeline(segs)
    # 첫 말은 0.3초 안에 시작
    assert tl.to_out(words[0]["s"]) <= 0.3
    # 1.5초 쉼 → gap(0.15초) 안팎만 남는다
    gap_out = tl.to_out(words[2]["s"]) - tl.to_out(words[1]["e"])
    assert 0.05 <= gap_out <= 0.25
    assert tl.cuts(words, 5.0)[1]["reason"] == "silence"


def test_short_pause_is_kept():
    words = W([("하나", 1.0, 1.5), ("둘", 1.8, 2.2)])
    segs, _ = build(words, 3.0)
    assert len(segs) == 1


def test_cut_word_and_untranscribed_sound_are_removed():
    words = W([("하나", 1.0, 1.5), ("음", 1.6, 1.9), ("둘", 2.0, 2.4), ("셋", 2.75, 3.1)])
    words[1]["cut"] = "filler"
    # '둘'과 '셋' 사이 0.35초 틈에 인식 안 된 소리(0.12초)가 있다
    segs, _ = build(words, 4.0, extra=[(2.52, 2.64)])
    tl = timeline.Timeline(segs)
    reasons = [c["reason"] for c in tl.cuts(words, 4.0)]
    assert "filler" in reasons and "noise" in reasons
    # 잘린 소리 구간은 완성본에 들어가지 않는다
    assert all(not (s.a < 1.75 < s.b) for s in segs)
    assert all(not (s.a < 2.58 < s.b) for s in segs)


def test_segments_on_frame_grid_and_mapping_roundtrip():
    words = W([("하나", 0.73, 1.21), ("둘", 2.37, 2.88), ("셋", 4.11, 4.52)])
    segs, _ = build(words, 5.0)
    for s in segs:
        assert abs(s.a * FPS - round(s.a * FPS)) < 1e-6 and abs(s.b * FPS - round(s.b * FPS)) < 1e-6
    tl = timeline.Timeline(segs)
    for t in (0.5, 1.0, 1.5):
        assert tl.to_src(tl.to_out(segs[0].a + t * 0.1)) == pytest.approx(segs[0].a + t * 0.1, abs=1e-6)


def test_keep_and_drop_ranges():
    words = W([("하나", 1.0, 1.5), ("둘", 3.0, 3.5), ("셋", 3.6, 4.0)])
    segs, _ = build(words, 5.0)
    base = timeline.Timeline(segs).duration
    kept, _ = build(W([("하나", 1.0, 1.5), ("둘", 3.0, 3.5), ("셋", 3.6, 4.0)]), 5.0, keep_ranges=[[1.5, 3.0]])
    assert timeline.Timeline(kept).duration > base + 1.0
    dropped_words = W([("하나", 1.0, 1.5), ("둘", 3.0, 3.5), ("셋", 3.6, 4.0)])
    dropped, _ = build(dropped_words, 5.0, drop_ranges=[[2.9, 3.55]])
    assert dropped_words[1]["cut"] == "manual"
    assert timeline.Timeline(dropped).duration < base - 0.3


def test_hidden_silence_inside_word_timing_is_cut():
    # 인식기가 단어 끝을 침묵 위로 늘려 잡은 경우(소리는 1.0~1.4초뿐)
    words = W([("하나", 1.0, 2.6), ("둘", 2.7, 3.1)])
    env = Envelope.from_samples(voice([{"s": 1.0, "e": 1.4}, {"s": 2.7, "e": 3.1}], 4.0))
    segs = timeline.build(words, env, SETTINGS, 4.0)
    assert timeline.Timeline(segs).duration < 4.0 - 1.0
