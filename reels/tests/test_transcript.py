import json

from conftest import voice

from editor import transcript
from editor.audio import Envelope

SRT = """1
00:00:01,000 --> 00:00:02,500
청약통장 하나로 3천만 원

2
00:00:03,000 --> 00:00:04,000
아끼는 법
"""


def test_parse_srt_and_vtt():
    cues = transcript.parse_srt(SRT)
    assert cues == [(1.0, 2.5, "청약통장 하나로 3천만 원"), (3.0, 4.0, "아끼는 법")]
    vtt = "WEBVTT\n\n00:00:01.000 --> 00:00:02.000\n<b>안녕</b>하세요\n"
    assert transcript.parse_srt(vtt) == [(1.0, 2.0, "안녕하세요")]


def test_words_from_srt_follow_speech(tmp_path):
    path = tmp_path / "a.srt"
    path.write_text(SRT, encoding="utf-8")
    env = Envelope.from_samples(voice([{"s": 1.2, "e": 2.3}, {"s": 3.1, "e": 3.8}], 5.0))
    words = transcript.words_from_srt(path, env)
    assert [w["w"] for w in words] == ["청약통장", "하나로", "3천만", "원", "아끼는", "법"]
    assert words[0]["s"] >= 1.15 and words[3]["e"] <= 2.35  # 소리 있는 곳 안에서만 나눈다
    assert all(a["e"] <= b["s"] + 1e-6 for a, b in zip(words, words[1:]))


def test_load_words_formats(tmp_path):
    path = tmp_path / "w.json"
    path.write_text(json.dumps({"words": [{"word": " 안녕", "start": 0.5, "end": 0.9}, {"text": "하세요", "start": 0.85, "end": 1.2}]}), encoding="utf-8")
    words = transcript.load_words(path)
    assert [w["w"] for w in words] == ["안녕", "하세요"]
    assert words[0]["e"] <= words[1]["s"]


def test_norm():
    assert transcript.norm("청약 통장은, 3천만!") == "청약통장은3천만"


def test_transcribe_with_fake_whisper(monkeypatch):
    """faster-whisper 결과(단어 앞 공백, 확률 포함)를 단어 시간표로 바꾸는지."""
    import sys
    import types

    import numpy as np

    class Word:
        def __init__(self, word, start, end):
            self.word, self.start, self.end, self.probability = word, start, end, 0.9

    class Seg:
        def __init__(self, words):
            self.words = words

    seen = {}

    class FakeModel:
        def __init__(self, name, device, compute_type):
            seen["init"] = (name, device, compute_type)

        def transcribe(self, audio, **kw):
            seen["kw"] = kw
            segs = [Seg([Word(" 음,", 0.2, 0.5), Word(" 청약통장", 0.7, 1.2)]), Seg([Word(" 만드세요.", 1.3, 1.9), Word(" ", 2.0, 2.1)])]
            return iter(segs), types.SimpleNamespace(language="ko")

    monkeypatch.setitem(sys.modules, "faster_whisper", types.SimpleNamespace(WhisperModel=FakeModel))
    words, meta = transcript.transcribe(np.zeros(16000, np.float32), "small", hotwords="청약통장", log=lambda m: None)
    assert [w["w"] for w in words] == ["음,", "청약통장", "만드세요."]
    assert seen["kw"]["word_timestamps"] is True and seen["kw"]["language"] == "ko"
    assert seen["kw"]["condition_on_previous_text"] is False
    assert seen["kw"]["hotwords"] == "청약통장"
    assert meta["engine"] == "faster-whisper small"
