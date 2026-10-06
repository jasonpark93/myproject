"""모션그래픽 모드: 대본·장면 시간, 카드 그리기, 컴퓨터 음성/녹음으로 처음부터 끝까지."""

import json
import subprocess

import numpy as np
import pytest
from conftest import W, needs_ffmpeg, voice

from editor import SR, cards, story

SPEC = {
    "name": "t",
    "header": {"label": "요즘 난리난", "title": "청약 꿀팁"},
    "scenes": [
        {"say": "청약통장 [[하나로]] 3천만 원 아끼는 법.", "card": {"type": "bigtext", "lines": ["청약통장", "[[3천만 원]]"], "at": ["청약통장", "3천만"]}},
        {"say": "줌인, 효과음, 자막까지 넣어 줍니다.", "card": {"type": "checklist", "title": "알아서", "items": [{"text": "줌인", "at": "줌인"}, {"text": "효과음", "at": "효과음"}, {"text": "자막", "at": "자막"}]}},
    ],
}


class FakeTiming:
    duration = 3.0

    def at(self, phrase, default):
        return default


def fake_narration():
    script, ranges, _ = story.script_of(SPEC)
    words, t = [], 0.2
    for m in __import__("re").finditer(r"\S+", script):
        words.append({"w": m.group(0), "s": round(t, 3), "e": round(t + 0.3, 3), "sp": [m.start(), m.end()]})
        t += 0.4
    return story.Narration(audio=np.zeros(int((t + 0.5) * SR), np.float32), words=words, source="test")


def test_script_of_and_keywords():
    script, ranges, keywords = story.script_of(SPEC)
    assert script.startswith("청약통장 하나로 3천만 원") and "[[" not in script
    assert keywords == ["하나로"]
    assert script[ranges[1][0] : ranges[1][1]] == "줌인, 효과음, 자막까지 넣어 줍니다."


def test_schedule_and_trigger_times():
    nar = fake_narration()
    timings = story.schedule(SPEC, nar)
    assert timings[0].start == 0.0
    second_first_word = next(w for w in nar.words if w["w"] == "줌인,")
    assert timings[1].start == pytest.approx(second_first_word["s"] - story.LEAD)
    # '효과음'이 나오는 순간 = 장면 시작 기준 시간
    t = timings[1].at("효과음", default=-1)
    word = next(w for w in nar.words if w["w"] == "효과음,")
    assert t == pytest.approx(word["s"] - timings[1].start)
    assert timings[1].at("없는 말", default=1.23) == 1.23


def test_caption_list_uses_script_text():
    nar = fake_narration()
    caps = story.caption_list(SPEC, nar, 12)
    texts = [c["text"] for c in caps]
    assert "".join(texts).replace(" ", "") == "청약통장하나로3천만원아끼는법줌인효과음자막까지넣어줍니다"
    assert caps[0]["show"][0] == 0.0
    assert any(c["hl"] for c in caps)


@pytest.mark.parametrize("spec", [
    {"type": "checklist", "title": "제목", "items": ["하나", {"text": "둘", "icon": "zoom"}]},
    {"type": "bigtext", "lines": ["방법도", "[[진짜 간단]]"], "pill": "딱 1단계"},
    {"type": "compare", "title": "시간", "bars": [{"label": "직접", "value": 1, "color": "red"}, {"label": "클로드", "value": 0.2, "color": "lime"}], "result": "확 줄어듦"},
    {"type": "compare", "bars": [{"label": "A", "value": 0.4}, {"label": "B", "value": 0.8}, {"label": "C", "value": 0.6}]},
    {"type": "calendar", "title": "매일", "check": 5},
    {"type": "comment", "keyword": "청약", "dm": "자료 보냈어요", "items": ["체크리스트"]},
    {"type": "waveform", "marks": [{"label": "틀린 부분"}, {"label": "공백"}], "cut_at": "x"},
    {"type": "steps", "title": "3단계", "steps": ["찍기", "넣기", "말하기"]},
    {"type": "stat", "label": "아끼는 돈", "value": 3000, "unit": "만 원"},
    {"type": "text", "title": "제목", "lines": ["한 줄", "두 줄"]},
])
def test_every_card_draws(spec):
    th = cards.theme()
    card = cards.make_card(spec, th, FakeTiming())
    sizes = set()
    for t in (0.0, 0.3, 1.0, 2.9):
        img = card.frame(t)
        assert img.w <= 1100 and img.h <= 1200  # 화면에 넣을 때 980x820 안으로 맞춰 줄인다
        assert np.isfinite(img.pm).all() and 0 <= img.a.min() and img.a.max() <= 1.0001
        sizes.add((img.w, img.h))
    assert len(sizes) == 1  # 애니메이션 중에도 카드 크기는 그대로


def test_image_card_and_overlays(tmp_path):
    from PIL import Image

    path = tmp_path / "shot.png"
    Image.new("RGB", (600, 900), (200, 50, 50)).save(path)
    th = cards.theme()
    card = cards.make_card({"type": "image", "path": "shot.png"}, th, FakeTiming(), base_dir=tmp_path)
    assert card.frame(1.0).h <= 760
    canvas = cards.Canvas(1080, 1200)
    cards.Pills([{"text": "컷 편집", "icon": "cut"}, {"text": "자막", "pos": "br"}], th, FakeTiming()).draw(canvas, (90, 200, 900, 600), 2.0)
    cards.Stamp({"text": "끝!", "at": None}, th, FakeTiming()).draw(canvas, (90, 200, 900, 600), 2.9)
    assert canvas.a.max() > 0.9
    for name in cards.ICONS:
        assert cards.icon(name, 48, th).w == 48


def test_unknown_card_type_is_clear_error():
    from editor.util import EditorError

    with pytest.raises(EditorError):
        cards.make_card({"type": "없는카드"}, cards.theme(), FakeTiming())


@needs_ffmpeg
def test_story_render_with_fake_voice(tmp_path, monkeypatch):
    from editor import motion, tts

    stories = tmp_path / "stories"
    stories.mkdir()
    (stories / "t.json").write_text(json.dumps(SPEC, ensure_ascii=False), encoding="utf-8")
    monkeypatch.setattr(story, "STORIES", stories)
    monkeypatch.setattr(motion, "OUT_DIR", tmp_path / "out")
    monkeypatch.setattr(motion, "WORK_DIR", tmp_path / "work")
    monkeypatch.setattr(motion, "BGM_DIR", tmp_path / "bgm")
    monkeypatch.setattr(tts, "available", lambda: ["fake"])

    def fake_speak(text, engine=None, voice_name=None):
        n = max(2, len(text.split()))
        ws = [{"s": 0.05 + i * 0.32, "e": 0.05 + i * 0.32 + 0.26} for i in range(n)]
        return voice(ws, 0.1 + n * 0.32)

    monkeypatch.setattr(tts, "speak", fake_speak)
    info = motion.render_story("t", use_tts=True, log=lambda m: None)
    out = tmp_path / "out" / "t.mp4"
    meta = json.loads(subprocess.run(["ffprobe", "-v", "error", "-show_streams", "-show_format", "-of", "json", str(out)], capture_output=True, check=True).stdout)
    video = next(s for s in meta["streams"] if s["codec_type"] == "video")
    assert (video["width"], video["height"]) == (1080, 1920)
    assert float(meta["format"]["duration"]) == pytest.approx(info["duration"], abs=0.15)
    assert info["scenes"] == 2 and info["captions"] >= 3
    assert abs(info["loudness"] - (-14.0)) < 1.5
    assert (tmp_path / "work" / "story-t" / "snapshots" / "scenes.jpg").exists()


@needs_ffmpeg
def test_recording_removes_ng_and_aligns(tmp_path, monkeypatch):
    from editor import transcript
    from editor.plan import DEFAULTS

    spoken = W([
        ("청약통장", 0.5, 1.0), ("하나로", 1.05, 1.4),  # NG: 2초 멈춤 뒤 처음부터 다시
        ("청약통장", 3.5, 4.0), ("하나로", 4.05, 4.4), ("삼천만", 4.5, 4.9), ("원", 4.92, 5.05), ("아끼는", 5.1, 5.5), ("법.", 5.55, 5.8),
        ("줌인,", 6.3, 6.6), ("효과음,", 6.65, 7.0), ("자막까지", 7.05, 7.5), ("넣어", 7.55, 7.8), ("줍니다.", 7.85, 8.3),
    ])
    path = tmp_path / "rec.wav"
    from editor.media import write_wav

    write_wav(path, voice(spoken, 9.0))
    monkeypatch.setattr(transcript, "transcribe", lambda *a, **k: ([dict(w) for w in spoken], {"engine": "fake"}))
    nar = story.from_recording(SPEC, path, tmp_path, dict(DEFAULTS), log=lambda m: None)
    assert nar.stats["ng"] == 2
    assert [w["w"] for w in nar.words][:3] == ["청약통장", "하나로", "3천만"]  # 대본 글자로 교정
    assert nar.duration < 7.0
