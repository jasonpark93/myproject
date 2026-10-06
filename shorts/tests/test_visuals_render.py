import math
import struct
import subprocess
import wave

import pytest

from conftest import needs_ffmpeg
from studio import render, textutil, tts, visuals


def _has_color(img, rgb, tolerance=10):
    raw = img.convert("RGBA").tobytes()
    return any(
        abs(raw[i] - rgb[0]) <= tolerance and abs(raw[i + 1] - rgb[1]) <= tolerance and abs(raw[i + 2] - rgb[2]) <= tolerance and raw[i + 3] > 200
        for i in range(0, len(raw), 4)
    )


def test_scene_card_and_subtitle(cfg):
    card = visuals.scene_card(
        brand=cfg["brand"], channel="돈한입", series_label="1분 돈 상식", headline="청약 가점 계산법",
        visual={"type": "number", "value": "84점", "label": "만점", "items": []}, progress=0.5, dim=False,
    )
    assert card.size == (1080, 1920) and card.mode == "RGBA"
    assert card.crop((80, 330, 940, 600)).getbbox() is not None  # 제목 영역에 글자가 있다
    accent = visuals.hex_rgb(cfg["brand"]["accent"])
    assert _has_color(card.crop((80, 500, 940, 1000)), accent)  # 큰 숫자는 강조색

    sub = visuals.subtitle(textutil.chunk("청약 가점은 [[84점]] 만점")[0], cfg["brand"])
    assert _has_color(sub, accent)
    box = sub.getbbox()
    assert box and 1050 < box[1] < box[3] < 1450  # 화면 아래쪽 자막 영역


@pytest.mark.parametrize("kind, items", [("list", ["하나", "둘", "셋"]), ("check", ["가", "나"]), ("versus", ["왼쪽", "오른쪽"]), ("quote", []), ("title", [])])
def test_visual_types_render(cfg, kind, items):
    card = visuals.scene_card(
        brand=cfg["brand"], channel="채널", series_label="시리즈", headline="제목",
        visual={"type": kind, "value": "핵심 한 줄", "label": "", "items": items}, progress=1.0, dim=True,
    )
    assert card.size == (1080, 1920)


def test_fit_font_avoids_orphan_line():
    fnt, lines = visuals.fit_font("청약 넣기 전 꼭 확인할 것 TOP 7", 860, (124, 112, 100, 90), max_lines=3)
    assert len(lines[-1].replace(" ", "")) > 2


def test_brand_images(cfg):
    assert visuals.profile_image("돈한입", cfg["brand"]).size == (800, 800)
    assert visuals.banner_image("돈한입", "한 줄 소개", cfg["brand"]).size == (2560, 1440)


def test_list_durations(listed):
    assert render.list_durations(listed["script"]) == [1.8, 7.0, 4.0]


@needs_ffmpeg
def test_render_narrated(tmp_path, cfg, short_narrated):
    out = tmp_path / "out.mp4"
    result = render.Renderer(cfg, tts=tts.SilentTTS(), workdir=tmp_path / "work", bgm_dir=tmp_path / "nobgm").render(short_narrated, out)
    info = render.probe(out)
    assert (info["width"], info["height"], info["fps"], info["audio"]) == (1080, 1920, 30.0, True)
    assert abs(info["duration"] - result["duration"]) < 0.08
    assert result["cover"].exists() and result["scenes"] == 3


def _sine_wav(path, seconds=3.0, rate=48000):
    with wave.open(str(path), "wb") as w:
        w.setnchannels(1)
        w.setsampwidth(2)
        w.setframerate(rate)
        w.writeframes(b"".join(struct.pack("<h", int(8000 * math.sin(2 * math.pi * 440 * i / rate))) for i in range(int(seconds * rate))))


@needs_ffmpeg
def test_render_list_with_bgm(tmp_path, cfg, listed):
    bgm = tmp_path / "bgm"
    bgm.mkdir()
    _sine_wav(bgm / "track.wav")
    out = tmp_path / "list.mp4"
    result = render.Renderer(cfg, tts=tts.SilentTTS(), workdir=tmp_path / "work", bgm_dir=bgm).render(listed, out)
    info = render.probe(out)
    assert abs(info["duration"] - 12.8) < 0.08 and result["duration"] == 12.8
    # 배경음악이 실제로 섞였는지: 소리가 0이 아닌 구간이 있어야 한다
    level = subprocess.run(
        ["ffmpeg", "-hide_banner", "-i", str(out), "-af", "volumedetect", "-f", "null", "-"], capture_output=True, text=True
    ).stderr
    mean = float(level.split("mean_volume:")[1].split("dB")[0])
    assert mean > -60


class FakePexels:
    def __init__(self, clip):
        self.clip = clip
        self.queries = []

    def find(self, query, *, seconds, seed, exclude):
        self.queries.append(query)
        return (len(self.queries), self.clip)


@needs_ffmpeg
def test_render_with_broll_clip(tmp_path, cfg, short_narrated):
    clip = tmp_path / "clip.mp4"
    subprocess.run(
        ["ffmpeg", "-y", "-hide_banner", "-loglevel", "error", "-f", "lavfi", "-i", "testsrc2=size=720x1280:rate=25:duration=2", "-pix_fmt", "yuv420p", str(clip)],
        check=True,
    )
    pexels = FakePexels(clip)
    out = tmp_path / "broll.mp4"
    render.Renderer(cfg, tts=tts.SilentTTS(), workdir=tmp_path / "work", pexels=pexels, bgm_dir=tmp_path / "x").render(short_narrated, out)
    assert pexels.queries[:2] == ["apartment", "calculator"]  # 대본의 broll 검색어 사용
    assert render.probe(out)["height"] == 1920


@needs_ffmpeg
def test_ffmpeg_failure_is_reported(tmp_path, cfg):
    renderer = render.Renderer(cfg, tts=tts.SilentTTS(), workdir=tmp_path / "work", bgm_dir=tmp_path / "x")
    with pytest.raises(render.RenderError, match="ffmpeg 실패"):
        renderer._run(["ffmpeg", "-hide_banner", "-i", str(tmp_path / "missing.mp4"), str(tmp_path / "x.mp4")])
