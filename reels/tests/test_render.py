"""실제 ffmpeg로 짧은 영상을 만들어 처음부터 끝까지 돌려 본다."""

import json
import subprocess

import pytest
from conftest import W, make_video, needs_ffmpeg

from editor import cli, plan as P
from editor.audio import measure_loudness
from editor.util import read_json

SPEC = [
    ("오늘은", 1.0, 1.5), ("3가지만", 1.55, 2.1), ("기억하세요.", 2.15, 2.8),
    ("음", 3.1, 3.4),
    ("첫째", 4.6, 5.0), ("통장부터", 5.05, 5.6), ("만드세요.", 5.65, 6.3),
    ("팔로우하세요.", 6.6, 7.4),
]


def info(path):
    out = subprocess.run(["ffprobe", "-v", "error", "-show_streams", "-show_format", "-of", "json", str(path)], capture_output=True, check=True).stdout
    return json.loads(out)


@needs_ffmpeg
@pytest.mark.parametrize("size", [(1080, 1920), (1280, 720)])
def test_edit_end_to_end(tmp_path, workdirs, size, capsys):
    words = W(SPEC)
    src = tmp_path / "raw.mp4"
    make_video(src, words, 8.5, size=size)
    words_file = tmp_path / "words.json"
    words_file.write_text(json.dumps(words, ensure_ascii=False), encoding="utf-8")

    work = cli.analyze(str(src), words_file=str(words_file))
    assert (work / "review.md").exists()
    result = cli.render(str(work))
    out = tmp_path / "out" / "raw.mp4"
    meta = info(out)
    video = next(s for s in meta["streams"] if s["codec_type"] == "video")
    audio = next(s for s in meta["streams"] if s["codec_type"] == "audio")
    assert (video["codec_name"], video["width"], video["height"], video["r_frame_rate"], video["pix_fmt"]) == ("h264", 1080, 1920, "30/1", "yuv420p")
    assert (audio["codec_name"], audio["sample_rate"]) == ("aac", "48000")
    assert float(meta["format"]["duration"]) == pytest.approx(result["duration"], abs=0.1)
    assert result["duration"] < 7.0  # 앞뒤 무음·'음'·긴 쉼이 빠졌다
    assert abs(measure_loudness(out) - (-14.0)) < 1.5
    for snap in result["snapshots"]:
        assert snap and __import__("pathlib").Path(snap).exists()
    report = (work / "report.md").read_text(encoding="utf-8")
    assert "자막 전문" in report and "3가지" in report

    # 효과음 볼륨만 바꾸면 영상은 다시 만들지 않는다
    cli.cmd_set(work.name, ["sfx.volume=0.5"], save=False)
    capsys.readouterr()
    cli.render(work.name)
    log = capsys.readouterr().out
    assert "영상: 이전 결과 재사용" in log and "목소리: 이전 결과 재사용" in log


@needs_ffmpeg
def test_corrections_flow(tmp_path, workdirs):
    words = W(SPEC)
    src = tmp_path / "clip.mp4"
    make_video(src, words, 8.5)
    words_file = tmp_path / "words.json"
    words_file.write_text(json.dumps(words, ensure_ascii=False), encoding="utf-8")
    work = cli.analyze(str(src), words_file=str(words_file))
    cli.cmd_fix(work.name, "통장", "청약통장")
    cli.cmd_assign(work.name, "zoom", ["S1=none"])
    cli.cmd_assign(work.name, "sfx", ["S3=ding"])  # S2는 '음'만 있어 잘린 문장
    cli.cmd_set(work.name, ["title.text=[[3가지]] 기억", "gap=0.2"], save=True)
    plan = read_json(work / "plan.json")
    assert plan["sentences"][0]["zoom"] == "none"
    assert read_json(tmp_path / "style.json")["gap"] == 0.2
    from editor.audio import Envelope

    res = P.resolve(plan, Envelope.load(work / "envelope.npz"))
    assert any("청약통장부터" in c["text"] for c in res.captions)
    fixed = [e for e in res.sfx_events if e.get("fixed")]
    assert [(e["type"], e["sentence"]) for e in fixed] == [("ding", 3)]
