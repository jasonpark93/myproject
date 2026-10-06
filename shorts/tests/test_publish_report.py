"""영상 보관함(GitHub Release) 발행 흐름과 주간 리포트."""

import copy
import json
from datetime import date

import pytest

from studio import episode, publish, report

TODAY = date(2026, 10, 10)


class FakeGitHub:
    def __init__(self):
        self.releases = {}
        self.saved = []

    def release(self, tag):
        return copy.deepcopy(self.releases.get(tag))

    def save_release(self, tag, title, notes, files, *, exists):
        assert exists == (tag in self.releases)
        self.releases[tag] = {"name": title, "body": notes.read_text(encoding="utf-8"), "assets": [f.name for f in files]}
        self.saved.append((tag, [f.name for f in files]))


class FakeRenderer:
    count = 0

    def __init__(self, workdir):
        self.workdir = workdir

    def render(self, ep, out):
        FakeRenderer.count += 1
        out.parent.mkdir(parents=True, exist_ok=True)
        out.write_bytes(b"mp4")
        cover = out.with_suffix(".jpg")
        cover.write_bytes(b"jpg")
        return {"duration": 31.5, "scenes": 5, "cover": cover, "notes": []}


class FakeUploader:
    def __init__(self):
        self.uploads = []

    def upload(self, video, metadata):
        self.uploads.append((video.name, metadata["status"].get("publishAt")))
        return f"yt{len(self.uploads)}"


@pytest.fixture
def episodes_dir(tmp_path, narrated, listed):
    d = tmp_path / "episodes"
    for day, ep in (("2026-10-09", listed), ("2026-10-11", narrated), ("2026-10-12", listed), ("2026-10-30", narrated)):
        e = copy.deepcopy(ep)
        e["id"] = f"{day}-{e['series']}-x"
        e["publish_at"] = f"{day}T19:00:00+09:00"
        episode.save(d / f"{e['id']}.json", e)
    return d


def run(cfg, gh, episodes_dir, tmp_path, uploader=None):
    return publish.publish(cfg, today=TODAY, gh=gh, make_renderer=FakeRenderer, out_dir=tmp_path / "out", uploader=uploader,
                           episodes_dir=episodes_dir, log=lambda *_: None)


def test_publish_renders_upcoming_once(cfg, tmp_path, episodes_dir):
    gh = FakeGitHub()
    FakeRenderer.count = 0
    summary = run(cfg, gh, episodes_dir, tmp_path)
    assert summary["rendered"] == ["2026-10-11-explainer-x", "2026-10-12-topn-x"]  # 지난 날짜·8일 이후는 제외
    body = gh.releases["short-2026-10-11-explainer-x"]["body"]
    assert "### 제목\n```\n청약 가점 84점, 이렇게 계산합니다\n```" in body
    assert "올리는 방법" in body and "#청약 #청약가점 #내집마련" in body
    assert publish.read_state(gh.releases["short-2026-10-11-explainer-x"])["duration"] == 31.5
    assert gh.saved[0][1] == ["2026-10-11-explainer-x.mp4", "2026-10-11-explainer-x.jpg"]

    second = run(cfg, gh, episodes_dir, tmp_path)
    assert second["rendered"] == [] and len(second["skipped"]) == 2 and FakeRenderer.count == 2


def test_publish_rerenders_edited_script(cfg, tmp_path, episodes_dir):
    gh = FakeGitHub()
    run(cfg, gh, episodes_dir, tmp_path)
    path = episodes_dir / "2026-10-11-explainer-x.json"
    ep = episode.load(path)
    ep["script"]["scenes"][0]["narration"] = "고친 문장입니다."
    episode.save(path, ep)
    assert run(cfg, gh, episodes_dir, tmp_path)["rendered"] == ["2026-10-11-explainer-x"]


def test_publish_uploads_once_with_api(cfg, tmp_path, episodes_dir):
    gh, uploader = FakeGitHub(), FakeUploader()
    summary = run(cfg, gh, episodes_dir, tmp_path, uploader)
    assert summary["uploaded"] == ["2026-10-11-explainer-x", "2026-10-12-topn-x"]
    assert uploader.uploads[0] == ("2026-10-11-explainer-x.mp4", "2026-10-11T10:00:00Z")
    body = gh.releases["short-2026-10-11-explainer-x"]["body"]
    assert "https://youtu.be/yt1" in body and "올리는 방법" not in body
    assert run(cfg, gh, episodes_dir, tmp_path, uploader)["uploaded"] == []

    path = episodes_dir / "2026-10-11-explainer-x.json"
    ep = episode.load(path)
    ep["script"]["title"] = "바뀐 제목"
    episode.save(path, ep)
    errors = run(cfg, gh, episodes_dir, tmp_path, uploader)["errors"]
    assert any("이미 유튜브에 올라간" in e for e in errors)


def test_invalid_episode_is_reported(cfg, tmp_path, episodes_dir):
    path = episodes_dir / "2026-10-12-topn-x.json"
    ep = episode.load(path)
    ep["script"]["items"] = []
    episode.save(path, ep)
    summary = run(cfg, FakeGitHub(), episodes_dir, tmp_path)
    assert any("2026-10-12-topn-x.json" in e for e in summary["errors"])


def test_github_wrapper_commands(tmp_path):
    calls = []

    class Done:
        def __init__(self, code=0, out="", err=""):
            self.returncode, self.stdout, self.stderr = code, out, err

    def fake_run(args, **kwargs):
        calls.append(args)
        if args[1:3] == ["release", "view"]:
            return Done(1, err="release not found")
        return Done()

    gh = publish.GitHub(run=fake_run)
    assert gh.release("short-x") is None
    notes = tmp_path / "n.md"
    notes.write_text("본문")
    gh.save_release("short-x", "제목", notes, [tmp_path / "a.mp4"], exists=False)
    assert calls[-1][:4] == ["gh", "release", "create", "short-x"] and "--prerelease" in calls[-1]
    gh.save_release("short-x", "제목", notes, [tmp_path / "a.mp4"], exists=True)
    assert calls[-2][:3] == ["gh", "release", "upload"] and calls[-1][:3] == ["gh", "release", "edit"]


def test_weekly_report(cfg, narrated):
    stats = {
        "title": "돈한입",
        "subscribers": 340,
        "total_views": 52000,
        "videos": [
            {"id": "a", "title": narrated["script"]["title"], "published_at": "2026-10-08T10:00:00Z", "views": 4200, "likes": 80, "comments": 12},
            {"id": "b", "title": "다른 영상", "published_at": "2026-09-20T10:00:00Z", "views": 900, "likes": 10, "comments": 1},
            {"id": "c", "title": "오래된 영상", "published_at": "2026-05-01T10:00:00Z", "views": 99999, "likes": 1, "comments": 0},
        ],
    }
    text = report.weekly_markdown(stats, [narrated], cfg, TODAY)
    assert "구독자 **340명**" in text and "(34%)" in text
    assert "1. [청약 가점 84점, 이렇게 계산합니다](https://youtu.be/a) — 조회 4,200" in text
    assert "1분 돈 상식: 평균 4,200회 (1편)" in text
    assert "오래된 영상" not in text
    assert report.popular_titles(stats, TODAY) == [narrated["script"]["title"], "다른 영상"]
