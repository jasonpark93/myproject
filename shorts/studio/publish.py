"""CI용: 곧 공개될 에피소드를 렌더해 GitHub Release(영상 보관함)에 올리고, 설정에 따라 유튜브에 예약 업로드한다.

릴리스 하나 = 영상 하나 (태그 short-<id>). 본문 끝 숨은 주석에 상태(대본 해시, 유튜브 ID)를 저장해
같은 영상을 두 번 렌더하거나 두 번 업로드하지 않는다.
"""

from __future__ import annotations

import json
import re
import subprocess
from datetime import date, datetime
from pathlib import Path
from typing import Callable

from . import episode as episode_mod

STATE = re.compile(r"<!-- studio-state: (\{.*?\}) -->")


class GitHubError(RuntimeError):
    pass


class GitHub:
    """gh CLI 래퍼 (GitHub Actions에서는 GH_TOKEN으로 인증됨)."""

    def __init__(self, run: Callable = subprocess.run, gh: str = "gh"):
        self._run = run
        self.gh = gh

    def _call(self, args: list[str]) -> subprocess.CompletedProcess:
        return self._run([self.gh, *args], capture_output=True, text=True)

    def release(self, tag: str) -> dict | None:
        result = self._call(["release", "view", tag, "--json", "name,body,assets"])
        if result.returncode != 0:
            if "not found" in (result.stderr or "").lower():
                return None
            raise GitHubError(result.stderr.strip())
        return json.loads(result.stdout)

    def save_release(self, tag: str, title: str, notes: Path, files: list[Path], *, exists: bool) -> None:
        if exists:
            steps = [["release", "edit", tag, "--title", title, "--notes-file", str(notes)]]
            if files:
                steps.insert(0, ["release", "upload", tag, *map(str, files), "--clobber"])
        else:
            steps = [["release", "create", tag, *map(str, files), "--title", title, "--notes-file", str(notes), "--prerelease"]]
        for args in steps:
            result = self._call(args)
            if result.returncode != 0:
                raise GitHubError(f"gh {' '.join(args[:3])} 실패: {result.stderr.strip()}")


def read_state(release: dict | None) -> dict:
    if not release:
        return {}
    match = STATE.search(release.get("body") or "")
    return json.loads(match.group(1)) if match else {}


def release_notes(episode: dict, cfg: dict, *, duration: float, state: dict, description: str) -> str:
    script = episode["script"]
    when = datetime.fromisoformat(episode["publish_at"])
    label = cfg["series"][episode["series"]]["label"]
    lines = [
        f"## 📅 공개 예정: {when:%Y-%m-%d} ({'월화수목금토일'[when.weekday()]}) {when:%H:%M}",
        f"{label} · 길이 {duration:.1f}초",
        "",
    ]
    if state.get("youtube"):
        lines += [f"✅ 유튜브 예약 업로드 완료: https://youtu.be/{state['youtube']}", ""]
    else:
        lines += [
            "### 올리는 방법 (YouTube 앱, 약 3분)",
            "1. 아래 Assets의 mp4를 휴대폰에 저장 → YouTube 앱 ➕ → 동영상 업로드",
            "2. 아래 제목·설명을 복사해 붙여넣기 → 공개 범위 **예약** → 위 시각 선택",
            "3. 배경음악이 없으면 앱의 '사운드'에서 저작권 걱정 없는 음악을 추가",
            "4. 공개 후 고정 댓글 달기",
            "",
        ]
    lines += ["### 제목", "```", script["title"], "```", "### 설명", "```", description, "```"]
    if script.get("pinned_comment"):
        lines += ["### 고정 댓글", "```", script["pinned_comment"], "```"]
    if script.get("fact_check"):
        lines += ["### 올리기 전 확인할 사실"] + [f"- [ ] {f}" for f in script["fact_check"]]
    lines += ["", f"<!-- studio-state: {json.dumps(state, ensure_ascii=False)} -->"]
    return "\n".join(lines)


def publish(
    cfg: dict,
    *,
    today: date,
    gh: GitHub,
    make_renderer: Callable[[Path], object],
    out_dir: Path,
    uploader=None,
    horizon_days: int = 8,
    episodes_dir: Path = episode_mod.EPISODES,
    log=print,
) -> dict:
    summary = {"rendered": [], "uploaded": [], "skipped": [], "errors": []}
    for path, ep in episode_mod.all_episodes(episodes_dir):
        day = episode_mod.publish_datetime(ep).date()
        if day < today or (day - today).days > horizon_days:
            continue
        errors, _ = episode_mod.validate(ep["script"], ep["format"], speed=cfg["voice"]["speed"])
        if errors:
            summary["errors"].append(f"{path.name}: " + "; ".join(errors))
            continue
        tag = f"short-{ep['id']}"
        release = gh.release(tag)
        state = read_state(release)
        digest = episode_mod.content_hash(ep, cfg)
        description = episode_mod.description(ep, cfg)
        needs_render = state.get("hash") != digest
        if needs_render and state.get("youtube"):
            summary["errors"].append(f"{path.name}: 이미 유튜브에 올라간 영상의 대본이 바뀌었습니다. 유튜브에서 직접 교체하세요.")
            continue

        files: list[Path] = []
        duration = float(state.get("duration") or 0)
        if needs_render:
            log(f"렌더: {ep['id']}")
            out = out_dir / f"{ep['id']}.mp4"
            renderer = make_renderer(out_dir / "work" / ep["id"])
            result = renderer.render(ep, out)
            duration = result["duration"]
            files = [out, result["cover"]]
            state = {"hash": digest, "duration": duration, "youtube": None}
            summary["rendered"].append(ep["id"])
            for note in result.get("notes", []):
                log(f"  - {note}")

        if uploader is not None and not state.get("youtube"):
            video = files[0] if files else _download(gh, tag, out_dir)
            from .youtube import build_metadata

            state["youtube"] = uploader.upload(video, build_metadata(ep, cfg, description))
            summary["uploaded"].append(ep["id"])
            log(f"업로드: {ep['id']} → https://youtu.be/{state['youtube']}")
        elif not needs_render:
            summary["skipped"].append(ep["id"])
            continue

        notes = out_dir / f"{ep['id']}.notes.md"
        notes.parent.mkdir(parents=True, exist_ok=True)
        notes.write_text(release_notes(ep, cfg, duration=duration, state=state, description=description), encoding="utf-8")
        gh.save_release(tag, f"{ep['publish_at'][:10]} {ep['script']['title']}", notes, files, exists=release is not None)
    return summary


def _download(gh: GitHub, tag: str, out_dir: Path) -> Path:
    target = out_dir / "download" / tag
    target.mkdir(parents=True, exist_ok=True)
    result = gh._call(["release", "download", tag, "--pattern", "*.mp4", "--dir", str(target), "--clobber"])
    if result.returncode != 0:
        raise GitHubError(f"영상 내려받기 실패: {result.stderr.strip()}")
    return next(target.glob("*.mp4"))
