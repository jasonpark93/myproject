"""AI 쇼츠 스튜디오 CLI (shorts/ 디렉터리에서 실행)

  python -m studio check                 대본 파일 검사 (오류가 있으면 종료 코드 1)
  python -m studio preview EP.json       API 키 없이 무음으로 미리보기 렌더 (out/)
  python -m studio render EP.json        설정한 목소리로 실제 렌더 (out/)
  python -m studio plan --days 7         Claude로 다음 7일치 대본 생성 (ANTHROPIC_API_KEY)
  python -m studio publish               곧 공개될 영상 렌더 → GitHub Release (+ 유튜브 예약 업로드)
  python -m studio report                주간 성과 리포트 (YOUTUBE_API_KEY, YOUTUBE_CHANNEL_ID)
  python -m studio brand                 채널 프로필·배너 이미지 만들기 (brand/)
  python -m studio voices                Google TTS 한국어 목소리 목록
  python -m studio auth                  유튜브 업로드용 refresh token 발급 (내 컴퓨터에서 한 번)
"""

from __future__ import annotations

import argparse
import os
import sys
from datetime import date, datetime, timedelta
from pathlib import Path
from zoneinfo import ZoneInfo

from . import ROOT, config, episode as episode_mod

KST = ZoneInfo("Asia/Seoul")
OUT = ROOT / "out"
CACHE = ROOT / "cache"


def today_kst() -> date:
    override = os.environ.get("STUDIO_TODAY")
    return date.fromisoformat(override) if override else datetime.now(KST).date()


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(prog="python -m studio", description="AI 쇼츠 스튜디오")
    sub = parser.add_subparsers(dest="command", required=True)
    sub.add_parser("check")
    for name in ("preview", "render"):
        p = sub.add_parser(name)
        p.add_argument("episodes", nargs="+")
        p.add_argument("--out", default=str(OUT))
    p = sub.add_parser("plan")
    p.add_argument("--days", type=int, default=7)
    p.add_argument("--start", help="시작 날짜 YYYY-MM-DD (기본: 내일)")
    p.add_argument("--review-file", help="PR 본문으로 쓸 검수용 요약을 이 파일에 저장")
    p = sub.add_parser("publish")
    p.add_argument("--horizon", type=int, default=8, help="며칠 뒤 공개분까지 미리 렌더할지")
    p = sub.add_parser("report")
    p.add_argument("--output", help="마크다운을 이 파일에 저장")
    sub.add_parser("brand")
    sub.add_parser("voices")
    sub.add_parser("auth")
    args = parser.parse_args(argv)

    cfg = config.load()
    try:
        return COMMANDS[args.command](cfg, args)
    except config.ConfigError as e:
        print(f"설정 오류: {e}", file=sys.stderr)
        return 2


def cmd_check(cfg, args) -> int:
    failed = 0
    for path, ep in episode_mod.all_episodes():
        errors, warnings = episode_mod.validate(ep["script"], ep["format"], speed=cfg["voice"]["speed"])
        status = "❌" if errors else ("⚠️" if warnings else "✅")
        seconds = episode_mod.estimate_seconds(ep["script"], ep["format"], cfg["voice"]["speed"]) if not errors else 0
        print(f"{status} {path.name} ({seconds}초) {ep['script'].get('title', '')}")
        for message in errors + warnings:
            print(f"    - {message}")
        failed += bool(errors)
    return 1 if failed else 0


def _renderer(cfg, workdir: Path, *, silent: bool = False):
    from . import broll, render, tts

    engine = tts.SilentTTS() if silent else tts.from_config(cfg, os.environ, CACHE / "tts")
    pexels = None
    if cfg["broll"] == "pexels" and os.environ.get("PEXELS_API_KEY") and not silent:
        pexels = broll.Pexels(os.environ["PEXELS_API_KEY"], CACHE / "broll")
    return render.Renderer(cfg, tts=engine, workdir=workdir, pexels=pexels)


def _render(cfg, args, silent: bool) -> int:
    out_dir = Path(args.out)
    for name in args.episodes:
        ep = episode_mod.load(name)
        errors, warnings = episode_mod.validate(ep["script"], ep["format"], speed=cfg["voice"]["speed"])
        if errors:
            print(f"❌ {name}: " + "; ".join(errors), file=sys.stderr)
            return 1
        out = out_dir / f"{ep['id']}{'-preview' if silent else ''}.mp4"
        result = _renderer(cfg, out_dir / "work" / ep["id"], silent=silent).render(ep, out)
        print(f"✅ {out} ({result['duration']}초, 장면 {result['scenes']}개)")
        for note in result["notes"]:
            print(f"   - {note}")
    return 0


def cmd_preview(cfg, args) -> int:
    return _render(cfg, args, silent=True)


def cmd_render(cfg, args) -> int:
    return _render(cfg, args, silent=False)


def cmd_plan(cfg, args) -> int:
    from . import planner, writer

    today = today_kst()
    start = date.fromisoformat(args.start) if args.start else today + timedelta(days=1)
    data_path = Path(os.environ.get("CHEONGYAK_DATA", ROOT.parent / "cheongyak" / "data" / "notices.json"))
    popular: list[str] = []
    if os.environ.get("YOUTUBE_API_KEY") and os.environ.get("YOUTUBE_CHANNEL_ID"):
        from . import report, youtube

        try:
            stats = youtube.channel_stats(os.environ["YOUTUBE_API_KEY"], os.environ["YOUTUBE_CHANNEL_ID"])
            popular = report.popular_titles(stats, today)
        except youtube.YouTubeError as e:
            print(f"조회수 데이터를 가져오지 못했습니다(계속 진행): {e}")
    author = writer.Writer(cfg)
    try:
        created = planner.plan(
            cfg,
            start=start,
            days=args.days,
            writer=author,
            today=today,
            cheongyak=planner.load_cheongyak(data_path),
            popular_titles=popular,
        )
    except writer.WriterError as e:
        print(f"대본 생성 실패: {e}", file=sys.stderr)
        return 1
    u = author.usage
    cost = u["input_tokens"] / 1e6 * 4 + u["output_tokens"] / 1e6 * 20 + u["web_search_requests"] * 0.01
    print(f"새 대본 {len(created)}편 · API 호출 {u['calls']}회 · 검색 {u['web_search_requests']}회 · 예상 비용 약 ${cost:.2f} (Opus 5.5 정가 기준)")
    if args.review_file:
        Path(args.review_file).write_text(planner.review_markdown(created, cfg), encoding="utf-8")
    return 0


def cmd_publish(cfg, args) -> int:
    from . import publish, youtube

    uploader = None
    if cfg["upload"] == "api":
        uploader = youtube.YouTube(
            os.environ.get("YOUTUBE_CLIENT_ID", ""), os.environ.get("YOUTUBE_CLIENT_SECRET", ""), os.environ.get("YOUTUBE_REFRESH_TOKEN", "")
        )
    summary = publish.publish(
        cfg,
        today=today_kst(),
        gh=publish.GitHub(),
        make_renderer=lambda workdir: _renderer(cfg, workdir),
        out_dir=OUT,
        uploader=uploader,
        horizon_days=args.horizon,
    )
    print(f"렌더 {len(summary['rendered'])}편, 업로드 {len(summary['uploaded'])}편, 변경 없음 {len(summary['skipped'])}편")
    for error in summary["errors"]:
        print(f"❌ {error}", file=sys.stderr)
    return 1 if summary["errors"] else 0


def cmd_report(cfg, args) -> int:
    from . import report, youtube

    stats = youtube.channel_stats(os.environ.get("YOUTUBE_API_KEY", ""), os.environ.get("YOUTUBE_CHANNEL_ID", ""))
    text = report.weekly_markdown(stats, report.episodes_for_report(), cfg, today_kst())
    if args.output:
        Path(args.output).write_text(text, encoding="utf-8")
    print(text)
    return 0


def cmd_brand(cfg, args) -> int:
    from . import visuals

    target = ROOT / "brand"
    target.mkdir(exist_ok=True)
    visuals.profile_image(cfg["channel"]["name"], cfg["brand"]).save(target / "profile.png")
    visuals.banner_image(cfg["channel"]["name"], cfg["channel"]["tagline"], cfg["brand"]).save(target / "banner.png")
    print(f"✅ {target / 'profile.png'} (800x800), {target / 'banner.png'} (2560x1440)")
    return 0


def cmd_voices(cfg, args) -> int:
    from . import tts

    engine = tts.GoogleTTS(os.environ.get("GOOGLE_TTS_API_KEY", ""), cfg["voice"]["google_voice"])
    for voice in sorted(engine.voices(), key=lambda v: v["name"]):
        print(f"{voice['name']:<36} {voice.get('ssmlGender', '')}")
    return 0


def cmd_auth(cfg, args) -> int:
    from . import youtube

    client_id = os.environ.get("YOUTUBE_CLIENT_ID") or input("OAuth 클라이언트 ID: ").strip()
    client_secret = os.environ.get("YOUTUBE_CLIENT_SECRET") or input("OAuth 클라이언트 보안 비밀번호: ").strip()
    token = youtube.authorize(client_id, client_secret)
    print("\n아래 값을 GitHub Secrets의 YOUTUBE_REFRESH_TOKEN에 저장하세요 (다른 사람에게 보여주지 마세요):\n")
    print(token)
    return 0


COMMANDS = {
    "check": cmd_check,
    "preview": cmd_preview,
    "render": cmd_render,
    "plan": cmd_plan,
    "publish": cmd_publish,
    "report": cmd_report,
    "brand": cmd_brand,
    "voices": cmd_voices,
    "auth": cmd_auth,
}

if __name__ == "__main__":
    sys.exit(main())
