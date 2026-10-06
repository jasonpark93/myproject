"""주간 성과 리포트 (GitHub 이슈로 올림). 어떤 주제가 잘 되는지 보고 다음 주 주제에 반영한다."""

from __future__ import annotations

from datetime import date, datetime, timedelta, timezone

from . import episode as episode_mod


def _published(video: dict) -> date:
    return datetime.fromisoformat(video["published_at"].replace("Z", "+00:00")).astimezone(timezone(timedelta(hours=9))).date()


def popular_titles(stats: dict, today: date, days: int = 60, limit: int = 10) -> list[str]:
    recent = [v for v in stats["videos"] if (today - _published(v)).days <= days]
    return [v["title"] for v in sorted(recent, key=lambda v: v["views"], reverse=True)[:limit]]


def weekly_markdown(stats: dict, episodes: list[dict], cfg: dict, today: date) -> str:
    videos = stats["videos"]
    by_title = {e["script"]["title"]: e for e in episodes}
    last7 = [v for v in videos if (today - _published(v)).days < 7]
    last30 = [v for v in videos if (today - _published(v)).days < 30]
    last90_views = sum(v["views"] for v in videos if (today - _published(v)).days < 90)

    lines = [
        f"## 📊 {stats['title']} 주간 리포트 ({today.isoformat()})",
        "",
        f"- 구독자 **{stats['subscribers']:,}명** · 누적 조회수 {stats['total_views']:,}회",
        f"- 최근 7일 업로드 {len(last7)}편, 이 영상들의 조회수 합계 {sum(v['views'] for v in last7):,}회",
        f"- 최근 90일 업로드 영상 조회수 합계 약 {last90_views:,}회 (수익 창출 조건 참고용 추정치)",
        "",
        "### 수익 창출(YPP) 진행 상황",
        f"- 구독자 {stats['subscribers']:,} / 1,000명 ({min(100, stats['subscribers'] / 10):.0f}%)",
        f"- 쇼츠 조회수(90일) 약 {last90_views:,} / 10,000,000회 — 2027년 2월 1일부터 새로 신청하는 채널은 20,000,000회",
        "",
        "### 최근 30일 조회수 TOP 5",
    ]
    top = sorted(last30, key=lambda v: v["views"], reverse=True)[:5]
    if top:
        lines += [f"{i}. [{v['title']}](https://youtu.be/{v['id']}) — 조회 {v['views']:,} · 좋아요 {v['likes']:,} · 댓글 {v['comments']:,}" for i, v in enumerate(top, 1)]
    else:
        lines.append("- 아직 데이터가 없습니다.")

    series_views: dict[str, list[int]] = {}
    for v in last30:
        ep = by_title.get(v["title"])
        if ep:
            series_views.setdefault(ep["series"], []).append(v["views"])
    if series_views:
        lines += ["", "### 시리즈별 평균 조회수 (최근 30일)"]
        for name, views in sorted(series_views.items(), key=lambda kv: -sum(kv[1]) / len(kv[1])):
            label = cfg["series"].get(name, {}).get("label", name)
            lines.append(f"- {label}: 평균 {sum(views) // len(views):,}회 ({len(views)}편)")

    lines += [
        "",
        "### 다음 주에 할 일",
        "- 조회수 상위 영상과 비슷한 주제·첫 문장 패턴을 다음 주 대본에 반영합니다 (자동 계획에도 반영됨).",
        "- 조회수가 낮은 시리즈는 첫 2초(훅)와 제목을 먼저 바꿔 보세요.",
    ]
    return "\n".join(lines)


def episodes_for_report() -> list[dict]:
    return [ep for _, ep in episode_mod.all_episodes()]
