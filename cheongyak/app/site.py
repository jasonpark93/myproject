"""정적 사이트 생성: data/notices.json → dist/ (GitHub Pages 같은 정적 호스팅에 그대로 올린다)."""

from __future__ import annotations

import hashlib
import json
import shutil
from datetime import date, datetime, time, timedelta
from email.utils import format_datetime
from pathlib import Path
from xml.sax.saxutils import escape as xml_escape

from jinja2 import Environment, FileSystemLoader, select_autoescape
from markupsafe import Markup

from . import ROOT, dates, ics, regions, view

ACTIVE = ("open", "upcoming", "winner_wait", "contract")
RECENT_CLOSED_DAYS = 30
CALENDAR_PAST_DAYS = 14
CALENDAR_FUTURE_DAYS = 180
FEED_ITEMS = 50
RELATED_ITEMS = 6


def build(cfg: dict, db: dict, out_dir: Path | str, *, today: date, now: datetime, demo: bool = False) -> dict:
    out = _prepare_out(Path(out_dir))

    assets = _copy_static(out)
    env = _environment(cfg, assets, today=today, now=now, db=db, demo=demo)
    views = [view.enrich(n, today) for n in db["notices"].values()]
    by_status: dict[str, list[dict]] = {s: [] for s in view.STATUS_LABELS}
    for v in views:
        by_status[v["status"]].append(v)
    for status, key in view.SORT_KEYS.items():
        by_status[status].sort(key=key)
    finished = sorted(by_status["closed"], key=view.recent_first, reverse=True)

    region_names = {v["region"] for v in views}
    region_list = []
    for name, slug in regions.ordered(region_names):
        items = [v for v in views if v["region"] == name]
        region_list.append(
            {
                "name": name,
                "slug": slug,
                "path": f"/region/{slug}/",
                "notices": items,
                "active_count": sum(v["status"] in ACTIVE for v in items),
            }
        )
    env.globals["region_list"] = region_list

    pages: list[tuple[str, str]] = []  # (path, lastmod)

    def page(path: str, template: str, lastmod: str | None = None, **context) -> None:
        html = env.get_template(template).render(path=path, canonical=cfg["base_url"] + path, **context)
        _write(out, path, html)
        pages.append((path, lastmod or today.isoformat()))

    recent_cutoff = (today - timedelta(days=RECENT_CLOSED_DAYS)).isoformat()
    sections = [
        {"id": "open", "title": "지금 접수 중", "notices": by_status["open"], "empty": "오늘 접수 중인 청약이 없습니다."},
        {"id": "upcoming", "title": "접수 예정", "notices": by_status["upcoming"], "empty": "아직 예정된 청약 공고가 없습니다."},
        {
            "id": "winner",
            "title": "당첨자 발표·계약",
            "notices": by_status["winner_wait"] + by_status["contract"],
            "empty": "발표나 계약을 앞둔 공고가 없습니다.",
        },
        {
            "id": "closed",
            "title": "최근 마감",
            "notices": [v for v in finished if (v.get("apply_end") or v.get("announce_date") or "") >= recent_cutoff][:30],
            "empty": "최근 30일 안에 마감된 공고가 없습니다.",
        },
    ]
    counts = {s: len(by_status[s]) for s in by_status}
    page("/", "index.html", sections=sections, counts=counts)

    # 같은 지역 공고: 진행 중인 공고 먼저, 그다음 최신순
    region_ranked: dict[str, list[dict]] = {}
    for v in sorted(views, key=view.recent_first, reverse=True):
        region_ranked.setdefault(v["region"], []).append(v)
    for ranked in region_ranked.values():
        ranked.sort(key=lambda r: r["status"] not in ACTIVE)

    for v in views:
        related = [r for r in region_ranked[v["region"]][: RELATED_ITEMS + 1] if r["id"] != v["id"]]
        page(
            v["path"],
            "notice.html",
            lastmod=v.get("updated_on"),
            n=v,
            related=related[:RELATED_ITEMS],
            jsonld=_breadcrumb(cfg, [("홈", "/"), (v["region"], f"/region/{v['region_slug']}/"), (v["name"], v["path"])]),
        )
        events = _events(cfg, [v])
        if events:
            _write(out, v["path"] + "calendar.ics", ics.calendar(f"{v['name']} 청약 일정", events, now=now))

    page("/region/", "regions.html")
    for region in region_list:
        items = region["notices"]
        active = [v for v in items if v["status"] in ACTIVE]
        active.sort(key=lambda v: (ACTIVE.index(v["status"]), view.SORT_KEYS[v["status"]](v)))
        closed = sorted((v for v in items if v["status"] not in ACTIVE), key=view.recent_first, reverse=True)
        page(
            region["path"],
            "region.html",
            region=region,
            active=active,
            closed=closed,
            jsonld=_breadcrumb(cfg, [("홈", "/"), ("지역별 청약", "/region/"), (region["name"], region["path"])]),
        )

    for path, template in (
        ("/calculator/", "calculator.html"),
        ("/guide/", "guide.html"),
        ("/about/", "about.html"),
        ("/privacy/", "privacy.html"),
    ):
        page(path, template)

    _write(out, "/404.html", env.get_template("404.html").render(path="/404.html", canonical=cfg["base_url"] + "/"))

    window_start = (today - timedelta(days=CALENDAR_PAST_DAYS)).isoformat()
    window_end = (today + timedelta(days=CALENDAR_FUTURE_DAYS)).isoformat()
    calendar_views = [v for v in views if v.get("schedule")]
    events = [e for e in _events(cfg, calendar_views) if e["end"] >= window_start and e["start"] <= window_end]
    _write(out, "/calendar.ics", ics.calendar(cfg["site_name"], events, now=now))

    if not demo:
        _write(out, "/sitemap.xml", _sitemap(cfg, pages))
        _write(out, "/robots.txt", f"User-agent: *\nAllow: /\n\nSitemap: {cfg['base_url']}/sitemap.xml\n")
    else:
        _write(out, "/robots.txt", "User-agent: *\nDisallow: /\n")
    _write(out, "/feed.xml", _feed(cfg, views, now))
    if cfg["adsense_client"] and not demo:
        publisher = cfg["adsense_client"].removeprefix("ca-")
        _write(out, "/ads.txt", f"google.com, {publisher}, DIRECT, f08c47fec0942fa0\n")
    _write(out, "/.nojekyll", "")

    return {"pages": len(pages), "notices": len(views), "counts": counts, "events": len(events)}


def _prepare_out(out: Path) -> Path:
    """출력 폴더를 비운다. 실수로 프로젝트 폴더를 지우지 않도록 막는다."""
    out = out.resolve()
    if out == ROOT or out in ROOT.parents or (out / ".git").exists() or (out / "app").is_dir():
        raise ValueError(f"출력 폴더로 쓸 수 없는 경로입니다: {out}")
    if out.exists():
        shutil.rmtree(out)
    out.mkdir(parents=True)
    return out


def _environment(cfg: dict, assets: dict, *, today: date, now: datetime, db: dict, demo: bool) -> Environment:
    env = Environment(
        loader=FileSystemLoader(ROOT / "templates"),
        autoescape=select_autoescape(["html", "xml"]),
        trim_blocks=True,
        lstrip_blocks=True,
    )
    base_path = cfg["base_path"]

    def url(path: str) -> str:
        return base_path + path

    def asset(name: str) -> str:
        return f"{base_path}/static/{name}?v={assets[name]}"

    def ad(slot: str) -> Markup:
        unit = cfg["adsense_slots"].get(slot)
        if not (cfg["adsense_client"] and unit) or demo:
            return Markup("")
        return Markup(
            '<div class="ad-slot"><ins class="adsbygoogle" style="display:block" '
            f'data-ad-client="{cfg["adsense_client"]}" data-ad-slot="{unit}" '
            'data-ad-format="auto" data-full-width-responsive="true"></ins>'
            "<script>(adsbygoogle = window.adsbygoogle || []).push({});</script></div>"
        )

    updated = db.get("updated_at")
    env.globals.update(
        site=cfg,
        url=url,
        abs_url=lambda path: cfg["base_url"] + path,
        asset=asset,
        ad=ad,
        demo=demo,
        today=today.isoformat(),
        today_full=dates.full(today.isoformat()),
        updated_text=_updated_text(updated) if updated else "",
        year=today.year,
        flag_help=view.FLAG_HELP,
        webcal=_webcal(cfg["base_url"] + "/calendar.ics"),
    )
    env.filters["comma"] = lambda n: f"{n:,}" if isinstance(n, (int, float)) else ""
    env.filters["manwon"] = view.manwon
    env.filters["md"] = dates.md
    env.filters["full"] = dates.full
    env.filters["josa"] = lambda word, with_final, without_final: word + view.josa(word, with_final, without_final)
    return env


def _updated_text(iso: str) -> str:
    stamp = datetime.fromisoformat(iso).astimezone(dates.KST)
    return f"{stamp.year}년 {stamp.month}월 {stamp.day}일 {stamp:%H:%M}"


def _webcal(url: str) -> str:
    return "webcal://" + url.split("://", 1)[1]


def _copy_static(out: Path) -> dict[str, str]:
    target = out / "static"
    target.mkdir(parents=True)
    hashes = {}
    for src in sorted((ROOT / "static").iterdir()):
        if src.is_file():
            data = src.read_bytes()
            hashes[src.name] = hashlib.sha1(data).hexdigest()[:10]
            (target / src.name).write_bytes(data)
    return hashes


def _write(out: Path, path: str, content: str) -> None:
    target = out / path.lstrip("/")
    if path.endswith("/"):
        target = target / "index.html"
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_text(content, encoding="utf-8", newline="")


def _breadcrumb(cfg: dict, items: list[tuple[str, str]]) -> Markup:
    data = {
        "@context": "https://schema.org",
        "@type": "BreadcrumbList",
        "itemListElement": [
            {"@type": "ListItem", "position": i, "name": name, "item": cfg["base_url"] + path}
            for i, (name, path) in enumerate(items, start=1)
        ],
    }
    payload = json.dumps(data, ensure_ascii=False).replace("<", "\\u003c")
    return Markup(f'<script type="application/ld+json">{payload}</script>')


def _events(cfg: dict, views: list[dict]) -> list[dict]:
    events = []
    for v in views:
        page_url = cfg["base_url"] + v["path"]
        for step in v["schedule_view"]:
            action = " 접수" if step["receipt"] else ""
            events.append(
                {
                    "uid": f"{v['id'].replace('/', '-')}-{step['key']}@{cfg['host']}",
                    "start": step["start"],
                    "end": step.get("end") or step["start"],
                    "summary": f"[{v['region']}] {v['name']} {step['label']}{action}",
                    "description": f"{v['kind_label']} · 자세한 일정과 분양가: {page_url}",
                    "url": page_url,
                }
            )
    return events


def _sitemap(cfg: dict, pages: list[tuple[str, str]]) -> str:
    rows = [
        f"  <url><loc>{xml_escape(cfg['base_url'] + path)}</loc><lastmod>{lastmod}</lastmod></url>" for path, lastmod in pages
    ]
    return (
        '<?xml version="1.0" encoding="UTF-8"?>\n'
        '<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">\n' + "\n".join(rows) + "\n</urlset>\n"
    )


def _feed(cfg: dict, views: list[dict], now: datetime) -> str:
    latest = sorted(views, key=lambda v: (v.get("first_seen") or "", v.get("announce_date") or "", v["id"]), reverse=True)
    items = []
    for v in latest[:FEED_ITEMS]:
        link = cfg["base_url"] + v["path"]
        seen = date.fromisoformat(v.get("first_seen") or v.get("announce_date") or now.date().isoformat())
        published = datetime.combine(seen, time(6, 0), tzinfo=dates.KST)
        title = f"[{v['region']}] {v['name']} 청약 일정·분양가"
        items.append(
            "  <item>\n"
            f"   <title>{xml_escape(title)}</title>\n"
            f"   <link>{xml_escape(link)}</link>\n"
            f'   <guid isPermaLink="true">{xml_escape(link)}</guid>\n'
            f"   <pubDate>{format_datetime(published)}</pubDate>\n"
            f"   <description>{xml_escape(v['summary'])}</description>\n"
            "  </item>"
        )
    return (
        '<?xml version="1.0" encoding="UTF-8"?>\n<rss version="2.0">\n <channel>\n'
        f"  <title>{xml_escape(cfg['site_name'])}</title>\n"
        f"  <link>{xml_escape(cfg['base_url'])}/</link>\n"
        f"  <description>{xml_escape(cfg['tagline'])}</description>\n"
        "  <language>ko</language>\n"
        f"  <lastBuildDate>{format_datetime(now)}</lastBuildDate>\n" + "\n".join(items) + "\n </channel>\n</rss>\n"
    )
