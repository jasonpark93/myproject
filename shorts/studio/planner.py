"""한 주치 에피소드 계획: 요일별 시리즈 → 주제 선택 → 사실 확인 → 대본 → episodes/*.json 저장."""

from __future__ import annotations

import json
from datetime import date, datetime, timedelta
from pathlib import Path
from zoneinfo import ZoneInfo

from . import ROOT, config as config_mod, episode as episode_mod

KST = ZoneInfo("Asia/Seoul")
TOPICS = ROOT / "topics.json"


def load_topics(path: Path = TOPICS) -> dict:
    if not path.exists():
        return {"topics": []}
    with open(path, encoding="utf-8") as f:
        return json.load(f)


def save_topics(data: dict, path: Path = TOPICS) -> None:
    path.write_text(json.dumps(data, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")


def publish_at(day: date, cfg: dict) -> str:
    hh, mm = map(int, cfg["schedule"]["publish_time"].split(":"))
    return datetime(day.year, day.month, day.day, hh, mm, tzinfo=KST).isoformat()


def series_for(day: date, cfg: dict) -> str | None:
    return cfg["schedule"]["days"].get(config_mod.WEEKDAYS[day.weekday()]) or None


def next_topic(bank: dict, series: str) -> dict | None:
    for topic in bank["topics"]:
        if topic.get("series") == series and not topic.get("used_on"):
            return topic
    return None


# ---------- 청약 데이터 시리즈 ----------
def load_cheongyak(path: Path | None) -> dict | None:
    if not path or not Path(path).exists():
        return None
    with open(path, encoding="utf-8") as f:
        return json.load(f)


def _manwon(value: int) -> str:
    eok, man = divmod(int(value), 10000)
    return f"{eok}억 {man:,}만원" if eok and man else (f"{eok}억원" if eok else f"{man:,}만원")


def _md(iso: str | None) -> str:
    if not iso:
        return ""
    d = date.fromisoformat(iso)
    return f"{d.month}/{d.day}({'월화수목금토일'[d.weekday()]})"


def cheongyak_digest(data: dict | None, start: date, days: int = 7, limit: int = 6) -> str | None:
    """이번 주(start부터 days일) 접수가 시작되는 공고를 세대수 순으로 요약. 없으면 None."""
    if not data:
        return None
    end = start + timedelta(days=days - 1)
    picked = []
    for n in data.get("notices", {}).values():
        s = n.get("apply_start")
        if s and start.isoformat() <= s <= end.isoformat():
            picked.append(n)
    if not picked:
        return None
    picked.sort(key=lambda n: (-(n.get("total_units") or 0), n.get("apply_start") or ""))
    lines = [f"기간: {start.isoformat()} ~ {end.isoformat()} 접수 시작 공고 {len(picked)}건 (세대수 많은 순 상위 {min(limit, len(picked))}건)"]
    for n in picked[:limit]:
        steps = {s["key"]: s for s in n.get("schedule", [])}
        parts = []
        for key, label in (("special", "특별공급"), ("rank1_local", "1순위"), ("receipt", "접수"), ("general", "일반공급"), ("winner", "당첨자 발표")):
            if key in steps:
                parts.append(f"{label} {_md(steps[key]['start'])}")
        prices = [m["top_price"] for m in n.get("models") or [] if m.get("top_price")]
        price = f", 최고 분양가 {_manwon(max(prices))}" if prices else ""
        units = f", {n['total_units']:,}세대" if n.get("total_units") else ""
        lines.append(f"- [{n.get('region', '')}] {n['name']} ({n.get('kind_label', '')}{units}{price}) — {', '.join(parts)}")
    lines.append("출처: 한국부동산원 청약홈 분양정보(공공데이터포털). 세부 자격은 각 모집공고문 기준.")
    return "\n".join(lines)


# ---------- 계획 ----------
def plan(
    cfg: dict,
    *,
    start: date,
    days: int,
    writer,
    today: date,
    episodes_dir: Path = episode_mod.EPISODES,
    topics_path: Path = TOPICS,
    cheongyak: dict | None = None,
    popular_titles: list[str] | None = None,
    log=print,
) -> list[Path]:
    bank = load_topics(topics_path)
    existing = {p.name[:10] for p in episodes_dir.glob("*.json")}  # 날짜(YYYY-MM-DD)별 1편
    created: list[Path] = []
    for offset in range(days):
        day = start + timedelta(days=offset)
        if day.isoformat() in existing:
            log(f"{day}: 이미 에피소드가 있어 건너뜀")
            continue
        series = series_for(day, cfg)
        if not series:
            continue
        spec = cfg["series"][series]
        material, topic = "", None
        if spec.get("source") == "cheongyak":
            digest = cheongyak_digest(cheongyak, day)
            if digest:
                topic = {"id": f"cheongyak-week-{day.isoformat()}", "title": f"{day.month}월 {day.day}일부터 이번 주 청약 일정", "angle": "이번 주 놓치면 안 되는 청약 공고를 날짜순으로", "notes": ""}
                material = digest
            else:
                log(f"{day}: 청약 데이터가 없어 {spec.get('fallback')} 시리즈로 대체")
                series = spec.get("fallback")
                if not series:
                    continue
                spec = cfg["series"][series]
        if topic is None:
            topic = next_topic(bank, series)
            if topic is None:
                log(f"{day}: '{series}' 주제가 바닥나 AI에게 새 주제를 받습니다")
                used = [t["title"] for t in bank["topics"] if t.get("used_on")]
                bank["topics"] += writer.suggest_topics(today=today, count=12, used_titles=used, popular_titles=popular_titles or [])
                topic = next_topic(bank, series)
                if topic is None:
                    log(f"{day}: 주제를 정하지 못해 건너뜀")
                    continue
            if cfg["writer"]["research"]:
                notes, cited = writer.research(topic, today)
                material = notes + ("\n\n[검색에서 인용된 출처]\n" + "\n".join(f"- {c['title']} {c['url']}" for c in cited) if cited else "")
        script = writer.write(topic=topic, series_name=series, today=today, material=material)
        when = publish_at(day, cfg)
        ep = {
            "id": episode_mod.make_id(when, series, topic["id"]),
            "series": series,
            "format": spec["format"],
            "publish_at": when,
            "topic": {k: topic.get(k, "") for k in ("id", "title", "angle")},
            "script": script,
            "research_notes": material,
            "meta": {"generator": "claude", "model": cfg["writer"]["model"], "created_at": datetime.now(KST).isoformat(timespec="seconds")},
        }
        path = episodes_dir / f"{ep['id']}.json"
        episode_mod.save(path, ep)
        created.append(path)
        if not topic["id"].startswith("cheongyak-week-"):
            topic["used_on"] = day.isoformat()
        log(f"{day}: [{series}] {script['title']}")
    save_topics(bank, topics_path)
    return created


def review_markdown(paths: list[Path], cfg: dict) -> str:
    """PR 본문: 대본을 JSON을 열지 않고 읽을 수 있게 정리한다."""
    out = [
        "이번 주 쇼츠 대본입니다. 읽어 보고 고칠 곳은 **Files changed** 탭에서 파일을 직접 수정하세요.",
        "문제가 없으면 **Merge**하면 영상이 자동으로 만들어집니다.\n",
    ]
    for path in paths:
        ep = episode_mod.load(path)
        script = ep["script"]
        label = cfg["series"][ep["series"]]["label"]
        errors, warnings = episode_mod.validate(script, ep["format"], speed=cfg["voice"]["speed"])
        out.append(f"### {ep['publish_at'][:16].replace('T', ' ')} · {label}\n**{script['title']}** (`{path.name}`)")
        if ep["format"] == "list":
            out.append(f"> {script['list_title']} — {script.get('subtitle', '')}")
            out += [f"{i}. {item['text']}" + (f" — {item['detail']}" if item.get("detail") else "") for i, item in enumerate(script["items"], 1)]
        else:
            seconds = episode_mod.estimate_seconds(script, "narrated", cfg["voice"]["speed"])
            out.append(f"예상 길이 약 {seconds}초")
            out += [f"{i}. {s['narration']}" for i, s in enumerate(script["scenes"], 1)]
        if script.get("fact_check"):
            out.append("\n**업로드 전 확인할 사실**\n" + "\n".join(f"- [ ] {f}" for f in script["fact_check"]))
        if script.get("sources"):
            out.append("**출처** " + " · ".join(f"[{s['title'] or '링크'}]({s['url']})" for s in script["sources"]))
        for e in errors:
            out.append(f"> ❌ {e}")
        for w in warnings:
            out.append(f"> ⚠️ {w}")
        out.append("")
    return "\n".join(out)
