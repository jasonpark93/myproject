"""검토표(review.md, 렌더 전)와 편집 보고서(report.md, 렌더 후)."""

from __future__ import annotations

from collections import Counter
from pathlib import Path

from . import analyze, sfx as sfx_mod, zoom
from .util import fmt_time, write_json, write_text

CATEGORY = {"silence": "무음", "filler": "필러", "noise": "필러", "stutter": "필러", "ng": "NG", "offscript": "대본 외", "manual": "수동"}


def cut_counts(cuts: list) -> Counter:
    return Counter(CATEGORY.get(c["reason"], "무음") for c in cuts)


def _counts_text(counts: Counter) -> str:
    order = ["무음", "필러", "NG", "대본 외", "수동"]
    parts = [f"{k} {counts[k]}" for k in order if counts.get(k) or k in ("무음", "필러", "NG")]
    return " · ".join(parts)


def _marked(text: str, hl: list) -> str:
    for h in hl:
        text = text.replace(h, f"[[{h}]]", 1)
    return text


def _title(res) -> str:
    text = (res.settings["title"].get("text") or "").strip()
    return text.replace("\n", " / ") if text else "(없음)"


def _zoom_rows(res) -> list:
    rows = []
    level = {"punch": res.settings["zoom"]["punch"], "slow": res.settings["zoom"]["slow"]}
    for s in res.sentences:
        if s["kind"] == "none":
            continue
        pct = int(round(level[s["kind"]] * 100))
        label = f"펀치 {pct}%" if s["kind"] == "punch" else f"슬로우 100→{pct}%"
        rows.append((s["bound"], label, s["id"], s["text"]))
    return rows


def review(plan: dict, res, path: Path) -> str:
    """analyze 직후 Claude·사용자가 확인할 검토표."""
    words = res.words
    src = plan["source"]
    counts = cut_counts(res.cuts)
    kept_ids = {s["id"] for s in res.sentences}
    by_id = {s["id"]: s for s in res.sentences}
    lines = [
        f"# 편집 검토표 — {plan['name']}",
        "",
        f"- 원본: {Path(src['path']).name} ({fmt_time(src['duration'])}, {src['width']}x{src['height']}, {src['fps']:g}fps{', HDR' if src.get('color_transfer') in ('arib-std-b67', 'smpte2084') else ''})",
        f"- 음성 인식: {plan['transcript'].get('engine', '?')}" + (" · 대본 맞춤" if plan.get("script") else ""),
        f"- 예상 길이: {fmt_time(src['duration'])} → {fmt_time(res.duration)}",
        f"- 컷 {len(res.cuts)}곳: {_counts_text(counts)}",
        f"- 제목: {_title(res)}",
        "",
        "## 문장 (역할 → 줌)",
        "",
        "| # | 원본 시간 | 상태 | 역할 | 줌 | 내용 |",
        "| --- | --- | --- | --- | --- | --- |",
    ]
    for s in plan["sentences"]:
        a, b = s["from"], s["to"]
        if s["id"] in kept_ids:
            info = by_id[s["id"]]
            state = "남김"
            role = analyze.ROLE_LABEL.get(info["role"], info["role"])
            kind = zoom.ZOOM_LABEL[info["kind"]] + ("*" if info["zoom"] != "auto" else "")
            text = info["text"]
            removed = [words[i]["w"] for i in range(a, b + 1) if words[i].get("cut")]
            if removed:
                text += f"  (뺀 말: {' '.join(removed)})"
        else:
            reasons = Counter(words[i].get("cut") for i in range(a, b + 1))
            state = "✂ " + CATEGORY.get(reasons.most_common(1)[0][0], "컷")
            role = kind = "-"
            text = "~~" + analyze.sentence_text(words, a, b, kept_only=False) + "~~"
        lines.append(f"| S{s['id']} | {fmt_time(words[a]['s'])} | {state} | {role} | {kind} | {text} |")
    lines += ["", "## 잘린 곳", "", "| 컷 | 이유 | 원본 구간 | 완성본 위치 | 들어 있던 말 |", "| --- | --- | --- | --- | --- |"]
    for n, c in enumerate(res.cuts, 1):
        a, b = c["src"]
        lines.append(f"| C{n} | {CATEGORY.get(c['reason'], c['reason'])} | {fmt_time(a)}–{fmt_time(b)} ({b - a:.1f}초) | {fmt_time(c['out'])} | {c['text']} |")
    lines += ["", "## 자막 ([[ ]] = 강조색)", ""]
    for cap in res.captions:
        lines.append(f"- {fmt_time(cap['t0'])} S{cap['sentence']}: {_marked(cap['text'], cap['hl'])}")
    lines += ["", "## 효과음", ""]
    for ev in res.sfx_events:
        lines.append(f"- {fmt_time(ev['t'])} {sfx_mod.LABEL[ev['type']]} — {ev['why']} (S{ev['sentence']})")
    if not res.sfx_events:
        lines.append("- (없음)")
    lines += [
        "",
        "## 고치는 법",
        "",
        "- 오타: `python reels/reel.py fix 이름 클라우드 클로드`",
        "- 역할·줌: `python reels/reel.py role 이름 S3=number` / `zoom 이름 S3=none`",
        "- 잘린 것 살리기: `python reels/reel.py keep 이름 C4` (또는 완성본 시간 0:12)",
        "- 설정: `python reels/reel.py set 이름 gap=0.2 caption.size=1.2 caption.highlight=민트`",
        "- plan.json을 직접 고쳐도 됩니다. 고친 뒤 `python reels/reel.py render 이름`",
        "",
    ]
    text = "\n".join(lines)
    write_text(path, text)
    return text


def write(work: Path, plan: dict, res, out_path: Path, snaps: list, loudness, timings: dict) -> dict:
    src = plan["source"]
    counts = cut_counts(res.cuts)
    zoom_rows = _zoom_rows(res)
    saved = src["duration"] - res.duration
    lines = [
        f"# 편집 보고서 — {plan['name']}",
        "",
        f"- 완성본: `{out_path}`",
        f"- 길이: 원본 {fmt_time(src['duration'])} → 완성 {fmt_time(res.duration)} ({saved:.1f}초 줄임)",
        f"- 컷: {len(res.cuts)}곳 — {_counts_text(counts)}",
        f"- 규격: 1080x1920 · 30fps · H.264 + AAC" + (f" · 음량 {loudness:.1f} LUFS" if loudness is not None else ""),
        f"- 제목: {_title(res)}",
        *[f"- 참고: {n}" for n in res.notes],
        "",
        f"## 줌 ({len(zoom_rows)}회)",
        "",
        "| 시간 | 종류 | 문장 |",
        "| --- | --- | --- |",
    ]
    for t, label, sid, text in zoom_rows:
        lines.append(f"| {fmt_time(t)} | {label} | S{sid} {text} |")
    lines += ["", f"## 효과음 ({len(res.sfx_events)}개)", "", "| 시간 | 소리 | 이유 |", "| --- | --- | --- |"]
    for ev in res.sfx_events:
        lines.append(f"| {fmt_time(ev['t'])} | {sfx_mod.LABEL[ev['type']]} | {ev['why']} |")
    lines += ["", "## 자막 전문 (오타 확인용)", "", "| 시간 | 자막 |", "| --- | --- |"]
    for cap in res.captions:
        lines.append(f"| {fmt_time(cap['t0'])} | {_marked(cap['text'], cap['hl'])} |")
    lines += ["", "## 컷 목록", "", "| 컷 | 이유 | 원본 구간 | 완성본 위치 | 들어 있던 말 |", "| --- | --- | --- | --- | --- |"]
    for n, c in enumerate(res.cuts, 1):
        a, b = c["src"]
        lines.append(f"| C{n} | {CATEGORY.get(c['reason'], c['reason'])} | {fmt_time(a)}–{fmt_time(b)} | {fmt_time(c['out'])} | {c['text']} |")
    lines += ["", "## 스냅샷", ""]
    for label, t, path in snaps:
        name = {"start": "처음", "middle": "중간", "end": "끝", "sheet": "3장 모음"}[label]
        lines.append(f"- {name}{'' if t is None else f' ({fmt_time(t)})'}: `{path}`")
    if timings:
        lines += ["", "다시 만든 단계: " + ", ".join(f"{k} {v:.0f}초" for k, v in timings.items())]
    text = "\n".join(lines) + "\n"
    write_text(work / "report.md", text)
    summary = {
        "out": str(out_path),
        "duration": round(res.duration, 2),
        "source_duration": round(src["duration"], 2),
        "cuts": dict(counts),
        "zooms": len(zoom_rows),
        "sfx": len(res.sfx_events),
        "captions": len(res.captions),
        "loudness": loudness,
        "snapshots": [str(p) for _, _, p in snaps],
    }
    write_json(work / "report.json", summary)
    return {"text": text, **summary}
