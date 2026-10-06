"""명령어: python reels/reel.py <명령> ...

  edit  영상 [--script 대본.txt]   분석 + 렌더 한 번에 (가장 많이 씀)
  analyze 영상 [--script/--srt/--words]  분석만 → 검토표(review.md)
  render 이름                       계획대로 렌더 (바뀐 단계만 다시)
  set 이름 키=값 ...  [--save]      설정 바꾸기 (--save: 다음 영상에도 적용)
  fix 이름 틀린말 바른말            자막 오타 고치기
  keep 이름 C3|0:12|src:0:12        잘린 부분 되살리기
  drop 이름 S3|0:12-0:14            문장·구간 지우기
  role 이름 S3=number ...           문장 역할(hook number twist conclusion cta explain)
  zoom 이름 S3=punch|slow|none|auto 줌 직접 지정
  sfx 이름 S3=whoosh|ding|pop|none|auto  효과음 직접 지정
  title 이름 "제목 [[강조]]"         상단 제목 (빈 문자열이면 없앰)
  review 이름                       검토표 다시 만들기
  list                              작업 목록
  story 이름 [--voice 녹음.m4a]       모션그래픽 릴스 (stories/이름.json: 대본 + 카드)
  ref 영상                          참고 영상(잘 된 릴스) 분석
  sounds                            효과음 미리듣기 파일 만들기
  doctor                            설치 상태 점검
'이름' 자리에 last 를 쓰면 가장 최근 작업.
"""

from __future__ import annotations

import argparse
import re
import sys
import time
from pathlib import Path

from . import ROOT, SR
from .util import EditorError, fmt_time, read_json, write_json

WORK_DIR = ROOT / "work"
INBOX = ROOT / "inbox"
STYLE = ROOT / "style.json"
VIDEO_EXTS = (".mp4", ".mov", ".m4v", ".mkv", ".webm", ".avi", ".mts")


def log(msg: str) -> None:
    print(msg, flush=True)


# ---------- 작업 찾기 ----------
def _name_for(src: Path) -> str:
    base = re.sub(r"[^0-9A-Za-z가-힣._-]+", "_", src.stem).strip("_") or "video"
    name, n = base, 2
    while (WORK_DIR / name / "plan.json").exists():
        try:
            if read_json(WORK_DIR / name / "plan.json")["source"]["path"] == str(src):
                return name
        except Exception:
            pass
        name, n = f"{base}-{n}", n + 1
    return name


def find_work(ref: str) -> Path:
    ref = (ref or "last").strip()
    path = Path(ref).expanduser()
    if path.is_dir() and (path / "plan.json").exists():
        return path
    if (WORK_DIR / ref / "plan.json").exists():
        return WORK_DIR / ref
    items = sorted(WORK_DIR.glob("*/plan.json"), key=lambda p: p.stat().st_mtime, reverse=True) if WORK_DIR.exists() else []
    if ref in ("last", "latest", "최근", "방금"):
        if not items:
            raise EditorError("아직 작업이 없습니다. 먼저 edit 또는 analyze를 실행하세요.")
        return items[0].parent
    if path.suffix.lower() in VIDEO_EXTS and path.exists():
        for item in items:
            if read_json(item)["source"]["path"] == str(path.resolve()):
                return item.parent
    raise EditorError(f"작업을 찾지 못했습니다: {ref} (python reels/reel.py list 로 확인)")


def find_video(ref: str | None) -> Path:
    if ref and ref not in ("last", "inbox", "최근"):
        path = Path(ref).expanduser()
        if not path.exists():
            raise EditorError(f"영상 파일이 없습니다: {ref}")
        return path.resolve()
    videos = sorted((p for p in INBOX.glob("*") if p.suffix.lower() in VIDEO_EXTS), key=lambda p: p.stat().st_mtime, reverse=True)
    if not videos:
        raise EditorError(f"영상이 없습니다. {INBOX} 폴더에 촬영본을 넣거나 파일 경로를 알려 주세요.")
    return videos[0].resolve()


def _style() -> dict:
    return read_json(STYLE) if STYLE.exists() else {}


def _script_for(src: Path, explicit: str | None):
    if explicit:
        return Path(explicit).expanduser().read_text(encoding="utf-8")
    for candidate in (src.with_suffix(".txt"), src.parent / f"{src.stem}_대본.txt", src.parent / "대본.txt"):
        if candidate.exists():
            log(f"대본 사용: {candidate.name}")
            return candidate.read_text(encoding="utf-8")
    return None


# ---------- 분석 ----------
def analyze(video: str | None, script: str | None = None, srt: str | None = None, words_file: str | None = None, model: str | None = None, retranscribe: bool = False) -> Path:
    from . import face as face_mod, plan as plan_mod, report, transcript
    from .audio import Envelope
    from .media import decode_audio, probe, sdr_filter

    src = find_video(video)
    p = probe(src)
    name = _name_for(src)
    work = WORK_DIR / name
    work.mkdir(parents=True, exist_ok=True)
    log(f"원본: {src.name} ({fmt_time(p.duration)}, {p.width}x{p.height}, {p.fps:g}fps) → 작업 이름: {name}")
    if not p.has_audio:
        raise EditorError("소리가 없는 영상입니다. 말하는 영상을 넣어 주세요.")
    t = time.time()
    samples = decode_audio(src, duration=p.duration)
    env = Envelope.from_samples(samples)
    env.save(work / "envelope.npz")

    script_text = _script_for(src, script)
    words_path = work / "words.json"
    source_id = {"path": str(src), "size": src.stat().st_size, "mtime": int(src.stat().st_mtime)}
    cached = read_json(words_path) if words_path.exists() else None
    if words_file:
        words, meta = transcript.load_words(Path(words_file)), {"engine": f"단어 파일 {Path(words_file).name}"}
    elif srt:
        words, meta = transcript.words_from_srt(Path(srt), env), {"engine": f"SRT {Path(srt).name} (단어 시간 추정)"}
    elif cached and cached.get("source") == source_id and not retranscribe and (not model or cached["meta"].get("model") == model):
        words, meta = cached["words"], cached["meta"]
        log("음성 인식: 이전 결과 재사용")
    else:
        model = model or transcript.DEFAULT_MODEL
        log("음성 인식 중…")
        audio16 = decode_audio(src, sr=16000, duration=p.duration)
        hot = None
        if script_text:
            hot = " ".join(sorted({w for w in re.findall(r"[가-힣A-Za-z0-9]{2,}", script_text)}, key=len, reverse=True)[:30])
        words, meta = transcript.transcribe(audio16, model, hotwords=hot, log=log)
        meta["model"] = model
    write_json(words_path, {"source": source_id, "meta": meta, "words": words})
    if not words:
        raise EditorError("말소리를 찾지 못했습니다. 소리가 너무 작거나 말이 없는 영상인지 확인하세요.")

    sdr, warn = sdr_filter(p)
    if warn:
        log(warn)
    face = face_mod.track(p, sdr, log=log)
    write_json(work / "faces.json", face)

    plan_path = work / "plan.json"
    if plan_path.exists():
        plan_path.replace(work / "plan.prev.json")
        log("이전 계획은 plan.prev.json 으로 보관했습니다.")
    plan = plan_mod.build(name, p, words, meta, script_text, settings=_style())
    write_json(plan_path, plan)
    res = plan_mod.resolve(plan, env)
    report.review(plan, res, work / "review.md")
    stats = plan["stats"]
    log(
        f"분석 끝 ({time.time() - t:.0f}초): 단어 {len(words)}개, 문장 {len(res.sentences)}개 남김, "
        f"군말 {stats['filler']} · 말더듬 {stats['stutter']} · NG 단어 {stats['ng']}"
        + (f" · 대본 외 {stats.get('offscript', 0)} · 오타 수정 {stats.get('fixed', 0)}" if plan.get("script") else "")
    )
    log(f"예상 길이 {fmt_time(p.duration)} → {fmt_time(res.duration)}, 컷 {len(res.cuts)}곳")
    log(f"검토표: {work / 'review.md'}")
    return work


def render(ref: str, out: str | None = None) -> dict:
    from . import render as render_mod

    work = find_work(ref)
    info = render_mod.render(work, log=log, out_path=Path(out).expanduser() if out else None)
    log("")
    log(info["text"])
    return info


# ---------- 고치기 ----------
def _load(ref: str):
    from .audio import Envelope

    work = find_work(ref)
    return work, read_json(work / "plan.json"), Envelope.load(work / "envelope.npz")


def _save(work: Path, plan: dict, env) -> None:
    from . import plan as plan_mod, report

    write_json(work / "plan.json", plan)
    res = plan_mod.resolve(plan, env)
    report.review(plan, res, work / "review.md")
    log(f"저장했습니다 → 예상 길이 {fmt_time(res.duration)}, 컷 {len(res.cuts)}곳. 반영하려면: python reels/reel.py render {work.name}")


def cmd_set(ref: str, pairs: list, save: bool) -> None:
    from . import plan as plan_mod

    work, plan, env = _load(ref)
    style = _style()
    for pair in pairs:
        if "=" not in pair:
            raise EditorError(f"키=값 형식으로 적어 주세요: {pair}")
        key, value = pair.split("=", 1)
        old, new = plan_mod.set_value(plan, key.strip(), value)
        log(f"{key.strip()}: {old} → {new}")
        if save:
            key = plan_mod.SETTING_ALIASES.get(key.strip(), key.strip())
            node = style
            parts = key.split(".")
            for part in parts[:-1]:
                node = node.setdefault(part, {})
            node[parts[-1]] = new
    if save:
        write_json(STYLE, style)
        log(f"다음 영상부터도 적용: {STYLE}")
    _save(work, plan, env)


def cmd_fix(ref: str, old: str, new: str) -> None:
    from . import plan as plan_mod

    work, plan, env = _load(ref)
    n = plan_mod.fix_text(plan, old, new)
    log(f"'{old}' → '{new}' {n}곳 바꿈")
    if n == 0:
        log("바꿀 곳이 없었습니다. 검토표의 자막 글자를 확인해 주세요.")
    _save(work, plan, env)


def cmd_keep(ref: str, targets: list) -> None:
    from . import plan as plan_mod

    work, plan, env = _load(ref)
    for target in targets:
        res = plan_mod.resolve(plan, env)
        cut = plan_mod.restore(plan, res, target)
        log(f"되살림: 원본 {fmt_time(cut['src'][0])}–{fmt_time(cut['src'][1])} ({cut['reason']}) {cut['text']}")
    _save(work, plan, env)


def cmd_drop(ref: str, targets: list) -> None:
    from . import plan as plan_mod

    work, plan, env = _load(ref)
    for target in targets:
        if re.fullmatch(r"[Ss]\d+", target.strip()):
            s = plan_mod.sentence(plan, target)
            for i in range(s["from"], s["to"] + 1):
                plan["words"][i].setdefault("cut", "manual")
            log(f"S{s['id']} 지움")
        else:
            res = plan_mod.resolve(plan, env)
            lo, hi = plan_mod.drop_range(plan, res, target)
            log(f"원본 {fmt_time(lo)}–{fmt_time(hi)} 지움")
    _save(work, plan, env)


def cmd_assign(ref: str, field: str, pairs: list) -> None:
    from . import analyze as analyze_mod, plan as plan_mod, sfx as sfx_mod

    work, plan, env = _load(ref)
    allowed = {
        "role": set(analyze_mod.ROLES),
        "zoom": {"punch", "slow", "none", "auto"},
        "sfx": set(sfx_mod.KINDS) | {"none", "auto"},
    }[field]
    for pair in pairs:
        if "=" not in pair:
            raise EditorError(f"S번호=값 형식으로 적어 주세요: {pair}")
        ref_s, value = pair.split("=", 1)
        values = [v.strip() for v in value.split(",") if v.strip()]
        bad = [v for v in values if v not in allowed]
        if bad or not values:
            raise EditorError(f"{field}에 쓸 수 있는 값: {', '.join(sorted(allowed))}")
        s = plan_mod.sentence(plan, ref_s)
        if field == "sfx" and values[0] not in ("auto", "none"):
            s["sfx"] = [{"type": v, "at": "start"} for v in values]
        else:
            s[field] = values[0]
        log(f"S{s['id']} {field} → {value}")
    _save(work, plan, env)


def cmd_review(ref: str) -> None:
    from . import plan as plan_mod, report

    work, plan, env = _load(ref)
    res = plan_mod.resolve(plan, env)
    log(report.review(plan, res, work / "review.md"))


def cmd_list() -> None:
    if not WORK_DIR.exists():
        log("작업이 없습니다.")
        return
    for item in sorted(WORK_DIR.glob("*/plan.json"), key=lambda p: p.stat().st_mtime, reverse=True):
        plan = read_json(item)
        done = (item.parent / "report.json").exists()
        info = read_json(item.parent / "report.json") if done else {}
        state = f"완성 {fmt_time(info['duration'])}" if done else "분석만 됨"
        log(f"- {plan['name']}: {Path(plan['source']['path']).name} ({fmt_time(plan['source']['duration'])}) · {state}")


def cmd_sounds() -> None:
    from . import sfx as sfx_mod
    from .media import write_wav

    folder = sfx_mod.SFX_DIR / "preview"
    sounds, _, origin = sfx_mod.load()
    for kind, x in sounds.items():
        write_wav(folder / f"{kind}.wav", x, SR)
        log(f"{sfx_mod.LABEL[kind]} ({kind}): {origin[kind]} → {folder / (kind + '.wav')}")


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(prog="reel", description="말하는 영상 → 릴스/쇼츠 자동 편집")
    sub = ap.add_subparsers(dest="cmd", required=True)

    def add_source(p):
        p.add_argument("video", nargs="?", help="영상 파일 (생략하면 reels/inbox의 가장 최근 영상)")
        p.add_argument("--script", help="대본 txt ([[강조]] 표시 가능)")
        p.add_argument("--srt", help="Vrew·CapCut 자막(SRT) — 음성 인식 대신 사용")
        p.add_argument("--words", help="단어 시간표 JSON [{w,s,e}]")
        p.add_argument("--model", help="음성 인식 모델 (기본 large-v3-turbo, 빠르게: small)")
        p.add_argument("--retranscribe", action="store_true", help="음성 인식 다시 하기")

    add_source(sub.add_parser("analyze", help="분석만"))
    p_edit = sub.add_parser("edit", help="분석 + 렌더")
    add_source(p_edit)
    p_edit.add_argument("--out", help="완성본 경로")
    p_render = sub.add_parser("render", help="렌더")
    p_render.add_argument("name", nargs="?", default="last")
    p_render.add_argument("--out", help="완성본 경로")
    p_set = sub.add_parser("set", help="설정 바꾸기")
    p_set.add_argument("name")
    p_set.add_argument("pairs", nargs="+")
    p_set.add_argument("--save", action="store_true", help="다음 영상에도 적용")
    p_fix = sub.add_parser("fix", help="자막 오타")
    p_fix.add_argument("name")
    p_fix.add_argument("old")
    p_fix.add_argument("new")
    for cmd in ("keep", "drop"):
        p = sub.add_parser(cmd)
        p.add_argument("name")
        p.add_argument("targets", nargs="+")
    for cmd in ("role", "zoom", "sfx"):
        p = sub.add_parser(cmd)
        p.add_argument("name")
        p.add_argument("pairs", nargs="+")
    p_title = sub.add_parser("title", help="상단 제목")
    p_title.add_argument("name")
    p_title.add_argument("text")
    p_review = sub.add_parser("review")
    p_review.add_argument("name", nargs="?", default="last")
    sub.add_parser("list")
    sub.add_parser("sounds")
    sub.add_parser("doctor")
    p_ref = sub.add_parser("ref", help="참고 영상 분석")
    p_ref.add_argument("video")
    p_story = sub.add_parser("story", help="모션그래픽 릴스 (목소리 + 카드)")
    p_story.add_argument("name", help="reels/stories/이름.json")
    p_story.add_argument("--voice", help="녹음 파일 (생략하면 inbox/이름.m4a 등, 없으면 컴퓨터 음성)")
    p_story.add_argument("--tts", action="store_true", help="녹음이 있어도 컴퓨터 음성으로 미리보기")
    p_story.add_argument("--engine", help="컴퓨터 음성 종류: say(Mac) sapi(Windows) edge espeak")
    p_story.add_argument("--model", help="음성 인식 모델")
    p_story.add_argument("--out", help="완성본 경로")

    args = ap.parse_args(argv)
    try:
        if args.cmd == "analyze":
            analyze(args.video, args.script, args.srt, args.words, args.model, args.retranscribe)
        elif args.cmd == "edit":
            work = analyze(args.video, args.script, args.srt, args.words, args.model, args.retranscribe)
            render(str(work), args.out)
        elif args.cmd == "render":
            render(args.name, args.out)
        elif args.cmd == "set":
            cmd_set(args.name, args.pairs, args.save)
        elif args.cmd == "fix":
            cmd_fix(args.name, args.old, args.new)
        elif args.cmd == "keep":
            cmd_keep(args.name, args.targets)
        elif args.cmd == "drop":
            cmd_drop(args.name, args.targets)
        elif args.cmd in ("role", "zoom", "sfx"):
            cmd_assign(args.name, args.cmd, args.pairs)
        elif args.cmd == "title":
            cmd_set(args.name, [f"title.text={args.text}"], False)
        elif args.cmd == "review":
            cmd_review(args.name)
        elif args.cmd == "list":
            cmd_list()
        elif args.cmd == "sounds":
            cmd_sounds()
        elif args.cmd == "doctor":
            from .doctor import doctor

            return doctor(log)
        elif args.cmd == "story":
            from .motion import render_story

            render_story(args.name, args.voice, args.tts, args.engine, args.out, args.model, log=log)
        elif args.cmd == "ref":
            from .reference import analyze_reference

            analyze_reference(Path(args.video).expanduser(), log=log)
    except EditorError as exc:
        print(f"\n오류: {exc}", file=sys.stderr)
        return 1
    return 0
