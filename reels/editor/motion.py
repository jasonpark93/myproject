"""모션그래픽 릴스 렌더: 배경 + 상단 고정 제목 + 장면 카드 + 자막 + 목소리·효과음(+배경음악)."""

from __future__ import annotations

import time
from pathlib import Path

import numpy as np

from . import FPS, H, ROOT, SR, W, audio, cards, graphics, sfx as sfx_mod, story
from .media import decode_audio, write_wav
from .render import _encoder, mux, snapshots
from .util import EditorError, fmt_time, read_json, stable_hash, write_json, write_text

OUT_DIR = ROOT / "out"
WORK_DIR = ROOT / "work"
BGM_DIR = ROOT / "bgm"
CARD_CY = 830  # 카드 중심 높이
CARD_MAX = (980, 820)
CAPTION_BOTTOM = 0.27  # 참고 릴스와 같은 높이 (화면 아래에서 27%)
PIPELINE = "m2"  # 카드·렌더 방식이 바뀌면 올린다 (예전 영상 캐시 무효화)


def background(th: dict) -> np.ndarray:
    """어두운 그라데이션 + 제목 뒤 연두 빛 + 아래쪽 보랏빛 + 가장자리 어둡게."""
    yy, xx = np.mgrid[0:H, 0:W].astype(np.float32)
    top, bottom = np.array(th["bg_top"], np.float32), np.array(th["bg_bottom"], np.float32)
    f = (yy / H)[..., None]
    img = top * (1 - f) + bottom * f
    glow = np.exp(-(((xx - W / 2) / 620) ** 2 + ((yy - 300) / 380) ** 2))[..., None]
    img += glow * np.array(th["glow"], np.float32) * 0.13
    low = np.exp(-(((xx - W * 0.35) / 700) ** 2 + ((yy - 1650) / 420) ** 2))[..., None]
    img += low * np.array([70, 50, 140], np.float32) * 0.10
    vig = 1 - 0.35 * (((xx - W / 2) / (W * 0.75)) ** 2 + ((yy - H / 2) / (H * 0.7)) ** 2)
    img *= np.clip(vig, 0.55, 1)[..., None]
    return np.ascontiguousarray(img[..., ::-1]).clip(0, 255).astype(np.uint8)  # BGR


def put_u8(frame: np.ndarray, img: cards.Img, x: float, y: float, alpha: float = 1.0) -> None:
    if alpha <= 0.003:
        return
    x0, y0 = int(round(x)), int(round(y))
    xa, ya = max(0, x0), max(0, y0)
    xb, yb = min(W, x0 + img.w), min(H, y0 + img.h)
    if xb <= xa or yb <= ya:
        return
    sp = img.pm[ya - y0 : yb - y0, xa - x0 : xb - x0]
    sa = img.a[ya - y0 : yb - y0, xa - x0 : xb - x0]
    region = frame[ya:yb, xa:xb].astype(np.float32)
    if alpha < 0.999:
        region = region * (1 - sa * alpha) + sp * alpha
    else:
        region = region * (1 - sa) + sp
    frame[ya:yb, xa:xb] = (region + 0.5).clip(0, 255).astype(np.uint8)


class FrameTarget:
    """카드 위에 얹는 것(꼬리표·도장)을 프레임에 바로 그리기 위한 어댑터."""

    def __init__(self, frame: np.ndarray):
        self.frame = frame

    def put(self, img, x, y, alpha=1.0, scale=1.0, anchor="tl"):
        if scale != 1.0:
            img2 = img.scaled(scale)
            x += (img.w - img2.w) / 2
            y += (img.h - img2.h) / 2
            img = img2
        put_u8(self.frame, img, x, y, alpha)


def header_images(spec: dict, th: dict):
    head = spec.get("header") or {}
    lab = None
    if head.get("label"):
        txt = cards.text(head["label"], 46, th, max_w=900)
        c = cards.Canvas(txt.w + 36, txt.h + 6)
        c.put(cards.rounded(txt.w + 36, txt.h + 6, 10, (0, 0, 0), alpha=0.82), 0, 0)
        c.put(txt, 18, 3)
        lab = c.img()
    title = cards.hero(head["title"], int(head.get("size", 118)), th, max_w=1010) if head.get("title") else None
    return lab, title


def _fit(img: cards.Img) -> float:
    return min(1.0, CARD_MAX[0] / img.w, CARD_MAX[1] / img.h)


def build_scenes(spec: dict, th: dict, timings: list, base_dir: Path):
    out = []
    for sc, tm in zip(spec["scenes"], timings):
        card = cards.make_card(sc.get("card") or {"type": "text", "title": cards.plain(sc["say"])[:20]}, th, tm, base_dir)
        extras = []
        if sc.get("pills"):
            extras.append(cards.Pills(sc["pills"], th, tm))
        if sc.get("stamp"):
            extras.append(cards.Stamp(sc["stamp"] if isinstance(sc["stamp"], dict) else {"text": str(sc["stamp"])}, th, tm))
        out.append((card, extras, tm))
    return out


def sfx_plan(scenes: list, max_per_10s: int) -> list:
    events = []
    for k, (card, extras, tm) in enumerate(scenes):
        if k > 0:
            events.append({"type": "whoosh", "t": round(tm.start, 3), "why": f"장면 {k + 1}"})
        for obj in [card] + extras:
            for kind, local in getattr(obj, "sfx_events", []):
                t = tm.start + local
                if tm.start <= t < tm.end - 0.05:
                    events.append({"type": kind, "t": round(t, 3), "why": f"장면 {k + 1} 애니메이션"})
    return sfx_mod.thin(events, max_per_10s, min_gap=0.35)


def render_video(spec: dict, th: dict, scenes: list, caps: list, duration: float, out: Path, log=print) -> None:
    n = int(round(duration * FPS))
    bg = background(th)
    lab, title = header_images(spec, th)
    ccfg = {"size": 1.1, "center_y": H * (1 - CAPTION_BOTTOM), "highlight": "#%02X%02X%02X" % th["accent"], "max_width": 900}
    sprites = [graphics.caption_sprite(c["text"], c["hl"], ccfg) for c in caps]
    shows = [(int(round(c["show"][0] * FPS)), int(round(c["show"][1] * FPS))) for c in caps]
    enc = _encoder(out, 18, "fast")
    t0 = time.time()
    k = 0
    cap_i = 0
    prev_img = None
    try:
        for j in range(n):
            t = j / FPS
            while k + 1 < len(scenes) and t >= scenes[k + 1][2].start - 1e-9:
                prev_img = scenes[k][0].frame(max(0.0, t - scenes[k][2].start))
                k += 1
            frame = bg.copy()
            # 상단 제목 (처음에만 톡 튀어나옴)
            s, a = cards.pop(t, 0.3, 0.85)
            for img, cy in ((lab, 150), (title, 258)):
                if img is not None:
                    im = img.scaled(s) if s != 1.0 else img
                    put_u8(frame, im, (W - im.w) / 2, cy - im.h / 2, a)
            card, extras, tm = scenes[k]
            local = t - tm.start
            img = card.frame(local)
            fit = _fit(img)
            # 이전 카드는 흐려지며 사라지고, 새 카드는 흐림에서 또렷하게 커지며 등장
            if prev_img is not None and local < 0.25 and k > 0:
                q = local / 0.25
                old = prev_img.scaled(_fit(prev_img)).blurred(2 + 14 * q)
                put_u8(frame, old, (W - old.w) / 2, CARD_CY - old.h / 2, 1 - q)
            if k > 0 and local < 0.3:
                q = cards.ease_out_back(local / 0.3, 1.2)
                blur = 10 * (1 - min(1.0, local / 0.2))
                scale = fit * (0.92 + 0.08 * q)
                im = img.scaled(scale)
                im = im.blurred(blur) if blur > 0.5 else im
                put_u8(frame, im, (W - im.w) / 2, CARD_CY - im.h / 2, min(1.0, local / 0.15))
            else:
                im = img.scaled(fit) if fit != 1.0 else img
                put_u8(frame, im, (W - im.w) / 2, CARD_CY - im.h / 2)
            box_w, box_h = img.w * fit, img.h * fit
            target = FrameTarget(frame)
            for extra in extras:
                extra.draw(target, ((W - box_w) / 2, CARD_CY - box_h / 2, box_w, box_h), local)
            while cap_i < len(shows) and shows[cap_i][1] <= j:
                cap_i += 1
            for c in range(cap_i, min(cap_i + 2, len(shows))):
                a_, b_ = shows[c]
                if a_ <= j < b_:
                    sprites[c].draw(frame, j - a_)
            enc.stdin.write(frame.tobytes())
            if j and j % (FPS * 10) == 0:
                log(f"  영상 {j / FPS:.0f}/{duration:.0f}초 ({j / max(1e-6, time.time() - t0):.0f}fps)")
        enc.stdin.close()
        err = enc.stderr.read().decode("utf-8", "replace")
        if enc.wait() != 0:
            raise EditorError("영상 인코딩 실패:\n" + "\n".join(err.strip().splitlines()[-8:]))
    except BrokenPipeError:
        err = enc.stderr.read().decode("utf-8", "replace")
        raise EditorError("영상 인코딩 실패:\n" + "\n".join(err.strip().splitlines()[-8:]))
    finally:
        if enc.poll() is None:
            enc.kill()


def bgm_track(spec: dict, base_dir: Path, duration: float, voice_rms_db: float):
    path = None
    if spec.get("bgm"):
        path = Path(spec["bgm"])
        if not path.is_absolute():
            path = base_dir / path
    elif BGM_DIR.exists():
        files = sorted(p for p in BGM_DIR.iterdir() if p.suffix.lower() in (".mp3", ".m4a", ".wav", ".aac", ".ogg", ".flac"))
        path = files[0] if files else None
    if path is None or not Path(path).exists() or spec.get("bgm") is False:
        return None, None
    x = decode_audio(Path(path))
    n = int(duration * SR)
    if x.size == 0:
        return None, None
    reps = int(np.ceil(n / x.size))
    x = np.tile(x, reps)[:n]
    gain = 10 ** ((voice_rms_db - 21 - audio.rms_db(x)) / 20)  # 목소리보다 21dB 작게 (참고 릴스 수준)
    fade_in, fade_out = int(0.4 * SR), int(1.5 * SR)
    env = np.ones(n, np.float32)
    env[:fade_in] = np.linspace(0, 1, min(fade_in, n))
    env[-fade_out:] = np.linspace(1, 0, min(fade_out, n))
    return (x * gain * env).astype(np.float32), Path(path).name


def render_story(ref: str, voice: str | None = None, use_tts: bool = False, engine: str | None = None, out: str | None = None,
                 model: str | None = None, log=print) -> dict:
    from .plan import DEFAULTS

    path = story.find(ref)
    spec = story.load(path)
    name = spec["name"]
    th = cards.theme(spec.get("theme", "neon"))
    work = WORK_DIR / f"story-{name}"
    work.mkdir(parents=True, exist_ok=True)
    voice_path = None
    if voice:
        voice_path = Path(voice).expanduser().resolve()
    elif not use_tts:
        for ext in (".m4a", ".mp3", ".wav", ".aac", ".mp4", ".mov", ".caf", ".amr", ".3gp", ".ogg"):
            cand = ROOT / "inbox" / f"{name}{ext}"
            if cand.exists():
                voice_path = cand.resolve()
                break
    if voice_path is not None and not voice_path.exists():
        raise EditorError(f"녹음 파일이 없습니다: {voice_path}")
    settings = dict(DEFAULTS)
    settings.update(spec.get("settings") or {})
    key = story.narration_key(spec, voice_path, engine)
    nar_audio = work / f"narration-{key}.wav"
    nar_meta = work / f"narration-{key}.json"
    if nar_audio.exists() and nar_meta.exists():
        meta = read_json(nar_meta)
        nar = story.Narration(audio=decode_audio(nar_audio), words=meta["words"], source=meta["source"], cuts=meta.get("cuts", []), stats=meta.get("stats", {}))
        log(f"목소리: 이전 결과 재사용 ({nar.source})")
    else:
        if voice_path is not None:
            nar = story.from_recording(spec, voice_path, work, settings, model, log)
        else:
            log("녹음 파일이 없어 컴퓨터 음성으로 미리보기를 만듭니다. (올릴 영상은 직접 녹음 권장)")
            nar = story.from_tts(spec, engine, log=log)
        write_wav(nar_audio, nar.audio)
        write_json(nar_meta, {"words": nar.words, "source": nar.source, "cuts": nar.cuts, "stats": nar.stats})
    timings = story.schedule(spec, nar)
    scenes = build_scenes(spec, th, timings, path.parent)
    caps = story.caption_list(spec, nar, int(settings["caption"]["max_chars"]))
    sfx_cfg = {"enabled": True, "volume": 1.0, "max_per_10s": 4}  # 모션 영상은 움직임이 많아 10초에 4개까지
    sfx_cfg.update((spec.get("settings") or {}).get("sfx") or {})
    events = sfx_plan(scenes, int(sfx_cfg["max_per_10s"])) if sfx_cfg.get("enabled", True) else []

    video = work / f"video-{stable_hash(PIPELINE, spec, nar.words, nar.duration)}.mp4"
    timings_info = {}
    if not video.exists():
        t = time.time()
        log(f"영상 만드는 중… (장면 {len(scenes)}개, 자막 {len(caps)}줄, {fmt_time(nar.duration)})")
        tmp = video.with_suffix(".tmp.mp4")
        render_video(spec, th, scenes, caps, nar.duration, tmp, log=log)
        tmp.replace(video)
        timings_info["영상"] = time.time() - t
    else:
        log("영상: 이전 결과 재사용")
    voice_norm = work / f"voicenorm-{key}.wav"
    if not voice_norm.exists():
        audio.normalize_voice(nar_audio, voice_norm, float(settings["audio"]["loudness"]), float(settings["audio"]["true_peak"]))
    voice_samples = decode_audio(voice_norm)
    sounds, hits, origin = sfx_mod.load()
    level = audio.rms_db(voice_samples)
    gains = {k: sfx_mod.gain_db(sounds[k], k, level) for k in sfx_mod.KINDS}
    mixed = audio.mix(voice_samples, [dict(e, start=e["t"] - hits[e["type"]]) for e in events], sounds, gains, float(sfx_cfg.get("volume", 1.0)))
    bgm, bgm_name = bgm_track(spec, path.parent, nar.duration, level)
    if bgm is not None:
        mixed[: bgm.size] += bgm[: mixed.size]
    mix_path = work / "mix.wav"
    write_wav(mix_path, mixed)
    out_path = Path(out).expanduser() if out else OUT_DIR / f"{name}.mp4"
    log("합치는 중…")
    mux(video, mix_path, out_path)
    snaps = snapshots(out_path, nar.duration, work / "snapshots")
    sheet = scene_sheet(out_path, timings, work / "snapshots" / "scenes.jpg")
    loud = audio.measure_loudness(out_path)
    upload = story.upload_text(spec)
    if upload:
        write_text(work / "upload.md", upload)
    report = write_report(work, spec, nar, timings, caps, events, out_path, snaps, sheet, loud, bgm_name, origin)
    if upload:
        report += f"\n업로드 문구(제목·설명·해시태그): `{work / 'upload.md'}`\n"
    log(f"완성: {out_path}")
    log("")
    log(report)
    return {"out": str(out_path), "duration": nar.duration, "scenes": len(scenes), "captions": len(caps), "sfx": len(events), "loudness": loud, "report": report}


def scene_sheet(video: Path, timings: list, out: Path) -> Path:
    """장면마다 가운데 순간 한 장씩 모은 이미지 (Claude가 한 번에 보고 점검)."""
    from .util import need, run

    frames = []
    for tm in timings:
        t = tm.start + min(tm.duration * 0.6, max(0.4, tm.duration - 0.3))
        data = run([need("ffmpeg"), "-v", "error", "-nostdin", "-ss", f"{t:.2f}", "-i", str(video), "-frames:v", "1", "-vf", "scale=270:480", "-f", "rawvideo", "-pix_fmt", "rgb24", "-"]).stdout
        if len(data) == 270 * 480 * 3:
            frames.append(np.frombuffer(data, np.uint8).reshape(480, 270, 3))
    if not frames:
        return out
    from PIL import Image

    cols = min(6, len(frames))
    rows = (len(frames) + cols - 1) // cols
    sheet = Image.new("RGB", (270 * cols, 480 * rows))
    for i, f in enumerate(frames):
        sheet.paste(Image.fromarray(f), ((i % cols) * 270, (i // cols) * 480))
    out.parent.mkdir(parents=True, exist_ok=True)
    sheet.save(out, quality=88)
    return out


def write_report(work: Path, spec: dict, nar, timings: list, caps: list, events: list, out_path: Path, snaps: list, sheet: Path, loud, bgm_name, origin) -> str:
    lines = [
        f"# 모션 릴스 보고서 — {spec['name']}",
        "",
        f"- 완성본: `{out_path}`",
        f"- 길이: {fmt_time(nar.duration)} · 장면 {len(timings)}개 · 자막 {len(caps)}줄 · 효과음 {len(events)}개" + (f" · 음량 {loud:.1f} LUFS" if loud is not None else ""),
        f"- 목소리: {nar.source}" + (f" (컷 {len(nar.cuts)}곳)" if nar.cuts else ""),
        f"- 배경음악: {bgm_name or '없음 (인스타에서 올릴 때 음악 추가 가능)'}",
        "",
        "## 장면",
        "",
        "| # | 시간 | 카드 | 말 |",
        "| --- | --- | --- | --- |",
    ]
    for k, (sc, tm) in enumerate(zip(spec["scenes"], timings), 1):
        kind = (sc.get("card") or {}).get("type", "text")
        lines.append(f"| {k} | {fmt_time(tm.start)} | {kind} | {cards.plain(sc['say'])} |")
    lines += ["", "## 자막 전문 (오타 확인용)", "", "| 시간 | 자막 |", "| --- | --- |"]
    for c in caps:
        txt = c["text"]
        for h in c["hl"]:
            txt = txt.replace(h, f"[[{h}]]", 1)
        lines.append(f"| {fmt_time(c['t0'])} | {txt} |")
    lines += ["", "## 효과음", ""]
    for e in events:
        lines.append(f"- {fmt_time(e['t'])} {sfx_mod.LABEL[e['type']]} — {e['why']}")
    lines += ["", "## 스냅샷", ""]
    for label_, t, p in snaps:
        lines.append(f"- {label_}: `{p}`")
    lines.append(f"- 장면별 모음: `{sheet}`")
    text = "\n".join(lines) + "\n"
    write_text(work / "report.md", text)
    return text
