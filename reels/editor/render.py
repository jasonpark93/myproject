"""계획대로 완성본 만들기.

단계마다 결과를 캐시해서, 고친 부분과 관계있는 단계만 다시 만든다.
  컷 오디오 → 음량 정리 → (영상: 컷 + 줌 + 자막) → 효과음 믹스 → 합치기 → 스냅샷·보고서
예) '효과음 볼륨 절반' → 믹스와 합치기만 다시 (몇 초), '자막 20% 크게' → 영상만 다시.
"""

from __future__ import annotations

import math
import queue
import subprocess
import threading
import time
from pathlib import Path

import numpy as np

from . import FPS, H, ROOT, SR, W, audio, face as face_mod, graphics, sfx as sfx_mod, zoom
from .media import Probe, decode_audio, ffmpeg_features, sdr_filter, write_wav
from .plan import resolve
from .util import EditorError, file_hash, need, read_json, run, stable_hash, write_json

OUT_DIR = ROOT / "out"
MAX_WORK_PIXELS = 4_600_000
PIPELINE = "3"  # 처리 방식이 바뀌면 올려서 예전 캐시를 무효화한다


def _source_key(p: Probe) -> dict:
    path = Path(p.path)
    if not path.exists():
        raise EditorError(f"원본 영상이 없습니다(옮겼나요?): {p.path}")
    st = path.stat()
    return {"path": p.path, "size": st.st_size, "mtime": int(st.st_mtime)}


# ---------- 오디오 ----------
def _fade(n: int, length: int) -> np.ndarray:
    length = max(1, min(length, n // 2 or 1))
    return (0.5 - 0.5 * np.cos(np.linspace(0, np.pi, length))).astype(np.float32)


def cut_audio(src_audio: np.ndarray, segments: list, fade_ms: float = 12.0) -> np.ndarray:
    """세그먼트를 샘플 단위로 이어 붙이고 모든 이음매에 짧은 페이드를 준다 (딸깍 소리 방지)."""
    parts = []
    for k, seg in enumerate(segments):
        a, b = int(round(seg.a * SR)), int(round(seg.b * SR))
        piece = np.zeros(b - a, np.float32)
        lo, hi = max(a, 0), min(b, src_audio.size)
        if hi > lo:
            piece[lo - a : hi - a] = src_audio[lo:hi]
        fin = int(SR * (0.005 if k == 0 else fade_ms / 1000))
        fout = int(SR * (0.06 if k == len(segments) - 1 else fade_ms / 1000))
        if piece.size > 4:
            ramp = _fade(piece.size, fin)
            piece[: ramp.size] *= ramp
            ramp = _fade(piece.size, fout)
            piece[piece.size - ramp.size :] *= ramp[::-1]
        parts.append(piece)
    return np.concatenate(parts) if parts else np.zeros(0, np.float32)


# ---------- 영상 ----------
def work_size(p: Probe, zmax: float) -> tuple:
    """줌 최대치에서도 화질이 남도록 하되, 필요 이상 큰 원본(4K 등)은 미리 줄여 속도를 낸다."""
    dw, dh = p.width, p.height
    if dw / dh > 9 / 16:
        crop_h = dh
    else:
        crop_h = dw * 16 / 9
    f = min(1.0, (H * max(1.0, zmax)) / crop_h)
    if dw * dh * f * f > MAX_WORK_PIXELS:
        f = math.sqrt(MAX_WORK_PIXELS / (dw * dh))
    ww = max(2, int(round(dw * f / 2)) * 2)
    wh = max(2, int(round(dh * f / 2)) * 2)
    return ww, wh


class FrameReader:
    """ffmpeg가 30fps로 풀어 주는 원본 프레임을 별도 스레드에서 미리 읽는다."""

    def __init__(self, p: Probe, size: tuple, sdr: str):
        ww, wh = size
        chain = ",".join(f for f in (sdr, f"fps={FPS}", f"scale={ww}:{wh}:flags=bicubic", "format=bgr24") if f)
        cmd = [need("ffmpeg"), "-v", "error", "-nostdin", "-i", p.path, "-map", "0:v:0", "-vf", chain, "-f", "rawvideo", "-"]
        self.proc = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        self.shape = (wh, ww, 3)
        self.nbytes = ww * wh * 3
        self.q = queue.Queue(maxsize=8)
        self.thread = threading.Thread(target=self._pump, daemon=True)
        self.thread.start()
        self.index = -1
        self.frame = None
        self.done = False

    def _pump(self):
        while True:
            buf = bytearray(self.nbytes)
            view = memoryview(buf)
            got = 0
            while got < self.nbytes:
                n = self.proc.stdout.readinto(view[got:])
                if not n:
                    break
                got += n
            if got < self.nbytes:
                self.q.put(None)
                return
            self.q.put(np.frombuffer(buf, np.uint8).reshape(self.shape))

    def get(self, index: int) -> np.ndarray:
        while self.index < index and not self.done:
            item = self.q.get()
            if item is None:
                self.done = True
                break
            self.frame, self.index = item, self.index + 1
        if self.frame is None:
            raise EditorError("원본 영상 프레임을 읽지 못했습니다.")
        return self.frame

    def close(self):
        try:
            self.proc.stdout.close()
        except Exception:
            pass
        self.proc.kill()
        self.proc.wait()


def _anchors(res, p: Probe, face, size: tuple, n_frames: int):
    """프레임마다 (얼굴 x, 얼굴 y, 기본 화면 중심 x, 중심 y) — 작업 해상도 픽셀."""
    ww, wh = size
    crop_w = min(ww, wh * 9 / 16)
    arr = np.zeros((n_frames, 4), np.float64)
    tl = res.timeline
    default = (0.5, 0.42)
    for a, b in zoom.shots(res.bounds, tl.joins(), tl.duration):
        fx, fy = face_mod.median_in(face, tl.to_src(a), tl.to_src(max(a, b - 1e-3)), default)
        px, py = fx * ww, fy * wh
        c0x = min(max(px, crop_w / 2), ww - crop_w / 2)  # 가로 영상: 얼굴 중심으로 9:16 자르기
        c0y = wh / 2
        j0, j1 = int(round(a * FPS)), int(round(b * FPS))
        arr[j0:j1] = (px, py, c0x, c0y)
    if n_frames:
        empty = ~arr.any(axis=1)
        if empty.any():
            arr[empty] = (ww * default[0], wh * default[1], ww / 2, wh / 2)
    return arr


def _encoder(path: Path, crf: int, preset: str):
    if "libx264" not in ffmpeg_features()[1]:
        raise EditorError("ffmpeg에 H.264 인코더(libx264)가 없습니다. 공식 배포판 ffmpeg를 설치하세요.")
    cmd = [
        need("ffmpeg"), "-v", "error", "-nostdin", "-y",
        "-f", "rawvideo", "-pix_fmt", "bgr24", "-s", f"{W}x{H}", "-r", str(FPS), "-i", "-",
        "-vf", "scale=out_color_matrix=bt709:out_range=tv,format=yuv420p",
        "-c:v", "libx264", "-preset", preset, "-crf", str(crf), "-profile:v", "high",
        "-colorspace", "bt709", "-color_primaries", "bt709", "-color_trc", "bt709", "-color_range", "tv",
        "-movflags", "+faststart", "-an", path,
    ]
    return subprocess.Popen([str(c) for c in cmd], stdin=subprocess.PIPE, stderr=subprocess.PIPE)


SAFE_TOP = 250  # 인스타·유튜브 상단 UI(계정·메뉴)가 덮는 높이


def place_title(title, face, zmax: float, notes: list) -> None:
    """제목이 얼굴(머리 포함)을 가리지 않게 올리고, 그래도 겹치면 보고서에 알린다."""
    if not face or not face.get("t"):
        return
    fy = float(np.median(face["y"])) * H
    fh = float(np.median(face["h"])) * H
    head_top = fy - fh * 0.8  # 얼굴 상자 위로 머리카락까지
    head_top = fy - (fy - head_top) * zmax  # 펀치 줌일 때 더 위로 올라간다
    h = title.variants[1.0][1].shape[0]
    bottom_limit = head_top - 12
    if title.cy + h / 2 > bottom_limit:
        title.cy = max(SAFE_TOP + h / 2, bottom_limit - h / 2)
        if title.cy + h / 2 > bottom_limit + 4:
            notes.append("제목이 머리를 조금 가립니다. 촬영할 때 머리 위에 손바닥 하나만큼 여백을 두면 해결됩니다 (또는 title.size=0.85로 줄이기).")


def render_video(res, p: Probe, face, out: Path, log=print) -> None:
    import cv2

    tl = res.timeline
    n_frames = int(round(tl.duration * FPS))
    zcfg = res.settings["zoom"]
    z, _ = zoom.curve(res.sentences, [s["kind"] for s in res.sentences], res.bounds, tl.joins(), n_frames, zcfg)
    size = work_size(p, float(z.max()) if n_frames else 1.0)
    ww, wh = size
    anchors = _anchors(res, p, face, size, n_frames)
    crop_h = min(wh, ww * 16 / 9)

    ccfg = dict(res.settings["caption"])
    ccfg["center_y"] = H * (1 - float(ccfg["bottom"]))
    sprites = [graphics.caption_sprite(c["text"], c["hl"], ccfg, pop=bool(ccfg.get("pop", True))) for c in res.captions]
    shows = [(int(round(c["show"][0] * FPS)), int(round(c["show"][1] * FPS))) for c in res.captions]
    tcfg = dict(res.settings["title"])
    title = None
    if (tcfg.get("text") or "").strip():
        tcfg["center_y"] = H * float(tcfg.get("top", 0.19))
        title = graphics.title_sprite(tcfg["text"], tcfg)
        place_title(title, face, float(z.max()) if n_frames else 1.0, res.notes)
        title_end = n_frames if not tcfg.get("duration") else int(round(float(tcfg["duration"]) * FPS))

    sdr, warn = sdr_filter(p)
    if warn:
        log(warn)
    v_off = p.video_start - p.audio_start
    reader = FrameReader(p, size, sdr)
    enc = _encoder(out, int(res.settings["video"]["crf"]), str(res.settings["video"]["preset"]))
    starts = tl.starts
    k = 0
    cap_i = 0
    t0 = time.time()
    try:
        for j in range(n_frames):
            t = j / FPS
            while k + 1 < len(starts) and t >= starts[k + 1] - 1e-9:
                k += 1
            seg = tl.segments[k]
            src_t = seg.a + (t - starts[k])
            frame = reader.get(max(0, int(round((src_t - v_off) * FPS))))

            zj = float(z[j])
            fx, fy, c0x, c0y = anchors[j]
            win_h = crop_h / zj
            win_w = win_h * 9 / 16
            cx = fx - (fx - c0x) / zj  # 얼굴이 화면에서 같은 자리에 머물도록 얼굴을 중심으로 확대
            cy = fy - (fy - c0y) / zj
            cx = min(max(cx, win_w / 2), ww - win_w / 2)
            cy = min(max(cy, win_h / 2), wh - win_h / 2)
            s = win_h / H
            m = np.array([[s, 0.0, cx - s * W / 2 + 0.5 * s - 0.5], [0.0, s, cy - s * H / 2 + 0.5 * s - 0.5]])
            interp = cv2.INTER_LINEAR if s >= 0.85 else cv2.INTER_CUBIC
            img = cv2.warpAffine(frame, m, (W, H), flags=interp | cv2.WARP_INVERSE_MAP, borderMode=cv2.BORDER_REPLICATE)

            if title is not None and j < title_end:
                title.draw(img)
            while cap_i < len(shows) and shows[cap_i][1] <= j:
                cap_i += 1
            for c in range(cap_i, min(cap_i + 2, len(shows))):
                a, b = shows[c]
                if a <= j < b:
                    sprites[c].draw(img, j - a)
            enc.stdin.write(img.tobytes())
            if j and j % (FPS * 10) == 0:
                log(f"  영상 {j / FPS:.0f}/{tl.duration:.0f}초 ({j / max(1e-6, time.time() - t0):.0f}fps)")
        enc.stdin.close()
        err = enc.stderr.read().decode("utf-8", "replace")
        if enc.wait() != 0:
            raise EditorError("영상 인코딩 실패:\n" + "\n".join(err.strip().splitlines()[-8:]))
    except BrokenPipeError:
        err = enc.stderr.read().decode("utf-8", "replace")
        raise EditorError("영상 인코딩 실패:\n" + "\n".join(err.strip().splitlines()[-8:]))
    finally:
        reader.close()
        if enc.poll() is None:
            enc.kill()


# ---------- 합치기 ----------
def mux(video: Path, mix_wav: Path, out: Path) -> None:
    out.parent.mkdir(parents=True, exist_ok=True)
    tmp = out.with_name(out.stem + ".tmp.mp4")
    run([
        need("ffmpeg"), "-v", "error", "-nostdin", "-y", "-i", video, "-i", mix_wav,
        "-map", "0:v:0", "-map", "1:a:0", "-c:v", "copy",
        "-af", audio.limiter(0.84), "-c:a", "aac", "-b:a", "192k", "-ar", str(SR), "-ac", "2",
        "-shortest", "-movflags", "+faststart", tmp,
    ])
    tmp.replace(out)


def snapshots(video: Path, duration: float, folder: Path) -> list:
    """0초·중간·끝 3장 + 한 장으로 모은 미리보기."""
    folder.mkdir(parents=True, exist_ok=True)
    times = [("start", 0.0), ("middle", duration / 2), ("end", max(0.0, duration - 0.2))]
    paths = []
    for label, t in times:
        path = folder / f"snap_{label}.png"
        run([need("ffmpeg"), "-v", "error", "-nostdin", "-y", "-ss", f"{t:.3f}", "-i", video, "-frames:v", "1", "-update", "1", path])
        paths.append((label, t, path))
    sheet = folder / "snapshots.jpg"
    run([
        need("ffmpeg"), "-v", "error", "-nostdin", "-y",
        *sum((["-i", str(p)] for _, _, p in paths), []),
        "-filter_complex", "[0]scale=360:640[a];[1]scale=360:640[b];[2]scale=360:640[c];[a][b][c]hstack=inputs=3",
        "-frames:v", "1", "-update", "1", "-q:v", "3", sheet,
    ])
    return paths + [("sheet", None, sheet)]


# ---------- 전체 ----------
def render(work: Path, log=print, out_path: Path | None = None) -> dict:
    from .audio import Envelope

    work = Path(work)
    plan = read_json(work / "plan.json")
    p = Probe.from_dict(plan["source"])
    env = Envelope.load(work / "envelope.npz")
    face = read_json(work / "faces.json") if (work / "faces.json").exists() else None
    res = resolve(plan, env)
    cache = work / "cache"
    cache.mkdir(exist_ok=True)
    s = res.settings
    timings = {}

    seg_list = [[round(x.a, 4), round(x.b, 4)] for x in res.segments]
    k_cut = stable_hash(_source_key(p), seg_list)
    k_voice = stable_hash(k_cut, s["audio"], audio.VOICE_CHAIN, PIPELINE)
    font_key = file_hash(graphics.BUNDLED) if graphics.BUNDLED.exists() else ""
    k_video = stable_hash(
        k_cut,
        [(x["id"], x["kind"], round(x["start"], 3), round(x["end"], 3)) for x in res.sentences],
        [round(b, 3) for b in res.bounds],
        s["zoom"], s["caption"], s["title"], s["video"],
        [(c["text"], c["hl"], c["show"]) for c in res.captions],
        face, font_key, p.color_transfer, PIPELINE,
    )
    sounds_dir = sfx_mod.SFX_DIR
    sfx_files = {k: file_hash(f) for k in sfx_mod.KINDS if (f := sfx_mod.user_file(k, sounds_dir))}
    k_mix = stable_hash(k_voice, [(e["type"], e["t"]) for e in res.sfx_events], s["sfx"], sfx_files, PIPELINE)
    k_final = stable_hash(k_video, k_mix)

    out_path = Path(out_path) if out_path else OUT_DIR / f"{plan['name']}.mp4"
    marker = work / "final.json"
    if out_path.exists() and marker.exists() and read_json(marker).get("key") == k_final:
        log("바뀐 것이 없어 기존 완성본을 그대로 씁니다.")
        side = cache / f"video-{k_video}.json"
        if side.exists():
            res.notes.extend(read_json(side).get("notes") or [])
        return _finish(work, plan, res, out_path, timings, cached=True, log=log)

    voice_raw = cache / f"voice-{k_cut}.wav"
    voice = cache / f"voicenorm-{k_voice}.wav"
    if not voice.exists():
        t = time.time()
        if not voice_raw.exists():
            log("소리 자르는 중…")
            src = decode_audio(Path(p.path), duration=p.duration)
            write_wav(voice_raw, cut_audio(src, res.segments))
        log("목소리 음량 맞추는 중…")
        stats = audio.normalize_voice(voice_raw, voice, float(s["audio"]["loudness"]), float(s["audio"]["true_peak"]))
        write_json(cache / f"voicenorm-{k_voice}.json", stats)
        timings["음량"] = time.time() - t
    else:
        log("목소리: 이전 결과 재사용")

    video = cache / f"video-{k_video}.mp4"
    if not video.exists():
        t = time.time()
        log(f"영상 만드는 중… (컷 {len(res.segments) - 1}곳, 자막 {len(res.captions)}줄)")
        tmp = cache / f"video-{k_video}.tmp.mp4"
        render_video(res, p, face, tmp, log=log)
        tmp.replace(video)
        write_json(cache / f"video-{k_video}.json", {"notes": res.notes})
        timings["영상"] = time.time() - t
    else:
        log("영상: 이전 결과 재사용")
        side = cache / f"video-{k_video}.json"
        if side.exists():
            res.notes.extend(read_json(side).get("notes") or [])

    mix_path = cache / f"mix-{k_mix}.wav"
    if not mix_path.exists():
        t = time.time()
        voice_samples = decode_audio(voice)
        sounds, hits, origin = sfx_mod.load(sounds_dir)
        level = audio.rms_db(voice_samples)
        gains = {k: sfx_mod.gain_db(sounds[k], k, level) for k in sfx_mod.KINDS}
        events = [dict(e, start=e["t"] - hits[e["type"]]) for e in res.sfx_events]
        write_wav(mix_path, audio.mix(voice_samples, events, sounds, gains, float(s["sfx"]["volume"])))
        write_json(cache / f"mix-{k_mix}.json", {"origin": origin, "gains_db": {k: round(v, 1) for k, v in gains.items()}})
        timings["효과음"] = time.time() - t
    log("합치는 중…")
    mux(video, mix_path, out_path)
    write_json(marker, {"key": k_final, "video": video.name, "mix": mix_path.name, "out": str(out_path)})
    _prune(cache, {voice_raw.name, voice.name, video.name, mix_path.name})
    return _finish(work, plan, res, out_path, timings, cached=False, log=log)


def _prune(cache: Path, keep: set, per_kind: int = 2) -> None:
    """캐시는 종류별로 최근 것 몇 개만 남긴다 (디스크 절약)."""
    groups = {}
    for f in cache.iterdir():
        kind = f.name.split("-", 1)[0]
        groups.setdefault(kind, []).append(f)
    for kind, files in groups.items():
        files.sort(key=lambda f: f.stat().st_mtime, reverse=True)
        stems = []
        for f in files:
            stem = f.name.rsplit(".", 1)[0]
            if f.name in keep or stem in [k.rsplit(".", 1)[0] for k in keep]:
                continue
            if stem not in stems:
                stems.append(stem)
            if len(stems) > per_kind:
                f.unlink(missing_ok=True)


def _finish(work: Path, plan: dict, res, out_path: Path, timings: dict, cached: bool, log=print) -> dict:
    from . import report

    snaps = snapshots(out_path, res.duration, work / "snapshots")
    loud = audio.measure_loudness(out_path)
    info = report.write(work, plan, res, out_path, snaps, loud, timings)
    log(f"완성: {out_path}")
    return info
