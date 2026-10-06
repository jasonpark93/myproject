"""모션그래픽 릴스 렌더: 배경 + 상단 고정 제목 + 장면 카드 + 자막 + 목소리·효과음(+배경음악).

화면 꾸미기(Look)는 테마마다 다르다:
- NoteLook(기본): 모눈 노트 배경 · 채널 이름표 + 제목 형광펜 · 장면 진행 막대 · 종이 카드(그림자·테이프·살짝 기울임)
  · 카드가 오른쪽에서 밀려 들어오고 왼쪽으로 빠짐 · 먹색 라벨 자막
- NeonLook: 참고 릴스와 비슷한 검정 배경 · 가운데 제목 · 흐림 전환 · 흰 글씨 자막
"""

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
NOTE_CARD_CY = 872  # 노트 테마는 위 제목 영역이 조금 더 크다
CARD_MAX = (980, 820)
CAPTION_BOTTOM = 0.27  # 참고 릴스와 같은 높이 (화면 아래에서 27%)
PIPELINE = "m3"  # 카드·렌더 방식이 바뀌면 올린다 (예전 영상 캐시 무효화)
TILTS = (-1.2, 0.9, -0.7, 1.1)  # 종이 카드 기울기 (장면마다 돌아가며)


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


def note_background(th: dict) -> np.ndarray:
    """모눈 노트: 따뜻한 종이색 + 옅은 모눈 + 왼쪽 스프링 구멍 + 종이 결 + 가장자리 살짝 어둡게."""
    yy, xx = np.mgrid[0:H, 0:W].astype(np.float32)
    top, bottom = np.array(th["bg_top"], np.float32), np.array(th["bg_bottom"], np.float32)
    f = (yy / H)[..., None]
    img = top * (1 - f) + bottom * f
    grid = np.array(th.get("grid", (226, 216, 192)), np.float32)
    cell = 54
    gx = (xx - 6) % cell < 2
    gy = (yy - 10) % cell < 2
    line = (gx | gy).astype(np.float32)[..., None] * 0.55
    img = img * (1 - line) + grid * line
    rng = np.random.default_rng(3)
    img += rng.normal(0, 2.2, (H, W, 1)).astype(np.float32)  # 종이 결
    for y in range(330, H - 200, 96):  # 스프링 노트 구멍
        sub = img[y - 16 : y + 17, 10:43]
        d = (xx[y - 16 : y + 17, 10:43] - 26) ** 2 + (yy[y - 16 : y + 17, 10:43] - y) ** 2
        sub[d < 15**2] *= 0.93
        sub[d < 12**2] = (212, 202, 182)
    vig = 1 - 0.10 * (((xx - W / 2) / (W * 0.62)) ** 2 + ((yy - H / 2) / (H * 0.62)) ** 2)
    img *= np.clip(vig, 0.86, 1)[..., None]
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
    from .notecards import Stickers

    out = []
    for sc, tm in zip(spec["scenes"], timings):
        card = cards.make_card(sc.get("card") or {"type": "text", "title": cards.plain(sc["say"])[:20]}, th, tm, base_dir)
        extras = []
        if sc.get("pills"):
            extras.append(cards.Pills(sc["pills"], th, tm))
        if sc.get("stickers"):
            extras.append(Stickers(sc["stickers"], th, tm))
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


class Look:
    """테마별 화면 꾸미기의 공통 부분: 장면 번호 따라가기 · 자막 그리기."""

    def __init__(self, spec: dict, th: dict, scenes: list, caps: list):
        self.spec, self.th, self.scenes = spec, th, scenes
        self.k = 0
        self.cap_i = 0
        self.sprites = [self.caption_sprite(c) for c in caps]
        self.shows = [(int(round(c["show"][0] * FPS)), int(round(c["show"][1] * FPS))) for c in caps]
        self.bg = self.background()

    def background(self) -> np.ndarray:
        return background(self.th)

    def caption_sprite(self, cap: dict):
        ccfg = {"size": 1.1, "center_y": H * (1 - CAPTION_BOTTOM), "highlight": "#%02X%02X%02X" % self.th["accent"], "max_width": 900}
        return graphics.caption_sprite(cap["text"], cap["hl"], ccfg)

    def advance(self, t: float) -> bool:
        changed = False
        while self.k + 1 < len(self.scenes) and t >= self.scenes[self.k + 1][2].start - 1e-9:
            self.leave(t)
            self.k += 1
            changed = True
        return changed

    def leave(self, t: float) -> None:
        pass

    def captions(self, frame: np.ndarray, j: int) -> None:
        while self.cap_i < len(self.shows) and self.shows[self.cap_i][1] <= j:
            self.cap_i += 1
        for c in range(self.cap_i, min(self.cap_i + 2, len(self.shows))):
            a_, b_ = self.shows[c]
            if a_ <= j < b_:
                self.sprites[c].draw(frame, j - a_)


class NeonLook(Look):
    """참고 릴스 스타일: 가운데 제목, 흐림 전환."""

    def __init__(self, spec, th, scenes, caps):
        super().__init__(spec, th, scenes, caps)
        self.lab, self.title = header_images(spec, th)
        self.prev_img = None

    def leave(self, t):
        card, _, tm = self.scenes[self.k]
        self.prev_img = card.frame(max(0.0, t - tm.start))

    def frame(self, j: int) -> np.ndarray:
        t = j / FPS
        self.advance(t)
        k = self.k
        frame = self.bg.copy()
        # 상단 제목 (처음에만 톡 튀어나옴)
        s, a = cards.pop(t, 0.3, 0.85)
        for img, cy in ((self.lab, 150), (self.title, 258)):
            if img is not None:
                im = img.scaled(s) if s != 1.0 else img
                put_u8(frame, im, (W - im.w) / 2, cy - im.h / 2, a)
        card, extras, tm = self.scenes[k]
        local = t - tm.start
        img = card.frame(local)
        fit = _fit(img)
        # 이전 카드는 흐려지며 사라지고, 새 카드는 흐림에서 또렷하게 커지며 등장
        if self.prev_img is not None and local < 0.25 and k > 0:
            q = local / 0.25
            old = self.prev_img.scaled(_fit(self.prev_img)).blurred(2 + 14 * q)
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
        self.captions(frame, j)
        return frame


class NoteHeader:
    """노트 테마 상단: [채널 이름표][회차 꼬리표] / 제목(형광펜이 쓱) / 장면 진행 막대."""

    X = 64

    def __init__(self, spec: dict, th: dict, timings: list):
        self.th = th
        head = spec.get("header") or {}
        brand = spec.get("brand", head.get("brand"))
        tag = head.get("tag") or head.get("label")
        chips = []
        if brand:
            txt = cards.text(brand, 38, th, weight="black", fill=th.get("pill_text", (255, 255, 255)))
            coin = cards.sticker(spec.get("brand_sticker", "coin"), 46, outline=0, shadow=0)
            w = txt.w + 36 + (coin.w - 10 if coin else 0)
            h = 70
            c = cards.Canvas(w, h)
            c.put(cards.rounded(w, h, h / 2, th["pill"]), 0, 0)
            x = 18
            if coin:
                c.put(coin, x - 8, (h - coin.h) / 2)
                x += coin.w - 18
            c.put(txt, x, (h - txt.h) / 2)
            chips.append(c.img())
        if tag:
            txt = cards.text(tag, 34, th, weight="extrabold")
            w, h = txt.w + 30, 70
            c = cards.Canvas(w, h)
            c.put(cards.rounded(w, h, h / 2, th["card"], border=th["text"], bw=3), 0, 0)
            c.put(txt, (w - txt.w) / 2, (h - txt.h) / 2)
            chips.append(c.img())
        self.chips = chips
        self.chip_y = 132
        self.title_raw = head.get("title")
        self.title_size = int(head.get("size", 94))
        self._title = {}
        self.title_y = self.chip_y + (84 if chips else 0)
        first = self.title_img(0.0)
        self.bar_y = self.title_y + (first.h - 12 if first else 0) + 10
        self.progress = head.get("progress", True)
        self.starts = [tm.start for tm in timings]
        self.ends = [tm.end for tm in timings]
        n = len(timings)
        gap = 10
        self.seg_w = (W - self.X * 2 - gap * (n - 1)) / max(1, n)
        self.gap = gap
        self.track = cards.rounded(max(4, int(self.seg_w)), 10, 5, th.get("dim", (182, 172, 154)), alpha=0.45)
        self.full = cards.rounded(max(4, int(self.seg_w)), 10, 5, th["accent"])

    def title_img(self, p: float):
        if not self.title_raw:
            return None
        p = round(cards.clamp01(p), 2)
        if p not in self._title:
            self._title[p] = cards.text(self.title_raw, self.title_size, self.th, weight="black", mark="highlight", marker=p,
                                        max_w=W - self.X * 2 + 30, align="left")
        return self._title[p]

    def bottom(self) -> float:
        return self.bar_y + 10

    def draw(self, frame: np.ndarray, t: float, k: int) -> None:
        x = self.X - 4
        s, a = cards.pop(t + 0.15, 0.3, 0.85)  # 첫 화면(표지)부터 보이게
        for chip in self.chips:
            im = chip.scaled(s) if s != 1.0 else chip
            put_u8(frame, im, x + (chip.w - im.w) / 2, self.chip_y + (chip.h - im.h) / 2, a)
            x += chip.w + 12
        title = self.title_img((t - 0.25) / 0.5)
        if title is not None:
            put_u8(frame, title, self.X - 16, self.title_y)
        if self.progress and len(self.starts) > 1:
            for i in range(len(self.starts)):
                x0 = self.X + i * (self.seg_w + self.gap)
                put_u8(frame, self.track, x0, self.bar_y)
                if i < k:
                    put_u8(frame, self.full, x0, self.bar_y)
                elif i == k:
                    f = cards.clamp01((t - self.starts[i]) / max(0.1, self.ends[i] - self.starts[i]))
                    w = int(self.seg_w * f)
                    if w >= 10:
                        put_u8(frame, cards.Img(self.full.pm[:, :w], self.full.a[:, :w]), x0, self.bar_y)


class NoteLook(Look):
    """노트 테마: 종이 카드(그림자·테이프·기울임), 옆으로 밀려 들어오는 전환, 먹색 라벨 자막."""

    ENTER = 0.34
    EXIT = 0.24

    def __init__(self, spec, th, scenes, caps):
        super().__init__(spec, th, scenes, caps)
        self.header = NoteHeader(spec, th, [tm for _, _, tm in scenes])
        self.card_cy = max(NOTE_CARD_CY, self.header.bottom() + 40 + CARD_MAX[1] / 2) if self.header.title_raw else NOTE_CARD_CY
        self.tilts = [TILTS[i % len(TILTS)] if card.paper else 0.0 for i, (card, _, _) in enumerate(scenes)]
        self._sheet = {}
        self._decor = {}
        self.prev = None
        self.last = None

    def background(self):
        return note_background(self.th)

    def caption_sprite(self, cap):
        cfg = {"size": 1.0, "center_y": H * (1 - CAPTION_BOTTOM), "highlight": "#%02X%02X%02X" % tuple(self.th["highlight"]), "max_width": 960,
               "bg": tuple(self.th["pill"])}
        return graphics.pill_caption_sprite(cap["text"], cap["hl"], cfg)

    def leave(self, t):
        self.prev = self.last

    def sheet(self, k: int, img: cards.Img, angle: float) -> cards.Img:
        """종이 카드면 그림자와 테이프를 붙이고 기울인다. 카드 그림이 그대로면 지난번 결과를 다시 쓴다."""
        cached = self._sheet.get(k)
        ang = round(angle, 1)
        if cached and cached[0] is img and cached[1] == ang:
            return cached[2]
        card = self.scenes[k][0]
        if card.paper:
            if k not in self._decor:
                shadow, pad = cards.soft_shadow(img.w, img.h, r=card.paper_radius, blur=16, alpha=0.24)
                tp = cards.tape(190, 50, angle=(-5, 4, -3)[k % 3], seed=k + 1)
                self._decor[k] = (shadow, pad, tp)
            shadow, pad, tp = self._decor[k]
            c = cards.Canvas(img.w + pad * 2, img.h + pad * 2)
            c.put(shadow, 0, 12)
            c.put(img, pad, pad)
            c.put(tp, pad + img.w / 2 - tp.w / 2, pad - tp.h / 2 + 6)
            out = c.img()
        else:
            out = img
        if abs(ang) >= 0.1:
            out = out.rotated(ang)
        self._sheet[k] = (img, ang, out)
        return out

    def frame(self, j: int) -> np.ndarray:
        t = j / FPS
        self.advance(t)
        k = self.k
        frame = self.bg.copy()
        self.header.draw(frame, t, k)
        card, extras, tm = self.scenes[k]
        local = t - tm.start
        img = card.frame(local)
        fit = _fit(img)
        if k > 0 and self.prev is not None and local < self.EXIT:  # 이전 카드는 왼쪽으로 빠지며 사라짐
            q = cards.clamp01(local / self.EXIT)
            pim, px, py = self.prev
            put_u8(frame, pim, px - (q**1.4) * 900, py + q * 30, (1 - q) ** 1.5)
        angle = self.tilts[k]
        dx = 0.0
        alpha = 1.0
        if k > 0 and local < self.ENTER:  # 새 카드는 오른쪽에서 밀려 들어오며 제자리에 놓인다
            q = cards.ease_out_cubic(local / self.ENTER)
            dx = (1 - q) * 760
            angle += (1 - q) * 7
            alpha = cards.clamp01(local / 0.1)
        sx, sy = card.shake(local)
        sheet = self.sheet(k, img, angle)
        im = sheet.scaled(fit) if abs(fit - 1.0) > 1e-3 else sheet
        x = (W - im.w) / 2 + dx + sx
        y = self.card_cy - im.h / 2 + sy
        put_u8(frame, im, x, y, alpha)
        self.last = (im, x, y)
        box_w, box_h = img.w * fit, img.h * fit
        bx = (W - box_w) / 2 + dx + sx
        by = self.card_cy - box_h / 2 + sy
        target = FrameTarget(frame)
        for extra in extras:
            extra.draw(target, (bx, by, box_w, box_h), local)
        self.captions(frame, j)
        return frame


def make_look(spec: dict, th: dict, scenes: list, caps: list) -> Look:
    return NoteLook(spec, th, scenes, caps) if th.get("kind") == "note" else NeonLook(spec, th, scenes, caps)


def render_video(spec: dict, th: dict, scenes: list, caps: list, duration: float, out: Path, log=print) -> None:
    n = int(round(duration * FPS))
    look = make_look(spec, th, scenes, caps)
    enc = _encoder(out, 18, "fast")
    t0 = time.time()
    try:
        for j in range(n):
            frame = look.frame(j)
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
    th = cards.theme(spec.get("theme"))
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
    if voice_path is None:
        from . import tts

        engine = tts.pick(engine, log=log)  # 실제로 소리 나는 음성으로 정해야 캐시가 섞이지 않는다
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

    video = work / f"video-{stable_hash(PIPELINE, spec, nar.words, nar.duration, [(c['text'], c['hl'], c['show']) for c in caps])}.mp4"
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
        t = tm.start + max(0.4, tm.duration - 0.35)  # 장면이 끝나기 직전 = 애니메이션이 다 끝난 모습
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
