"""모션그래픽 카드: 참고 릴스 스타일(검정 배경 · 형광 연두 강조 · 둥근 카드)을 코드로 그린다.

카드는 미리 그린 조각(글자·도형·아이콘)을 프레임마다 위치·크기·투명도만 바꿔 합성해서 빠르다.
모든 애니메이션 시각은 대본의 단어(at)에 맞춰진다.
"""

from __future__ import annotations

import math
import re
from pathlib import Path

import numpy as np
from PIL import Image, ImageDraw, ImageFilter

from .graphics import color as parse_color, weight_font
from .util import EditorError

SS = 3  # 도형 가장자리를 부드럽게 그리기 위한 확대 배율

THEMES = {
    "neon": {
        "bg_top": (12, 14, 10),
        "bg_bottom": (10, 10, 15),
        "glow": (150, 255, 70),
        "accent": (166, 255, 77),
        "accent_dark": (12, 38, 6),
        "card": (20, 20, 21),
        "card_border": (44, 44, 46),
        "card_border_accent": (104, 160, 46),
        "text": (255, 255, 255),
        "muted": (120, 120, 124),
        "dim": (78, 78, 82),
        "track": (30, 30, 32),
        "red": (255, 82, 82),
        "cream": (243, 240, 234),
        "ink": (34, 34, 34),
        "orange": (217, 119, 87),
    },
}
NAMED = {"red": "red", "lime": "accent", "accent": "accent", "orange": "orange", "gray": "dim", "grey": "dim"}


def theme(name: str = "neon") -> dict:
    return THEMES.get(name, THEMES["neon"])


def col(th: dict, value, default="accent") -> tuple:
    if value is None:
        return th[default]
    if isinstance(value, (list, tuple)):
        return tuple(int(v) for v in value[:3])
    key = NAMED.get(str(value).lower())
    if key:
        return th[key]
    return parse_color(str(value))


# ---------- 이미지 조각 ----------
class Img:
    """알파를 곱해 둔 BGR float32 + 알파. 크기·흐림 변형은 캐시한다."""

    __slots__ = ("pm", "a", "_cache")

    def __init__(self, pm: np.ndarray, a: np.ndarray):
        self.pm, self.a = pm, a
        self._cache = {}

    @classmethod
    def from_pil(cls, im: Image.Image) -> "Img":
        arr = np.asarray(im.convert("RGBA"), np.float32)
        a = arr[..., 3:4] / 255.0
        return cls(np.ascontiguousarray(arr[..., 2::-1] * a), np.ascontiguousarray(a))

    @classmethod
    def empty(cls, w: int, h: int) -> "Img":
        return cls(np.zeros((h, w, 3), np.float32), np.zeros((h, w, 1), np.float32))

    @property
    def w(self) -> int:
        return self.a.shape[1]

    @property
    def h(self) -> int:
        return self.a.shape[0]

    def scaled(self, s: float) -> "Img":
        import cv2

        s = round(float(s), 3)
        if abs(s - 1.0) < 1e-3:
            return self
        key = ("s", s)
        if key not in self._cache:
            size = (max(1, int(round(self.w * s))), max(1, int(round(self.h * s))))
            interp = cv2.INTER_AREA if s < 1 else cv2.INTER_LINEAR
            pm = cv2.resize(self.pm, size, interpolation=interp)
            a = cv2.resize(self.a, size, interpolation=interp)
            self._cache[key] = Img(pm, a.reshape(a.shape[0], a.shape[1], 1))
            if len(self._cache) > 64:
                self._cache.pop(next(iter(self._cache)))
        return self._cache[key]

    def blurred(self, r: float) -> "Img":
        import cv2

        r = int(round(r))
        if r <= 0:
            return self
        key = ("b", r)
        if key not in self._cache:
            pad = r * 3
            pm = cv2.copyMakeBorder(self.pm, pad, pad, pad, pad, cv2.BORDER_CONSTANT, value=0)
            a = cv2.copyMakeBorder(self.a, pad, pad, pad, pad, cv2.BORDER_CONSTANT, value=0)
            pm = cv2.GaussianBlur(pm, (0, 0), r)
            a = cv2.GaussianBlur(a, (0, 0), r)
            self._cache[key] = Img(pm, a.reshape(a.shape[0], a.shape[1], 1))
        return self._cache[key]

    def rotated(self, deg: float) -> "Img":
        import cv2

        deg = round(float(deg), 1)
        key = ("r", deg)
        if key not in self._cache:
            h, w = self.h, self.w
            m = cv2.getRotationMatrix2D((w / 2, h / 2), deg, 1.0)
            cos, sin = abs(m[0, 0]), abs(m[0, 1])
            nw, nh = int(h * sin + w * cos) + 2, int(h * cos + w * sin) + 2
            m[0, 2] += nw / 2 - w / 2
            m[1, 2] += nh / 2 - h / 2
            pm = cv2.warpAffine(self.pm, m, (nw, nh), flags=cv2.INTER_LINEAR)
            a = cv2.warpAffine(self.a, m, (nw, nh), flags=cv2.INTER_LINEAR)
            self._cache[key] = Img(pm, a.reshape(nh, nw, 1))
        return self._cache[key]


def over(dst_pm: np.ndarray, dst_a, img: Img, x: float, y: float, alpha: float = 1.0) -> None:
    """img를 (x, y) 왼쪽 위 기준으로 dst에 겹친다. dst_a가 None이면 불투명 배경(프레임)."""
    if alpha <= 0.003:
        return
    x0, y0 = int(round(x)), int(round(y))
    H_, W_ = dst_pm.shape[:2]
    xa, ya = max(0, x0), max(0, y0)
    xb, yb = min(W_, x0 + img.w), min(H_, y0 + img.h)
    if xb <= xa or yb <= ya:
        return
    sp = img.pm[ya - y0 : yb - y0, xa - x0 : xb - x0]
    sa = img.a[ya - y0 : yb - y0, xa - x0 : xb - x0]
    if alpha < 0.999:
        sp = sp * alpha
        sa = sa * alpha
    region = dst_pm[ya:yb, xa:xb]
    region *= 1.0 - sa
    region += sp
    if dst_a is not None:
        ra = dst_a[ya:yb, xa:xb]
        ra *= 1.0 - sa
        ra += sa


class Canvas:
    def __init__(self, w: int, h: int):
        self.w, self.h = w, h
        self.pm = np.zeros((h, w, 3), np.float32)
        self.a = np.zeros((h, w, 1), np.float32)

    def put(self, img: Img, x: float, y: float, alpha: float = 1.0, scale: float = 1.0, anchor: str = "tl") -> None:
        if scale != 1.0:
            img2 = img.scaled(scale)
            if anchor == "tl":  # 왼쪽 위 기준이어도 가운데를 축으로 키운다
                x += (img.w - img2.w) / 2
                y += (img.h - img2.h) / 2
            img = img2
        if anchor == "c":
            x -= img.w / 2
            y -= img.h / 2
        over(self.pm, self.a, img, x, y, alpha)

    def img(self) -> Img:
        return Img(self.pm, self.a)


# ---------- 기본 도형·글자 ----------
def _big(w: int, h: int) -> Image.Image:
    return Image.new("RGBA", (max(1, w * SS), max(1, h * SS)), (0, 0, 0, 0))


def _small(im: Image.Image, w: int, h: int) -> Img:
    return Img.from_pil(im.resize((max(1, w), max(1, h)), Image.LANCZOS))


def rounded(w: int, h: int, r: float, fill, border=None, bw: float = 0, alpha: float = 1.0) -> Img:
    im = _big(w, h)
    d = ImageDraw.Draw(im)
    fill4 = tuple(fill) + (int(255 * alpha),)
    box = [0, 0, w * SS - 1, h * SS - 1]
    if border is not None and bw > 0:
        d.rounded_rectangle(box, radius=r * SS, fill=tuple(border) + (255,))
        inset = int(bw * SS)
        d.rounded_rectangle([inset, inset, w * SS - 1 - inset, h * SS - 1 - inset], radius=max(0, (r - bw) * SS), fill=fill4)
    else:
        d.rounded_rectangle(box, radius=r * SS, fill=fill4)
    return _small(im, w, h)


def disc(d_: int, fill, ring=None, rw: float = 0) -> Img:
    im = _big(d_, d_)
    d = ImageDraw.Draw(im)
    box = [0, 0, d_ * SS - 1, d_ * SS - 1]
    if ring is not None and rw > 0:
        if fill is not None:
            d.ellipse(box, fill=tuple(fill) + (255,))
        d.ellipse(box, outline=tuple(ring) + (255,), width=int(rw * SS))
    else:
        d.ellipse(box, fill=tuple(fill) + (255,))
    return _small(im, d_, d_)


_MARK = re.compile(r"\[\[(.+?)\]\]")


def split_marks(line: str) -> list:
    """'방법도 [[진짜 간단]]' → [('방법도 ', False), ('진짜 간단', True)]."""
    out, pos = [], 0
    for m in _MARK.finditer(line):
        if m.start() > pos:
            out.append((line[pos : m.start()], False))
        out.append((m.group(1), True))
        pos = m.end()
    if pos < len(line):
        out.append((line[pos:], False))
    return [r for r in out if r[0]]


def plain(text: str) -> str:
    return _MARK.sub(lambda m: m.group(1), text)


def text(txt: str, size: int, th: dict, weight: str = "extrabold", fill=None, accent=None, stroke: int = 0, stroke_fill=None,
         glow=None, glow_r: int = 0, glow_a: float = 0.45, shadow: float = 0.0, max_w: int | None = None, line_gap: float = 0.08,
         align: str = "center") -> Img:
    """여러 줄 글자. [[ ]] 부분은 accent 색. 테두리를 먼저 다 그리고 글자를 위에 그려 겹침을 막는다."""
    fill = tuple(fill or th["text"])
    accent = tuple(accent or th["accent"])
    stroke_fill = tuple(stroke_fill or (0, 0, 0))
    lines = [split_marks(ln) for ln in str(txt).split("\n")]
    fnt = weight_font(size, weight)
    widths = [sum(fnt.getlength(t) for t, _ in ln) for ln in lines]
    if max_w and max(widths or [0]) > max_w:
        size = max(int(size * 0.5), int(size * max_w / max(widths)))
        fnt = weight_font(size, weight)
        widths = [sum(fnt.getlength(t) for t, _ in ln) for ln in lines]
        stroke = int(round(stroke * size / max(1, size)))
    ascent, descent = fnt.getmetrics()
    line_h = ascent + descent
    gap = int(line_h * line_gap)
    pad = stroke + glow_r * 2 + int(shadow * 10) + 6
    w = int(max(widths or [1])) + pad * 2
    h = line_h * len(lines) + gap * (len(lines) - 1) + pad * 2
    stroke_layer = Image.new("RGBA", (w, h), (0, 0, 0, 0))
    fill_layer = Image.new("RGBA", (w, h), (0, 0, 0, 0))
    ds, df = ImageDraw.Draw(stroke_layer), ImageDraw.Draw(fill_layer)
    for n, ln in enumerate(lines):
        if align == "left":
            x = pad
        else:
            x = pad + (w - pad * 2 - widths[n]) / 2
        y = pad + n * (line_h + gap)
        for t, hl in ln:
            if stroke:
                ds.text((x, y), t, font=fnt, fill=stroke_fill + (255,), stroke_width=stroke, stroke_fill=stroke_fill + (255,))
            df.text((x, y), t, font=fnt, fill=(accent if hl else fill) + (255,))
            x += fnt.getlength(t)
    base = Image.new("RGBA", (w, h), (0, 0, 0, 0))
    shape = Image.alpha_composite(stroke_layer, fill_layer) if stroke else fill_layer
    if glow is not None and glow_r > 0:
        a = shape.getchannel("A").filter(ImageFilter.GaussianBlur(glow_r))
        layer = Image.new("RGBA", (w, h), tuple(glow) + (0,))
        layer.putalpha(a.point(lambda v: int(v * glow_a)))
        base = Image.alpha_composite(base, layer)
    if shadow > 0:
        a = shape.getchannel("A").filter(ImageFilter.GaussianBlur(max(2, int(size * 0.05))))
        layer = Image.new("RGBA", (w, h), (0, 0, 0, 0))
        layer.putalpha(a.point(lambda v: int(v * shadow)))
        layer = layer.transform((w, h), Image.AFFINE, (1, 0, 0, 0, 1, -max(2, int(size * 0.04))))
        base = Image.alpha_composite(base, layer)
    if stroke:
        base = Image.alpha_composite(base, stroke_layer)
    base = Image.alpha_composite(base, fill_layer)
    return Img.from_pil(base)


def hero(txt: str, size: int, th: dict, max_w: int = 960) -> Img:
    """형광 연두 큰 글씨 (짙은 초록 테두리 + 번짐)."""
    return text(txt, size, th, weight="black", fill=th["accent"], accent=th["accent"], stroke=max(4, size // 13),
                stroke_fill=th["accent_dark"], glow=th["glow"], glow_r=max(6, size // 9), glow_a=0.42, max_w=max_w)


def label(txt: str, size: int, th: dict, max_w: int | None = None, weight: str = "extrabold", fill=None) -> Img:
    return text(txt, size, th, weight=weight, fill=fill, max_w=max_w)


# ---------- 아이콘 (그림 글자 대신 직접 그린 단순한 아이콘) ----------
ICON_COLORS = {
    "spark": "orange", "zoom": (110, 196, 255), "sound": (210, 214, 224), "caption": (240, 240, 255), "bolt": (255, 150, 40),
    "cut": (240, 240, 240), "clock": (240, 240, 240), "calendar": (90, 140, 255), "mic": (230, 230, 230), "pin": (255, 70, 90),
    "money": (255, 205, 60), "chart": "accent", "home": (240, 240, 240), "heart": (255, 70, 100), "send": "accent",
    "mail": (255, 110, 140), "star": (255, 210, 60), "fire": (255, 120, 40), "warning": (255, 200, 40), "up": "accent",
    "down": "accent", "x": (255, 82, 82), "play": (240, 240, 240), "gift": (255, 82, 82), "phone": (230, 230, 230),
    "user": (230, 230, 230), "check": "accent", "bank": (240, 240, 240), "doc": (230, 230, 230),
}
ICONS = sorted(ICON_COLORS)


def icon(name: str, size: int, th: dict, color=None) -> Img | None:
    if not name or name not in ICON_COLORS:
        return None
    c = col(th, color if color is not None else ICON_COLORS[name])
    S = size * SS
    im = _big(size, size)
    d = ImageDraw.Draw(im)
    rgba = tuple(c) + (255,)
    dark = (12, 12, 12, 255)

    def P(x, y):
        return (x * S, y * S)

    def line(pts, w, fill=rgba):
        d.line([P(*p) for p in pts], fill=fill, width=max(1, int(w * S)), joint="curve")
        for p in (pts[0], pts[-1]):
            r = w * S / 2
            d.ellipse([P(*p)[0] - r, P(*p)[1] - r, P(*p)[0] + r, P(*p)[1] + r], fill=fill)

    def oval(x0, y0, x1, y1, fill=None, outline=None, w=0.0):
        d.ellipse([P(x0, y0), P(x1, y1)], fill=fill, outline=outline, width=int(w * S))

    if name == "check":
        oval(0, 0, 1, 1, fill=rgba)
        line([(0.27, 0.52), (0.43, 0.68), (0.74, 0.35)], 0.11, dark)
    elif name == "spark":
        for k in range(8):
            ang = math.pi / 8 + k * math.pi / 4
            line([(0.5 + 0.08 * math.cos(ang), 0.5 + 0.08 * math.sin(ang)), (0.5 + 0.4 * math.cos(ang), 0.5 + 0.4 * math.sin(ang))], 0.12)
    elif name == "zoom":
        oval(0.06, 0.06, 0.68, 0.68, fill=tuple(c) + (70,), outline=rgba, w=0.1)
        line([(0.62, 0.62), (0.9, 0.9)], 0.15, (150, 90, 200, 255))
    elif name == "sound":
        d.polygon([P(0.08, 0.38), P(0.3, 0.38), P(0.52, 0.16), P(0.52, 0.84), P(0.3, 0.62), P(0.08, 0.62)], fill=rgba)
        for r in (0.18, 0.32):
            d.arc([P(0.5 - r, 0.5 - r), P(0.5 + r + 0.12, 0.5 + r)], start=-50, end=50, fill=(80, 150, 255, 255), width=int(0.08 * S))
    elif name == "caption":
        oval(0.04, 0.12, 0.96, 0.76, fill=rgba)
        d.polygon([P(0.26, 0.66), P(0.16, 0.92), P(0.46, 0.72)], fill=rgba)
    elif name == "bolt":
        d.polygon([P(0.6, 0.02), P(0.16, 0.56), P(0.46, 0.56), P(0.38, 0.98), P(0.84, 0.4), P(0.54, 0.4)], fill=rgba)
    elif name == "cut":
        oval(0.06, 0.58, 0.42, 0.94, outline=rgba, w=0.09)
        oval(0.58, 0.58, 0.94, 0.94, outline=rgba, w=0.09)
        line([(0.34, 0.62), (0.8, 0.06)], 0.09)
        line([(0.66, 0.62), (0.2, 0.06)], 0.09)
    elif name == "clock":
        oval(0.06, 0.1, 0.94, 0.98, outline=rgba, w=0.1)
        line([(0.5, 0.54), (0.5, 0.3)], 0.09)
        line([(0.5, 0.54), (0.68, 0.64)], 0.09)
        line([(0.42, 0.02), (0.58, 0.02)], 0.09)
    elif name == "calendar":
        d.rounded_rectangle([P(0.06, 0.12), P(0.94, 0.94)], radius=0.12 * S, fill=(232, 236, 246, 255))
        d.rounded_rectangle([P(0.06, 0.12), P(0.94, 0.38)], radius=0.12 * S, fill=rgba)
        d.rectangle([P(0.06, 0.28), P(0.94, 0.38)], fill=rgba)
        for gx in (0.26, 0.5, 0.74):
            for gy in (0.55, 0.77):
                oval(gx - 0.06, gy - 0.06, gx + 0.06, gy + 0.06, fill=tuple(c) + (255,))
    elif name == "mic":
        d.rounded_rectangle([P(0.34, 0.04), P(0.66, 0.6)], radius=0.16 * S, fill=rgba)
        d.arc([P(0.2, 0.26), P(0.8, 0.76)], start=0, end=180, fill=rgba, width=int(0.08 * S))
        line([(0.5, 0.76), (0.5, 0.94)], 0.08)
    elif name == "pin":
        line([(0.5, 0.5), (0.16, 0.9)], 0.07, (190, 190, 200, 255))
        d.polygon([P(0.44, 0.08), P(0.92, 0.56), P(0.66, 0.62), P(0.38, 0.34)], fill=rgba)
        oval(0.5, 0.0, 0.86, 0.36, fill=rgba)
    elif name == "money":
        oval(0.04, 0.04, 0.96, 0.96, fill=rgba)
        oval(0.14, 0.14, 0.86, 0.86, outline=(190, 140, 20, 255), w=0.06)
        fnt = weight_font(int(S * 0.5), "black")
        d.text(P(0.5, 0.52), "₩", font=fnt, fill=(150, 100, 10, 255), anchor="mm")
    elif name == "chart":
        for i, hgt in enumerate((0.38, 0.62, 0.88)):
            x0 = 0.08 + i * 0.3
            d.rounded_rectangle([P(x0, 0.96 - hgt), P(x0 + 0.24, 0.96)], radius=0.06 * S, fill=rgba)
    elif name == "home":
        d.polygon([P(0.5, 0.06), P(0.96, 0.48), P(0.04, 0.48)], fill=rgba)
        d.rectangle([P(0.18, 0.46), P(0.82, 0.94)], fill=rgba)
        d.rectangle([P(0.42, 0.62), P(0.58, 0.94)], fill=dark)
    elif name == "heart":
        oval(0.06, 0.12, 0.54, 0.6, fill=rgba)
        oval(0.46, 0.12, 0.94, 0.6, fill=rgba)
        d.polygon([P(0.09, 0.46), P(0.5, 0.92), P(0.91, 0.46), P(0.5, 0.3)], fill=rgba)
    elif name == "send":
        oval(0, 0, 1, 1, fill=rgba)
        d.polygon([P(0.32, 0.26), P(0.78, 0.5), P(0.32, 0.74), P(0.4, 0.5)], fill=dark)
    elif name == "mail":
        d.rounded_rectangle([P(0.06, 0.5), P(0.94, 0.94)], radius=0.1 * S, fill=(240, 240, 245, 255))
        line([(0.5, 0.06), (0.5, 0.58)], 0.12)
        d.polygon([P(0.28, 0.38), P(0.72, 0.38), P(0.5, 0.66)], fill=rgba)
    elif name == "star":
        pts = []
        for k in range(10):
            r = 0.48 if k % 2 == 0 else 0.2
            ang = -math.pi / 2 + k * math.pi / 5
            pts.append(P(0.5 + r * math.cos(ang), 0.52 + r * math.sin(ang)))
        d.polygon(pts, fill=rgba)
    elif name == "fire":
        d.polygon([P(0.5, 0.02), P(0.84, 0.5), P(0.84, 0.72), P(0.5, 0.98), P(0.16, 0.72), P(0.16, 0.5), P(0.34, 0.3), P(0.4, 0.5)], fill=rgba)
        oval(0.32, 0.56, 0.68, 0.94, fill=(255, 210, 60, 255))
    elif name == "warning":
        d.polygon([P(0.5, 0.04), P(0.97, 0.92), P(0.03, 0.92)], fill=rgba)
        line([(0.5, 0.36), (0.5, 0.62)], 0.1, dark)
        oval(0.44, 0.72, 0.56, 0.84, fill=dark)
    elif name in ("up", "down"):
        sign = -1 if name == "up" else 1
        y0, y1 = (0.92, 0.12) if name == "up" else (0.08, 0.88)
        line([(0.5, y0), (0.5, y1)], 0.13)
        line([(0.2, y1 - sign * 0.3), (0.5, y1), (0.8, y1 - sign * 0.3)], 0.13)
    elif name == "x":
        oval(0, 0, 1, 1, fill=rgba)
        line([(0.32, 0.32), (0.68, 0.68)], 0.1, (255, 255, 255, 255))
        line([(0.68, 0.32), (0.32, 0.68)], 0.1, (255, 255, 255, 255))
    elif name == "play":
        oval(0.04, 0.04, 0.96, 0.96, outline=rgba, w=0.08)
        d.polygon([P(0.4, 0.3), P(0.72, 0.5), P(0.4, 0.7)], fill=rgba)
    elif name == "gift":
        d.rectangle([P(0.08, 0.36), P(0.92, 0.56)], fill=rgba)
        d.rectangle([P(0.14, 0.56), P(0.86, 0.96)], fill=rgba)
        d.rectangle([P(0.44, 0.36), P(0.56, 0.96)], fill=(255, 210, 60, 255))
        oval(0.24, 0.12, 0.52, 0.38, outline=(255, 210, 60, 255), w=0.07)
        oval(0.48, 0.12, 0.76, 0.38, outline=(255, 210, 60, 255), w=0.07)
    elif name == "phone":
        d.rounded_rectangle([P(0.24, 0.04), P(0.76, 0.96)], radius=0.12 * S, outline=rgba, width=int(0.08 * S))
        oval(0.44, 0.8, 0.56, 0.9, fill=rgba)
    elif name == "user":
        oval(0.3, 0.04, 0.7, 0.44, fill=rgba)
        d.pieslice([P(0.08, 0.5), P(0.92, 1.3)], start=180, end=360, fill=rgba)
    elif name == "bank":
        d.polygon([P(0.5, 0.04), P(0.96, 0.3), P(0.04, 0.3)], fill=rgba)
        for x in (0.16, 0.38, 0.6, 0.82):
            d.rectangle([P(x - 0.05, 0.36), P(x + 0.05, 0.8)], fill=rgba)
        d.rectangle([P(0.04, 0.84), P(0.96, 0.96)], fill=rgba)
    elif name == "doc":
        d.rounded_rectangle([P(0.16, 0.04), P(0.84, 0.96)], radius=0.08 * S, fill=rgba)
        for y in (0.3, 0.48, 0.66):
            line([(0.3, y), (0.7, y)], 0.06, (90, 90, 100, 255))
    return _small(im, size, size)


# ---------- 움직임 ----------
def clamp01(x: float) -> float:
    return 0.0 if x < 0 else 1.0 if x > 1 else x


def ease_out_cubic(x: float) -> float:
    x = clamp01(x)
    return 1 - (1 - x) ** 3


def ease_out_back(x: float, k: float = 1.70158) -> float:
    x = clamp01(x)
    return 1 + (k + 1) * (x - 1) ** 3 + k * (x - 1) ** 2


def pop(t: float, dur: float = 0.28, start: float = 0.6):
    """등장 애니메이션: (크기, 투명도). t<0이면 아직 안 보임."""
    if t < 0:
        return 0.0, 0.0
    p = clamp01(t / dur)
    return start + (1 - start) * ease_out_back(p), clamp01(t / (dur * 0.5))


# ---------- 카드 ----------
CARD_W = 900


class Card:
    """카드 한 장. timing.at('단어', 기본값) → 장면 안에서의 시각(초)."""

    sfx_events: list

    def __init__(self, spec: dict, th: dict, timing):
        self.spec = spec
        self.th = th
        self.timing = timing
        self.sfx_events = []
        self._last_key = None
        self._last_img = None

    def at(self, phrase, default: float) -> float:
        return self.timing.at(phrase, default)

    def spread(self, n: int, start: float = 0.35, end_frac: float = 0.7) -> list:
        end = max(start + 0.3 * n, self.timing.duration * end_frac)
        return [start + i * (end - start) / max(1, n) for i in range(n)]

    def frame(self, t: float) -> Img:
        key = self.state(t)
        if key is not None and key == self._last_key:
            return self._last_img
        img = self.draw(t)
        self._last_key, self._last_img = key, img
        return img

    def state(self, t: float):  # 같은 상태면 다시 그리지 않는다 (None이면 매번 그림)
        return None

    def draw(self, t: float) -> Img:
        raise NotImplementedError

    def box(self, w: int, h: int, accent: bool = False) -> Img:
        th = self.th
        return rounded(w, h, 36, th["card"], border=th["card_border_accent"] if accent else th["card_border"], bw=3, alpha=0.96)

    def header(self, title: str, icon_name: str | None, size: int = 50) -> Img:
        th = self.th
        txt = text(title, size, th, max_w=CARD_W - 200)
        ic = icon(icon_name, int(size * 1.1), th) if icon_name else None
        w = txt.w + (ic.w + 14 if ic else 0)
        h = max(txt.h, ic.h if ic else 0)
        c = Canvas(w, h)
        x = 0
        if ic:
            c.put(ic, 0, (h - ic.h) / 2)
            x = ic.w + 14
        c.put(txt, x, (h - txt.h) / 2)
        return c.img()


class ChecklistCard(Card):
    ROW = 112

    def __init__(self, spec, th, timing):
        super().__init__(spec, th, timing)
        items = spec.get("items") or []
        if not items:
            raise EditorError("checklist 카드에는 items가 필요합니다.")
        self.items = [i if isinstance(i, dict) else {"text": str(i)} for i in items]
        n = len(self.items)
        self.w = CARD_W
        self.head = self.header(spec.get("title", ""), spec.get("icon", "spark")) if spec.get("title") else None
        top = 150 if self.head else 50
        self.h = top + n * self.ROW + 40
        self.top = top
        self.bg = self.box(self.w, self.h, accent=spec.get("accent_border", True))
        self.divider = rounded(self.w - 112, 4, 2, th["card_border"])
        self.ring = disc(72, None, ring=th["dim"], rw=5)
        self.check = icon("check", 72, th)
        self.icons = [icon(i.get("icon"), 60, th) for i in self.items]
        self.off = [text(i["text"], 62, th, fill=th["dim"], max_w=self.w - 320) for i in self.items]
        self.on = [text(i["text"], 62, th, max_w=self.w - 320) for i in self.items]
        defaults = self.spread(n)
        self.when = [self.at(i.get("at"), d) for i, d in zip(self.items, defaults)]
        self.sfx_events = [("ding", t) for t in self.when]

    def state(self, t):
        return tuple(round(min(max(t - w, -0.01), 0.3), 2) for w in self.when)

    def draw(self, t):
        c = Canvas(self.w, self.h)
        c.put(self.bg, 0, 0)
        if self.head:
            c.put(self.head, 56, 70 - self.head.h / 2)
            c.put(self.divider, 56, 128)
        for i in range(len(self.items)):
            y = self.top + i * self.ROW + self.ROW / 2
            p = t - self.when[i]
            if p < 0:
                c.put(self.ring, 56, y - 36)
                c.put(self.off[i], 160 + (76 if self.icons[i] else 0), y - self.off[i].h / 2)
            else:
                s, a = pop(p, 0.25, 0.5)
                c.put(self.check, 56, y - 36, alpha=a, scale=s)
                fade = clamp01(p / 0.15)
                x = 160 + (76 if self.icons[i] else 0)
                c.put(self.off[i], x, y - self.off[i].h / 2, alpha=1 - fade)
                c.put(self.on[i], x, y - self.on[i].h / 2, alpha=fade)
            if self.icons[i]:
                c.put(self.icons[i], 160, y - 30)
        return c.img()


class BigTextCard(Card):
    def __init__(self, spec, th, timing):
        super().__init__(spec, th, timing)
        lines = spec.get("lines") or [spec.get("text", "")]
        self.parts = []
        for ln in lines:
            if re.fullmatch(r"\s*\[\[.+\]\]\s*", ln):
                self.parts.append(("hero", hero(plain(ln).strip(), int(spec.get("size", 250)), th, max_w=900)))
            else:
                self.parts.append(("plain", text(ln, 84, th, max_w=960)))
        self.underline = spec.get("underline", True) and any(k == "hero" for k, _ in self.parts)
        self.pill = None
        if spec.get("pill"):
            txt = text(spec["pill"], 44, th, max_w=760)
            self.pill = Canvas(txt.w + 56, txt.h + 20)
            self.pill.put(rounded(txt.w + 56, txt.h + 20, (txt.h + 20) / 2, (31, 31, 33), border=(62, 62, 66), bw=2), 0, 0)
            self.pill.put(txt, 28, 10)
            self.pill = self.pill.img()
        gap = 14
        self.w = max([img.w for _, img in self.parts] + [self.pill.w if self.pill else 0, 400])
        y = 0
        self.ys = []
        for kind, img in self.parts:
            self.ys.append(y)
            y += img.h - (20 if kind == "hero" else 6) + gap
        hero_i = next((i for i, (k, _) in enumerate(self.parts) if k == "hero"), None)
        self.hero_i = hero_i
        if self.underline:
            hero_img = self.parts[hero_i][1]
            self.bar_w = int(hero_img.w * 0.8)
            self.bar = rounded(self.bar_w, 14, 7, th["accent"])
            self.bar_y = y + 4
            y += 40
        if self.pill:
            self.pill_y = y + 6
            y += self.pill.h + 12
        self.h = int(y + 10)
        defaults = [0.05 + 0.3 * i for i in range(len(self.parts))]
        line_at = spec.get("at") or []
        if isinstance(line_at, str):
            line_at = [line_at]
        self.when = [self.at(line_at[i] if i < len(line_at) else None, d) for i, d in enumerate(defaults)]
        last = max(self.when) if self.when else 0
        self.bar_t = (self.when[hero_i] if hero_i is not None else last) + 0.18
        self.pill_t = self.at(spec.get("pill_at"), self.bar_t + 0.35)
        rng = np.random.default_rng(len(lines) * 7 + 3)
        self.sparks = [(rng.uniform(0.02, 0.98) * self.w, rng.uniform(0.0, 1.0) * self.h, rng.uniform(0, 1), rng.choice([14, 18, 24])) for _ in range(7)]
        self.star = {s: icon("star", s, th, color=(235, 245, 230)) for s in (14, 18, 24)}
        self.sfx_events = [("pop", w) for w in self.when] + ([("pop", self.pill_t)] if self.pill else [])

    def draw(self, t):
        c = Canvas(self.w, self.h)
        if self.hero_i is not None and t > self.when[self.hero_i]:
            for x, y, ph, s in self.sparks:  # 반짝이
                a = 0.25 + 0.75 * abs(math.sin(2 * math.pi * (t * 0.7 + ph)))
                c.put(self.star[s], x - s / 2, y - s / 2, alpha=a * clamp01((t - self.when[self.hero_i]) / 0.4))
        for (kind, img), y, w in zip(self.parts, self.ys, self.when):
            s, a = pop(t - w, 0.3, 0.55 if kind == "hero" else 0.8)
            c.put(img, (self.w - img.w) / 2, y, alpha=a, scale=s)
        if self.underline:
            p = ease_out_cubic((t - self.bar_t) / 0.35)
            if p > 0:
                bw = max(14, int(self.bar_w * p))
                c.put(rounded(bw, 14, 7, self.th["accent"]), (self.w - bw) / 2, self.bar_y)
        if self.pill:
            s, a = pop(t - self.pill_t, 0.25, 0.7)
            c.put(self.pill, (self.w - self.pill.w) / 2, self.pill_y, alpha=a, scale=s)
        return c.img()


class CompareCard(Card):
    """막대 비교. mode=replace: 같은 자리에서 막대가 바뀜(직접 편집 → 클로드), stack: 줄줄이."""

    BAR_H = 96

    def __init__(self, spec, th, timing):
        super().__init__(spec, th, timing)
        self.bars = spec.get("bars") or []
        if not self.bars:
            raise EditorError("compare 카드에는 bars가 필요합니다.")
        self.mode = spec.get("mode", "replace" if len(self.bars) == 2 else "stack")
        self.w = CARD_W
        self.head = self.header(spec.get("title", ""), spec.get("icon", "clock")) if spec.get("title") else None
        self.track_w = self.w - 112
        rows = 1 if self.mode == "replace" else len(self.bars)
        self.top = 120 if self.head else 40
        self.result = hero(spec["result"], int(spec.get("result_size", 104)), th, max_w=self.w - 80) if spec.get("result") else None
        self.h = self.top + rows * (self.BAR_H + 28) + (self.result.h + 10 if self.result else 0) + 30
        self.track = rounded(self.track_w, self.BAR_H, self.BAR_H / 2, th["track"])
        self.labels = []
        for b in self.bars:
            c = col(th, b.get("color", "accent"))
            dark_text = 0.299 * c[0] + 0.587 * c[1] + 0.114 * c[2] > 150
            self.labels.append(text(b.get("label", ""), 44, th, fill=(20, 20, 20) if dark_text else (255, 255, 255)))
        defaults = [0.25 + 0.9 * i for i in range(len(self.bars))]
        self.when = [self.at(b.get("at"), d) for b, d in zip(self.bars, defaults)]
        self.result_t = self.at(spec.get("result_at"), self.when[-1] + 0.45)
        self.sfx_events = [("pop", w) for w in self.when] + ([("ding", self.result_t)] if self.result else [])

    def _bar(self, i: int, frac: float) -> Img:
        b = self.bars[i]
        full = max(self.labels[i].w + 72, int(self.track_w * float(b.get("value", 1.0))))  # 글자가 들어갈 만큼은 늘 확보
        width = max(self.BAR_H, int(full * frac))
        return rounded(width, self.BAR_H, self.BAR_H / 2, col(self.th, b.get("color", "accent")))

    def _full(self, i: int) -> float:
        return max(self.labels[i].w + 72, self.track_w * float(self.bars[i].get("value", 1.0)))

    def state(self, t):
        return tuple(round(clamp01((t - w) / 0.5), 2) for w in self.when) + (round(clamp01((t - self.result_t) / 0.3), 2),)

    def draw(self, t):
        c = Canvas(self.w, self.h)
        if self.head:
            c.put(self.head, 40, 50 - self.head.h / 2)
        for row in range(1 if self.mode == "replace" else len(self.bars)):
            y = self.top + row * (self.BAR_H + 28)
            c.put(self.track, 56, y)
            if self.mode == "replace":
                active = max([i for i, w in enumerate(self.when) if t >= w] or [0])
                p = ease_out_cubic((t - self.when[active]) / 0.5)
                if active > 0:  # 이전 막대 길이에서 새 막대 길이로 줄어들며 색이 바뀐다
                    prev = self._full(active - 1)
                    cur = self._full(active)
                    frac = (prev + (cur - prev) * p) / max(cur, 1e-3)
                    bar = self._bar(active, frac)
                else:
                    bar = self._bar(0, p)
                if t >= self.when[0]:
                    c.put(bar, 56, y)
                    lab = self.labels[active]
                    if bar.w > lab.w + 40:
                        c.put(lab, 56 + 32, y + (self.BAR_H - lab.h) / 2, alpha=clamp01((t - self.when[active]) / 0.2))
            else:
                p = ease_out_cubic((t - self.when[row]) / 0.5)
                if p > 0:
                    bar = self._bar(row, p)
                    c.put(bar, 56, y)
                    lab = self.labels[row]
                    if bar.w > lab.w + 40:
                        c.put(lab, 56 + 32, y + (self.BAR_H - lab.h) / 2, alpha=clamp01((t - self.when[row]) / 0.25))
        if self.result:
            s, a = pop(t - self.result_t, 0.3, 0.6)
            rows = 1 if self.mode == "replace" else len(self.bars)
            c.put(self.result, (self.w - self.result.w) / 2, self.top + rows * (self.BAR_H + 28) - 6, alpha=a, scale=s)
        return c.img()


class CalendarCard(Card):
    def __init__(self, spec, th, timing):
        super().__init__(spec, th, timing)
        self.days = spec.get("days") or ["월", "화", "수", "목", "금", "토", "일"]
        n = len(self.days)
        self.count = int(spec.get("check", n))
        self.w = CARD_W
        self.head = self.header(spec.get("title", "매일 꾸준히"), spec.get("icon", "calendar"))
        gap = 14
        self.bw = int((self.w - 112 - gap * (n - 1)) / n)
        self.bh = 150
        self.gap = gap
        self.h = 130 + self.bh + 56
        self.bg = self.box(self.w, self.h)
        self.cell_off = rounded(self.bw, self.bh, 20, (26, 26, 28), border=(46, 46, 50), bw=2)
        self.cell_on = rounded(self.bw, self.bh, 20, (26, 30, 22), border=th["accent"], bw=4)
        self.lab_off = [text(d, 38, th, fill=th["muted"]) for d in self.days]
        self.lab_on = [text(d, 38, th) for d in self.days]
        self.check = icon("check", min(64, self.bw - 24), th)
        start = self.at(spec.get("at"), 0.4)
        step = float(spec.get("step", 0.22))
        self.when = [start + i * step if i < self.count else 1e9 for i in range(n)]
        self.sfx_events = [("pop", w) for w in self.when if w < 1e8][:3]

    def state(self, t):
        return tuple(round(min(max(t - w, -0.01), 0.3), 2) for w in self.when)

    def draw(self, t):
        c = Canvas(self.w, self.h)
        c.put(self.bg, 0, 0)
        c.put(self.head, 48, 64 - self.head.h / 2)
        for i in range(len(self.days)):
            x = 56 + i * (self.bw + self.gap)
            y = 120
            p = t - self.when[i]
            on = p >= 0
            c.put(self.cell_on if on else self.cell_off, x, y)
            lab = self.lab_on[i] if on else self.lab_off[i]
            c.put(lab, x + (self.bw - lab.w) / 2, y + 14)
            if on:
                s, a = pop(p, 0.25, 0.4)
                c.put(self.check, x + (self.bw - self.check.w) / 2, y + self.bh - self.check.h - 18, alpha=a, scale=s)
        return c.img()


class CommentCard(Card):
    """CTA: '댓글에 ○○' + 댓글 입력 + 자료 DM 카드."""

    def __init__(self, spec, th, timing):
        super().__init__(spec, th, timing)
        self.keyword = spec.get("keyword", "키워드")
        self.w = CARD_W
        lead = spec.get("lead", "댓글에")
        lead_txt = text(lead, 66, th)
        pin = icon(spec.get("icon", "pin"), 70, th)
        lc = Canvas(lead_txt.w + (pin.w + 12 if pin else 0), max(lead_txt.h, pin.h if pin else 0))
        if pin:
            lc.put(pin, 0, (lc.h - pin.h) / 2)
        lc.put(lead_txt, (pin.w + 12) if pin else 0, (lc.h - lead_txt.h) / 2)
        self.lead = lc.img()
        self.kw = hero(f"'{self.keyword}'", int(spec.get("size", 160)), th, max_w=self.w - 40)
        self.box_h = 200
        self.cbox = rounded(self.w - 40, self.box_h, 28, (22, 22, 24), border=(48, 48, 52), bw=2)
        ava = Image.new("RGBA", (60 * SS, 60 * SS))
        grad = Image.linear_gradient("L").resize((60 * SS, 60 * SS)).rotate(45)
        ava_rgb = Image.merge("RGB", [Image.new("L", (60 * SS, 60 * SS), 255), grad, Image.new("L", (60 * SS, 60 * SS), 140)])
        mask = Image.new("L", (60 * SS, 60 * SS), 0)
        ImageDraw.Draw(mask).ellipse([0, 0, 60 * SS - 1, 60 * SS - 1], fill=255)
        ava.paste(ava_rgb, (0, 0), mask)
        self.avatar = _small(ava, 60, 60)
        self.meta = text(spec.get("me", "나 · 방금"), 28, th, fill=th["muted"], weight="bold")
        self.avatar_gray = disc(48, (70, 70, 74))
        self.input = rounded(self.w - 220, 66, 33, (28, 28, 30), border=(56, 56, 60), bw=2)
        self.hint = text("댓글 달기...", 32, th, fill=th["muted"], weight="bold")
        self.send = icon("send", 70, th)
        self.heart = icon("heart", 40, th)
        self.chars = [text(self.keyword[:k], 44, th) if k else None for k in range(len(self.keyword) + 1)]
        self.items = spec.get("items") or []
        self.dm_from = spec.get("dm")
        self.dm = None
        if self.dm_from:
            self.dm_title = text(self.dm_from, 34, th, fill=(70, 70, 70), weight="bold", max_w=self.w - 180)
            self.mail = icon("mail", 40, th)
            self.dm_h = 90 + len(self.items) * 64 + 20
            self.dm = rounded(self.w - 40, self.dm_h, 26, th["cream"])
            self.item_txt = [text(i if isinstance(i, str) else i.get("text", ""), 38, th, fill=th["ink"], max_w=self.w - 200) for i in self.items]
            self.tick = rounded(40, 40, 8, (90, 200, 70))
            tick_mark = icon("check", 40, th, color=(90, 200, 70))
            self.tick = tick_mark
        self.lead_t = 0.05
        self.kw_t = self.at(spec.get("keyword_at"), 0.2)
        self.box_t = self.kw_t + 0.3
        self.type_t = self.at(spec.get("type_at"), self.box_t + 0.25)
        self.type_dur = 0.09 * len(self.keyword)
        self.dm_t = self.at(spec.get("dm_at"), self.type_t + self.type_dur + 0.6)
        defaults = [self.dm_t + 0.35 + 0.35 * i for i in range(len(self.items))]
        self.item_t = [self.at(i.get("at") if isinstance(i, dict) else None, d) for i, d in zip(self.items, defaults)]
        y = 0
        self.y_lead = y
        y += self.lead.h - 6
        self.y_kw = y
        y += self.kw.h - 10
        self.y_box = y
        y += self.box_h + 24
        self.y_dm = y
        if self.dm:
            y += self.dm_h
        self.h = int(y + 10)
        self.sfx_events = [("pop", self.kw_t), ("pop", self.type_t + self.type_dur)] + ([("ding", self.dm_t)] if self.dm else [])

    def state(self, t):
        q = lambda x, d=0.3: round(min(max(x, -0.01), d), 2)  # noqa: E731
        k = int(clamp01((t - self.type_t) / max(0.01, self.type_dur)) * len(self.keyword)) if t >= self.type_t else 0
        return (q(t - self.lead_t), q(t - self.kw_t), q(t - self.box_t), k, q(t - self.type_t - self.type_dur - 0.15), q(t - self.dm_t)) + tuple(q(t - it) for it in self.item_t)

    def draw(self, t):
        th = self.th
        c = Canvas(self.w, self.h)
        s, a = pop(t - self.lead_t, 0.25, 0.8)
        c.put(self.lead, (self.w - self.lead.w) / 2, self.y_lead, alpha=a, scale=s)
        s, a = pop(t - self.kw_t, 0.3, 0.55)
        c.put(self.kw, (self.w - self.kw.w) / 2, self.y_kw, alpha=a, scale=s)
        p = ease_out_cubic((t - self.box_t) / 0.3)
        if p > 0:
            y = self.y_box + (1 - p) * 40
            box = Canvas(self.w - 40, self.box_h)
            box.put(self.cbox, 0, 0)
            box.put(self.avatar, 32, 26)
            box.put(self.meta, 108, 22)
            k = int(clamp01((t - self.type_t) / max(0.01, self.type_dur)) * len(self.keyword)) if t >= self.type_t else 0
            if k and self.chars[k] is not None:
                box.put(self.chars[k], 104, 52)
            if t >= self.type_t + self.type_dur + 0.15:
                hs, ha = pop(t - self.type_t - self.type_dur - 0.15, 0.25, 0.5)
                box.put(self.heart, self.w - 40 - 70, 36, alpha=ha, scale=hs)
            box.put(self.avatar_gray, 36, 118)
            box.put(self.input, 100, 112)
            box.put(self.hint, 130, 112 + (66 - self.hint.h) / 2)
            box.put(self.send, self.w - 40 - 100, 110)
            c.put(box.img(), 20, y, alpha=p)
        if self.dm:
            s, a = pop(t - self.dm_t, 0.3, 0.8)
            if a > 0:
                dm = Canvas(self.w - 40, self.dm_h)
                dm.put(self.dm, 0, 0)
                dm.put(self.mail, 36, 30)
                dm.put(self.dm_title, 90, 30 + (40 - self.dm_title.h) / 2)
                for i, (txt, it) in enumerate(zip(self.item_txt, self.item_t)):
                    yy = 96 + i * 64
                    if t >= it:
                        ts, ta = pop(t - it, 0.25, 0.5)
                        dm.put(self.tick, 40, yy, alpha=ta, scale=ts)
                        dm.put(txt, 96, yy + (40 - txt.h) / 2, alpha=clamp01((t - it) / 0.15))
                c.put(dm.img(), 20, self.y_dm, alpha=a, scale=s)
        return c.img()


class WaveformCard(Card):
    """목소리 파형 위에 틀린 부분·공백·'음'을 빨갛게 표시하고, cut_at에 잘라 낸다."""

    def __init__(self, spec, th, timing):
        super().__init__(spec, th, timing)
        self.w = CARD_W
        self.head = self.header(spec.get("title", "내 목소리 원본"), spec.get("icon", "mic"), size=44)
        self.marks = spec.get("marks") or []
        self.n = int(spec.get("bars", 40))
        self._bars = {}
        rng = np.random.default_rng(int(spec.get("seed", 5)))
        env = 0.35 + 0.65 * np.abs(np.sin(np.linspace(0, 3.4 * np.pi, self.n)))
        self.heights = np.clip(env * rng.uniform(0.45, 1.0, self.n), 0.12, 1.0)
        self.h = 640
        self.bg = self.box(self.w, self.h)
        self.area = (60, 210, self.w - 60, 520)
        spans = []
        k = len(self.marks)
        for i, m in enumerate(self.marks):
            if "span" in m:
                spans.append(tuple(m["span"]))
            else:
                center = (i + 1) / (k + 1)
                spans.append((center - 0.075, center + 0.075))
        self.spans = spans
        self.tags = [text(m.get("label", ""), 34, th, weight="extrabold") for m in self.marks]
        defaults = self.spread(k, 0.45, 0.75)
        self.when = [self.at(m.get("at"), d) for m, d in zip(self.marks, defaults)]
        self.cut_t = self.at(spec.get("cut_at"), 1e9) if spec.get("cut_at") else 1e9
        self.sfx_events = [("pop", w) for w in self.when] + ([("whoosh", self.cut_t)] if self.cut_t < 1e8 else [])

    def state(self, t):
        return (round(clamp01(t / 0.5), 2),) + tuple(round(min(max(t - w, -0.01), 0.3), 2) for w in self.when) + (round(clamp01((t - self.cut_t) / 0.35), 2),)

    def draw(self, t):
        th = self.th
        c = Canvas(self.w, self.h)
        c.put(self.bg, 0, 0)
        c.put(self.head, 48, 70 - self.head.h / 2)
        x0, y0, x1, y1 = self.area
        mid = (y0 + y1) / 2
        span_w = x1 - x0
        step = span_w / self.n
        bw = max(8, int(step * 0.6))
        grow = ease_out_cubic(t / 0.5)
        cut = ease_out_cubic((t - self.cut_t) / 0.35)
        for i, hgt in enumerate(self.heights):
            frac = (i + 0.5) / self.n
            marked = [m for m, (a, b) in enumerate(self.spans) if a <= frac <= b and t >= self.when[m]]
            bh = max(8, hgt * (y1 - y0) * grow)
            if marked and cut > 0:
                bh = max(0, bh * (1 - cut))
                if bh < 2:
                    continue
            key = (int(bh) // 2 * 2, bool(marked), bw)
            if key not in self._bars:
                self._bars[key] = rounded(bw, max(2, key[0]), bw / 2, th["red"] if marked else (235, 235, 235))
            x = x0 + i * step + (step - bw) / 2
            c.put(self._bars[key], x, mid - key[0] / 2)
        for m, (a, b) in enumerate(self.spans):
            p = t - self.when[m]
            if p < 0:
                continue
            s, al = pop(p, 0.25, 0.85)
            al *= 1 - cut
            bx0, bx1 = x0 + a * span_w - 6, x0 + b * span_w + 6
            box = rounded(int(bx1 - bx0), int(y1 - y0 + 30), 16, (120, 20, 20), border=th["red"], bw=4, alpha=0.35)
            c.put(box, bx0, y0 - 15, alpha=al)
            tag = self.tags[m]
            pill = rounded(tag.w + 28, tag.h + 6, 12, (240, 70, 70))
            cx = (bx0 + bx1) / 2
            c.put(pill, cx - pill.w / 2, y0 - 30 - pill.h, alpha=al, scale=s)
            c.put(tag, cx - tag.w / 2, y0 - 27 - pill.h, alpha=al, scale=s)
        return c.img()


class StepsCard(Card):
    ROW = 128

    def __init__(self, spec, th, timing):
        super().__init__(spec, th, timing)
        steps = spec.get("steps") or spec.get("items") or []
        if not steps:
            raise EditorError("steps 카드에는 steps가 필요합니다.")
        self.steps = [s if isinstance(s, dict) else {"text": str(s)} for s in steps]
        self.w = CARD_W
        self.head = self.header(spec["title"], spec.get("icon")) if spec.get("title") else None
        self.top = 130 if self.head else 50
        self.h = self.top + len(self.steps) * self.ROW + 30
        self.bg = self.box(self.w, self.h)
        self.nums = []
        for i in range(len(self.steps)):
            d = Canvas(76, 76)
            d.put(disc(76, th["accent"]), 0, 0)
            num = text(str(i + 1), 42, th, weight="black", fill=(10, 10, 10))
            d.put(num, (76 - num.w) / 2, (76 - num.h) / 2)
            self.nums.append(d.img())
        self.txt = [text(s["text"], 54, th, max_w=self.w - 220, align="left") for s in self.steps]
        defaults = self.spread(len(self.steps), 0.25, 0.6)
        self.when = [self.at(s.get("at"), d) for s, d in zip(self.steps, defaults)]
        self.sfx_events = [("pop", w) for w in self.when]

    def state(self, t):
        return tuple(round(min(max(t - w, -0.01), 0.35), 2) for w in self.when)

    def draw(self, t):
        c = Canvas(self.w, self.h)
        c.put(self.bg, 0, 0)
        if self.head:
            c.put(self.head, 48, 66 - self.head.h / 2)
        for i in range(len(self.steps)):
            p = t - self.when[i]
            if p < 0:
                continue
            q = ease_out_cubic(p / 0.3)
            y = self.top + i * self.ROW + (self.ROW - 76) / 2
            dx = (1 - q) * 60
            c.put(self.nums[i], 56 + dx, y, alpha=q)
            c.put(self.txt[i], 160 + dx, y + 38 - self.txt[i].h / 2, alpha=q)
        return c.img()


class StatCard(Card):
    """큰 숫자가 0부터 올라간다."""

    def __init__(self, spec, th, timing):
        super().__init__(spec, th, timing)
        self.w = CARD_W
        self.value = float(spec.get("value", 0))
        self.decimals = int(spec.get("decimals", 0))
        self.unit = spec.get("unit", "")
        self.prefix = spec.get("prefix", "")
        self.size = int(spec.get("size", 180))
        self.label = text(spec["label"], 58, th, max_w=self.w) if spec.get("label") else None
        self.sub = text(spec["sub"], 36, th, fill=th["muted"], max_w=self.w) if spec.get("sub") else None
        self.t0 = self.at(spec.get("at"), 0.25)
        self.dur = float(spec.get("count", 0.9))
        final = self._num(self.value)
        self.final_img = hero(final, self.size, th, max_w=self.w)
        self.h = (self.label.h if self.label else 0) + self.final_img.h + (self.sub.h + 10 if self.sub else 0)
        self.sfx_events = [("ding", self.t0 + self.dur)]

    def _num(self, v: float) -> str:
        body = f"{v:,.{self.decimals}f}"
        return f"{self.prefix}{body}{self.unit}"

    def state(self, t):
        p = ease_out_cubic((t - self.t0) / self.dur)
        sub = round(clamp01((t - self.t0 - self.dur) / 0.3), 2) if self.sub else 0
        return (round(p, 3), round(clamp01((t - self.t0 + 0.3) / 0.3), 2), round(clamp01(t / 0.25), 2), sub)

    def draw(self, t):
        c = Canvas(self.w, int(self.h))
        y = 0
        if self.label:
            s, a = pop(t, 0.25, 0.85)
            c.put(self.label, (self.w - self.label.w) / 2, y, alpha=a, scale=s)
            y += self.label.h - 6
        p = ease_out_cubic((t - self.t0) / self.dur)
        if t >= self.t0 - 0.2:
            img = self.final_img if p >= 1 else hero(self._num(self.value * p), self.size, self.th, max_w=self.w)
            s, a = pop(t - self.t0 + 0.2, 0.3, 0.7)
            c.put(img, (self.w - img.w) / 2, y + (self.final_img.h - img.h) / 2, alpha=a, scale=s)
        y += self.final_img.h
        if self.sub and t >= self.t0 + self.dur:
            c.put(self.sub, (self.w - self.sub.w) / 2, y + 10, alpha=clamp01((t - self.t0 - self.dur) / 0.3))
        return c.img()


class ImageCard(Card):
    """사용자 이미지(캡처 화면 등)를 둥근 테두리 안에 넣고 천천히 확대."""

    def __init__(self, spec, th, timing, base_dir: Path | None = None):
        super().__init__(spec, th, timing)
        path = Path(spec.get("path", ""))
        if not path.is_absolute() and base_dir is not None:
            path = base_dir / path
        if not path.exists():
            raise EditorError(f"이미지 파일이 없습니다: {path}")
        im = Image.open(path).convert("RGB")
        max_w, max_h = CARD_W, int(spec.get("max_h", 760))
        scale = min(max_w / im.width, max_h / im.height)
        self.w, self.h = int(im.width * scale), int(im.height * scale)
        self.zoom = float(spec.get("zoom", 1.06))
        big = im.resize((int(self.w * self.zoom) + 2, int(self.h * self.zoom) + 2), Image.LANCZOS)
        self.big = np.asarray(big, np.float32)[..., ::-1]
        mask = Image.new("L", (self.w * SS, self.h * SS), 0)
        ImageDraw.Draw(mask).rounded_rectangle([0, 0, self.w * SS - 1, self.h * SS - 1], radius=32 * SS, fill=255)
        self.mask = np.asarray(mask.resize((self.w, self.h), Image.LANCZOS), np.float32)[..., None] / 255.0
        self.border = rounded(self.w, self.h, 32, (0, 0, 0), border=(60, 60, 64), bw=3, alpha=0.0)
        self.dur = max(0.5, timing.duration)

    def state(self, t):
        return round(t / self.dur * 60)  # 확대는 천천히라 1/60 단계면 충분

    def draw(self, t):
        import cv2

        z = 1 + (self.zoom - 1) * clamp01(t / self.dur)
        cw, ch = self.w * z, self.h * z
        bh, bw = self.big.shape[:2]
        x0 = (bw - cw) / 2
        y0 = (bh - ch) / 2
        m = np.array([[cw / self.w, 0, x0], [0, ch / self.h, y0]], np.float32)
        crop = cv2.warpAffine(self.big, m, (self.w, self.h), flags=cv2.INTER_LINEAR | cv2.WARP_INVERSE_MAP)
        img = Img(crop * self.mask, self.mask.copy())
        c = Canvas(self.w, self.h)
        c.put(img, 0, 0)
        c.put(self.border, 0, 0)
        return c.img()


class TextCard(Card):
    """제목 + 본문 몇 줄 (어디에도 안 맞을 때)."""

    def __init__(self, spec, th, timing):
        super().__init__(spec, th, timing)
        self.w = CARD_W
        self.title = text(spec.get("title", ""), 64, th, max_w=self.w - 100) if spec.get("title") else None
        self.body = [text(ln, 50, th, fill=(220, 220, 224), max_w=self.w - 100) for ln in spec.get("lines") or []]
        self.h = 70 + (self.title.h + 20 if self.title else 0) + sum(b.h + 8 for b in self.body) + 40
        self.bg = self.box(self.w, int(self.h), accent=spec.get("accent_border", False))
        defaults = [0.25 + 0.35 * i for i in range(len(self.body))]
        self.when = [self.at(None, d) for d in defaults]

    def draw(self, t):
        c = Canvas(self.w, int(self.h))
        c.put(self.bg, 0, 0)
        y = 50
        if self.title:
            c.put(self.title, (self.w - self.title.w) / 2, y)
            y += self.title.h + 20
        for img, w in zip(self.body, self.when):
            q = ease_out_cubic((t - w) / 0.3)
            c.put(img, (self.w - img.w) / 2, y + (1 - q) * 20, alpha=q)
            y += img.h + 8
        return c.img()


CARD_TYPES = {
    "checklist": ChecklistCard,
    "bigtext": BigTextCard,
    "compare": CompareCard,
    "calendar": CalendarCard,
    "comment": CommentCard,
    "waveform": WaveformCard,
    "steps": StepsCard,
    "stat": StatCard,
    "image": ImageCard,
    "text": TextCard,
}


def make_card(spec: dict, th: dict, timing, base_dir: Path | None = None) -> Card:
    kind = (spec or {}).get("type", "text")
    cls = CARD_TYPES.get(kind)
    if cls is None:
        raise EditorError(f"모르는 카드 종류: {kind} (가능: {', '.join(CARD_TYPES)})")
    if cls is ImageCard:
        return cls(spec, th, timing, base_dir)
    return cls(spec, th, timing)


# ---------- 카드 위에 얹는 것 ----------
class Pills:
    """카드 주변에 떠 있는 작은 꼬리표들 (✂ 컷 편집, 💬 자막...)."""

    POS = {"tl": (0.02, 0.0), "tr": (0.98, 0.04), "bl": (0.02, 0.96), "br": (0.98, 0.92), "t": (0.5, -0.04), "b": (0.5, 1.02)}

    def __init__(self, items: list, th: dict, timing):
        self.items = []
        self.sfx_events = []
        order = ["tl", "tr", "bl", "br", "t", "b"]
        defaults = [0.3 + 0.35 * i for i in range(len(items))]
        for i, it in enumerate(items):
            it = it if isinstance(it, dict) else {"text": str(it)}
            txt = text(it.get("text", ""), 38, th)
            ic = icon(it.get("icon"), 40, th)
            w = txt.w + (ic.w + 10 if ic else 0) + 44
            h = max(txt.h, 40) + 22
            c = Canvas(w, h)
            c.put(rounded(w, h, h / 2, (26, 26, 28), border=(70, 70, 76), bw=2), 0, 0)
            x = 22
            if ic:
                c.put(ic, x, (h - ic.h) / 2)
                x += ic.w + 10
            c.put(txt, x, (h - txt.h) / 2)
            pos = it.get("pos", order[i % len(order)])
            when = timing.at(it.get("at"), defaults[i])
            self.items.append((c.img(), self.POS.get(pos, (0.5, 1.0)) if isinstance(pos, str) else tuple(pos), when, i))
            self.sfx_events.append(("pop", when))

    def draw(self, canvas: Canvas, box: tuple, t: float) -> None:
        x0, y0, w, h = box
        for img, (fx, fy), when, i in self.items:
            p = t - when
            if p < 0:
                continue
            s, a = pop(p, 0.28, 0.5)
            bob = 5 * math.sin(2 * math.pi * (t * 0.5 + i * 0.27))
            cx, cy = x0 + fx * w, y0 + fy * h + bob
            cx = min(max(cx, img.w / 2 + 24), 1080 - img.w / 2 - 24)  # 화면 밖으로 나가지 않게
            canvas.put(img, cx - img.w / 2, cy - img.h / 2, alpha=a, scale=s)


class Stamp:
    """도장: 둥근 테두리 안의 글자가 쿵 하고 찍힌다 (예: '직접 편집 필요없음', '끝!')."""

    def __init__(self, spec: dict, th: dict, timing):
        d_ = int(spec.get("size", 300))
        c = Canvas(d_, d_)
        c.put(disc(d_, (16, 30, 10), ring=th["accent"], rw=10), 0, 0)
        txt = text(spec.get("text", "끝!"), int(d_ * 0.2), th, weight="black", fill=th["accent"], accent=th["text"], max_w=int(d_ * 0.78))
        c.put(txt, (d_ - txt.w) / 2, (d_ - txt.h) / 2)
        self.img = c.img().rotated(float(spec.get("angle", 14)))
        self.pos = spec.get("pos", [0.78, 0.72])
        self.when = timing.at(spec.get("at"), max(0.4, timing.duration * 0.6))
        self.sfx_events = [("ding", self.when)]

    def draw(self, canvas: Canvas, box: tuple, t: float) -> None:
        p = t - self.when
        if p < 0:
            return
        q = ease_out_cubic(p / 0.22)
        s = 1.8 - 0.8 * q
        x0, y0, w, h = box
        canvas.put(self.img, x0 + self.pos[0] * w - self.img.w / 2, y0 + self.pos[1] * h - self.img.h / 2, alpha=q, scale=s)
