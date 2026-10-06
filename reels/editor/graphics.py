"""자막·제목 그리기: 흰 글씨 + 검은 테두리 + 옅은 그림자, 강조 단어는 색으로. 프레임에 알파 합성."""

from __future__ import annotations

import os
import re
from pathlib import Path

import numpy as np
from PIL import Image, ImageDraw, ImageFilter, ImageFont

from . import ROOT, W
from .captions import runs
from .util import EditorError

BUNDLED = ROOT / "fonts" / "Pretendard-ExtraBold.otf"
SYSTEM_FONTS = [
    ("/System/Library/Fonts/AppleSDGothicNeo.ttc", None),
    ("C:/Windows/Fonts/malgunbd.ttf", 0),
    ("/usr/share/fonts/opentype/noto/NotoSansCJK-Bold.ttc", 1),
    ("/usr/share/fonts/noto-cjk/NotoSansCJK-Bold.ttc", 1),
]
COLOR_NAMES = {
    "노랑": "#FFE14D", "노란색": "#FFE14D", "yellow": "#FFE14D",
    "민트": "#3DF5C8", "mint": "#3DF5C8",
    "빨강": "#FF4B4B", "빨간색": "#FF4B4B", "red": "#FF4B4B",
    "주황": "#FF9F2E", "주황색": "#FF9F2E", "orange": "#FF9F2E",
    "초록": "#4BE36B", "초록색": "#4BE36B", "green": "#4BE36B",
    "하늘": "#5CC8FF", "하늘색": "#5CC8FF", "파랑": "#3D8BFF", "파란색": "#3D8BFF", "blue": "#3D8BFF",
    "분홍": "#FF7AC8", "분홍색": "#FF7AC8", "핑크": "#FF7AC8", "pink": "#FF7AC8",
    "보라": "#B27CFF", "보라색": "#B27CFF", "purple": "#B27CFF",
    "흰색": "#FFFFFF", "하양": "#FFFFFF", "white": "#FFFFFF",
    "검정": "#000000", "검은색": "#000000", "black": "#000000",
}


def color(value: str) -> tuple:
    value = COLOR_NAMES.get(str(value).strip().lower(), COLOR_NAMES.get(str(value).strip(), str(value).strip()))
    if not re.fullmatch(r"#?[0-9a-fA-F]{6}", value):
        raise EditorError(f"색 이름을 이해하지 못했습니다: {value} (예: #FFE14D, 노랑, 민트)")
    value = value.lstrip("#")
    return tuple(int(value[i : i + 2], 16) for i in (0, 2, 4))


def _system_font(size: int):
    for path, index in SYSTEM_FONTS:
        if not Path(path).exists():
            continue
        if index is not None:
            return ImageFont.truetype(path, size, index=index)
        best = None
        for i in range(16):  # ttc 안에서 가장 굵은 스타일을 고른다
            try:
                f = ImageFont.truetype(path, size, index=i)
            except OSError:
                break
            style = f.getname()[1].lower()
            rank = next((r for r, key in enumerate(("heavy", "extrabold", "bold", "semibold")) if key in style), None)
            if rank is not None and (best is None or rank < best[0]):
                best = (rank, f)
        if best:
            return best[1]
    return None


def font(size: int, path: str | None = None) -> ImageFont.FreeTypeFont:
    for candidate in (path, os.environ.get("REELS_FONT"), str(BUNDLED)):
        if candidate and Path(candidate).exists():
            return ImageFont.truetype(candidate, size)
    found = _system_font(size)
    if found is None:
        raise EditorError("한글 굵은 글꼴을 찾지 못했습니다. reels/fonts/Pretendard-ExtraBold.otf 가 있는지 확인하세요.")
    return found


class Sprite:
    """미리 그려 둔 글자 이미지(BGR, 알파 곱한 값). 팝 등장 애니메이션용 축소판도 만든다."""

    POP = (0.80, 0.93, 1.04, 1.0)  # 처음 4프레임 동안의 크기

    def __init__(self, rgba: np.ndarray, cx: float, cy: float, pop: bool = True):
        self.cx, self.cy = cx, cy
        self.variants = {}
        for scale in set(self.POP if pop else (1.0,)) | {1.0}:
            img = rgba
            if abs(scale - 1.0) > 1e-3:
                import cv2

                h, w = rgba.shape[:2]
                size = (max(1, int(round(w * scale))), max(1, int(round(h * scale))))
                pm = rgba.astype(np.float32)
                pm[..., :3] *= pm[..., 3:4] / 255.0
                pm = cv2.resize(pm, size, interpolation=cv2.INTER_AREA if scale < 1 else cv2.INTER_LINEAR)
                self.variants[scale] = (pm[..., 2::-1].copy(), pm[..., 3:4] / 255.0)
                continue
            pm = img.astype(np.float32)
            alpha = pm[..., 3:4] / 255.0
            self.variants[scale] = ((pm[..., 2::-1] * alpha).copy(), alpha)
        self.pop = pop

    def draw(self, frame: np.ndarray, age_frames: int = 99) -> None:
        scale = self.POP[age_frames] if self.pop and age_frames < len(self.POP) else 1.0
        rgb, alpha = self.variants[scale]
        h, w = alpha.shape[:2]
        x0 = int(round(self.cx - w / 2))
        y0 = int(round(self.cy - h / 2))
        fh, fw = frame.shape[:2]
        xa, ya = max(0, x0), max(0, y0)
        xb, yb = min(fw, x0 + w), min(fh, y0 + h)
        if xb <= xa or yb <= ya:
            return
        sub_rgb = rgb[ya - y0 : yb - y0, xa - x0 : xb - x0]
        sub_a = alpha[ya - y0 : yb - y0, xa - x0 : xb - x0]
        region = frame[ya:yb, xa:xb].astype(np.float32)
        frame[ya:yb, xa:xb] = (region * (1.0 - sub_a) + sub_rgb + 0.5).clip(0, 255).astype(np.uint8)


def text_image(lines: list, fnt, fill: tuple, accent: tuple, outline: tuple, stroke: int, line_gap: float = 0.12) -> np.ndarray:
    """lines: [[(글자, 강조여부), ...], ...] → RGBA 배열. 테두리를 먼저 다 그리고 글자를 위에 그려 겹침을 막는다."""
    ascent, descent = fnt.getmetrics()
    line_h = ascent + descent
    gap = int(line_h * line_gap)
    widths = [sum(fnt.getlength(t) for t, _ in line) for line in lines]
    pad = stroke + 16
    w = int(max(widths or [1])) + pad * 2
    h = line_h * len(lines) + gap * (len(lines) - 1) + pad * 2
    stroke_layer = Image.new("RGBA", (w, h), (0, 0, 0, 0))
    fill_layer = Image.new("RGBA", (w, h), (0, 0, 0, 0))
    ds, df = ImageDraw.Draw(stroke_layer), ImageDraw.Draw(fill_layer)
    for n, line in enumerate(lines):
        x = pad + (w - pad * 2 - widths[n]) / 2
        y = pad + n * (line_h + gap)
        for text, hl in line:
            ds.text((x, y), text, font=fnt, fill=outline + (255,), stroke_width=stroke, stroke_fill=outline + (255,))
            df.text((x, y), text, font=fnt, fill=(accent if hl else fill) + (255,))
            x += fnt.getlength(text)
    shadow = Image.new("RGBA", (w, h), (0, 0, 0, 0))
    alpha = stroke_layer.getchannel("A").filter(ImageFilter.GaussianBlur(max(2, stroke // 2 + 2)))
    shadow.putalpha(alpha.point(lambda v: int(v * 0.45)))
    shadow = shadow.transform((w, h), Image.AFFINE, (1, 0, 0, 0, 1, -max(2, stroke // 2)))
    out = Image.alpha_composite(shadow, stroke_layer)
    out = Image.alpha_composite(out, fill_layer)
    return np.asarray(out)


def caption_sprite(text: str, highlights: list, style: dict, pop: bool = True) -> Sprite:
    size = int(round(78 * float(style.get("size", 1.0))))
    max_width = int(style.get("max_width", 880))
    fnt = font(size, style.get("font") or None)
    width = fnt.getlength(text)
    if width > max_width:  # 너무 길면 한 줄에 맞게 줄인다
        size = max(int(size * 0.62), int(size * max_width / width))
        fnt = font(size, style.get("font") or None)
    stroke = max(3, int(round(size * 0.12)))
    rgba = text_image([runs(text, highlights)], fnt, color(style.get("color", "#FFFFFF")), color(style.get("highlight", "#FFE14D")), color(style.get("outline", "#000000")), stroke)
    cx = W / 2 - 20  # 오른쪽 버튼(좋아요·댓글)을 조금 피한다
    cy = style["center_y"]
    return Sprite(rgba, cx, cy, pop=pop)


_MARK = re.compile(r"\[\[(.+?)\]\]")


def title_sprite(text: str, style: dict) -> Sprite:
    """상단 고정 제목: 최대 2줄, [[강조]] 표시는 강조색."""
    size = int(round(70 * float(style.get("size", 1.0))))
    fnt = font(size, style.get("font") or None)
    lines = []
    for raw in [ln for ln in text.split("\n") if ln.strip()][:2]:
        highlights = _MARK.findall(raw)
        plain = _MARK.sub(lambda m: m.group(1), raw).strip()
        lines.append(runs(plain, highlights))
    widest = max((sum(fnt.getlength(t) for t, _ in line) for line in lines), default=1)
    if widest > 900:
        size = max(int(size * 0.6), int(size * 900 / widest))
        fnt = font(size, style.get("font") or None)
    stroke = max(3, int(round(size * 0.12)))
    rgba = text_image(lines, fnt, color(style.get("color", "#FFFFFF")), color(style.get("highlight", "#FFE14D")), color(style.get("outline", "#000000")), stroke)
    return Sprite(rgba, W / 2, style["center_y"], pop=False)
