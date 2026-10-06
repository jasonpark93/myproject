"""화면 그래픽(Pillow). 1080x1920 세로 화면 위에 올라가는 카드·자막·리스트·배경과 채널 아트를 그린다.

쇼츠 앱은 화면 아래쪽(제목·채널명)과 오른쪽(좋아요·댓글 버튼)을 가리므로, 글자는 SAFE 영역 안에만 둔다.
"""

from __future__ import annotations

import os
import random
from functools import lru_cache
from pathlib import Path

from PIL import Image, ImageDraw, ImageFilter, ImageFont

from . import textutil

W, H = 1080, 1920
SAFE_LEFT, SAFE_RIGHT = 80, 940
CENTER_X = (SAFE_LEFT + SAFE_RIGHT) // 2  # 오른쪽 버튼 영역을 피해 가운데 정렬 기준을 왼쪽으로 옮긴다
SUBTITLE_Y = 1235  # 자막 덩어리의 세로 중심

_FONT_CANDIDATES = {
    "bold": [
        ("/usr/share/fonts/opentype/noto/NotoSansCJK-Bold.ttc", 1),
        ("/usr/share/fonts/noto-cjk/NotoSansCJK-Bold.ttc", 1),
        ("/System/Library/Fonts/AppleSDGothicNeo.ttc", 6),
        ("C:/Windows/Fonts/malgunbd.ttf", 0),
    ],
    "regular": [
        ("/usr/share/fonts/opentype/noto/NotoSansCJK-Regular.ttc", 1),
        ("/usr/share/fonts/noto-cjk/NotoSansCJK-Regular.ttc", 1),
        ("/System/Library/Fonts/AppleSDGothicNeo.ttc", 0),
        ("C:/Windows/Fonts/malgun.ttf", 0),
    ],
}


class FontMissing(RuntimeError):
    pass


@lru_cache(maxsize=64)
def font(size: int, weight: str = "bold") -> ImageFont.FreeTypeFont:
    """한글 폰트. STUDIO_FONT_BOLD / STUDIO_FONT_REGULAR 환경변수로 경로를 바꿀 수 있다."""
    override = os.environ.get(f"STUDIO_FONT_{weight.upper()}")
    candidates = [(override, 0)] if override else _FONT_CANDIDATES[weight]
    for path, index in candidates:
        if path and Path(path).exists():
            return ImageFont.truetype(path, size, index=index)
    raise FontMissing("한글 폰트를 찾지 못했습니다. Ubuntu라면 'sudo apt-get install fonts-noto-cjk'로 설치하세요.")


def hex_rgb(value: str, alpha: int | None = None) -> tuple:
    r, g, b = (int(value[i : i + 2], 16) for i in (1, 3, 5))
    return (r, g, b) if alpha is None else (r, g, b, alpha)


def _blank() -> Image.Image:
    return Image.new("RGBA", (W, H), (0, 0, 0, 0))


def wrap(text: str, fnt: ImageFont.FreeTypeFont, max_width: int, max_lines: int = 3) -> list[str]:
    """띄어쓰기 기준 줄바꿈. 한 어절이 너무 길면 글자 단위로 자른다."""
    lines: list[str] = []
    current = ""
    for word in text.split():
        candidate = f"{current} {word}".strip()
        if fnt.getlength(candidate) <= max_width:
            current = candidate
            continue
        if current:
            lines.append(current)
        current = ""
        for ch in word:
            if fnt.getlength(current + ch) > max_width and current:
                lines.append(current)
                current = ch
            else:
                current += ch
    if current:
        lines.append(current)
    if len(lines) > max_lines:
        lines = lines[: max_lines - 1] + [" ".join(lines[max_lines - 1 :])]
    return lines


def fit_font(text: str, max_width: int, sizes: tuple[int, ...], max_lines: int, weight: str = "bold"):
    """주어진 줄 수 안에 들어가는 가장 큰 글자 크기를 고른다. 마지막 줄에 한두 글자만 남는 배치는 피한다."""
    fallback = None
    for size in sizes:
        fnt = font(size, weight)
        lines = wrap(text, fnt, max_width, max_lines=99)
        if len(lines) > max_lines:
            continue
        if len(lines) > 1 and len(lines[-1].replace(" ", "")) <= 2:
            fallback = fallback or (fnt, lines)
            continue
        return fnt, lines
    if fallback:
        return fallback
    fnt = font(sizes[-1], weight)
    return fnt, wrap(text, fnt, max_width, max_lines)


def _line_height(fnt: ImageFont.FreeTypeFont) -> int:
    return int(fnt.size * 1.28)


def _draw_lines(draw, lines, fnt, y, fill, *, align="center", x=CENTER_X, stroke=0, stroke_fill=(0, 0, 0)) -> int:
    for line in lines:
        width = fnt.getlength(line)
        lx = x - width / 2 if align == "center" else x
        draw.text((lx, y), line, font=fnt, fill=fill, stroke_width=stroke, stroke_fill=stroke_fill)
        y += _line_height(fnt)
    return y


def background(brand: dict, seed: str, size: tuple[int, int] = (1320, 2340)) -> Image.Image:
    """화면보다 큰 그라디언트 배경. ffmpeg가 이 이미지 위를 천천히 움직이며 잘라내 움직임을 만든다."""
    w, h = size
    top, bottom = hex_rgb(brand["bg_top"]), hex_rgb(brand["bg_bottom"])
    base = Image.new("RGB", (1, h))
    for yy in range(h):
        t = yy / (h - 1)
        base.putpixel((0, yy), tuple(int(top[i] + (bottom[i] - top[i]) * t) for i in range(3)))
    img = base.resize((w, h))
    rng = random.Random(seed)
    glow = Image.new("RGBA", (w, h), (0, 0, 0, 0))
    gdraw = ImageDraw.Draw(glow)
    for color in (brand["primary"], brand["accent"], brand["primary"]):
        r = rng.randint(260, 460)
        cx, cy = rng.randint(0, w), rng.randint(0, h)
        gdraw.ellipse((cx - r, cy - r, cx + r, cy + r), fill=hex_rgb(color, rng.randint(60, 95)))
    glow = glow.filter(ImageFilter.GaussianBlur(140))
    img = Image.alpha_composite(img.convert("RGBA"), glow)
    grain = Image.effect_noise((w, h), 18).convert("L").point(lambda v: 8 if v > 140 else 0)
    img.putalpha(255)
    noise = Image.merge("RGBA", (grain, grain, grain, grain))
    return Image.alpha_composite(img, noise).convert("RGB")


def _pill(draw, xy, text, fnt, fill, text_fill):
    x, y = xy
    width = fnt.getlength(text)
    pad_x, height = 26, int(fnt.size * 1.6)
    draw.rounded_rectangle((x, y, x + width + pad_x * 2, y + height), radius=height // 2, fill=fill)
    draw.text((x + pad_x, y + (height - fnt.size) / 2 - fnt.size * 0.12), text, font=fnt, fill=text_fill)
    return x + width + pad_x * 2


def _dim_layer(strength: int) -> Image.Image:
    """배경 영상 위 글자가 잘 보이도록 위·아래를 어둡게."""
    layer = Image.new("L", (1, H))
    for yy in range(H):
        t = yy / (H - 1)
        edge = max(0.0, 1 - min(t, 1 - t) * 3.2)  # 위·아래 가장자리에서 진하게
        layer.putpixel((0, yy), int(strength * (0.55 + 0.45 * edge)))
    alpha = layer.resize((W, H))
    black = Image.new("RGBA", (W, H), (0, 0, 0, 0))
    black.putalpha(alpha)
    return black


def scene_card(
    *,
    brand: dict,
    channel: str,
    series_label: str,
    headline: str,
    visual: dict,
    progress: float,
    dim: bool,
) -> Image.Image:
    """장면 위에 고정으로 올라가는 그래픽: 시리즈 표시, 큰 제목, 핵심 시각자료, 진행 막대."""
    img = _dim_layer(150) if dim else _blank()
    draw = ImageDraw.Draw(img)
    text = hex_rgb(brand["text"])
    accent = hex_rgb(brand["accent"])
    primary = hex_rgb(brand["primary"])

    # 진행 막대 + 채널 이름
    draw.rounded_rectangle((SAFE_LEFT, 150, SAFE_RIGHT, 160), radius=5, fill=(255, 255, 255, 60))
    filled = SAFE_LEFT + (SAFE_RIGHT - SAFE_LEFT) * max(0.02, min(progress, 1.0))
    draw.rounded_rectangle((SAFE_LEFT, 150, filled, 160), radius=5, fill=accent + (255,))
    draw.text((SAFE_LEFT, 182), channel, font=font(34), fill=(255, 255, 255, 190))

    _pill(draw, (SAFE_LEFT, 250), series_label, font(36), primary + (255,), (255, 255, 255))

    kind = (visual or {}).get("type", "title")
    y = 350
    if headline:
        sizes = (118, 108, 98, 90) if kind == "title" else (96, 88, 80, 74)
        fnt, lines = fit_font(headline, SAFE_RIGHT - SAFE_LEFT, sizes, max_lines=2)
        y = _draw_lines(draw, lines, fnt, y, text, align="left", x=SAFE_LEFT, stroke=3, stroke_fill=(0, 0, 0))
    _draw_visual(draw, img, visual or {}, top=max(y + 40, 600), brand=brand)
    return img


def _draw_visual(draw, img, visual: dict, *, top: int, brand: dict) -> None:
    kind = visual.get("type", "title")
    text = hex_rgb(brand["text"])
    accent = hex_rgb(brand["accent"])
    primary = hex_rgb(brand["primary"])
    width = SAFE_RIGHT - SAFE_LEFT
    items = [str(i) for i in visual.get("items") or []]

    if kind == "number":
        fnt, lines = fit_font(str(visual.get("value", "")), width, (220, 190, 160, 130), max_lines=1)
        y = _draw_lines(draw, lines, fnt, top, accent, stroke=6, stroke_fill=(0, 0, 0))
        if visual.get("label"):
            lfnt, llines = fit_font(str(visual["label"]), width, (60, 54, 48), max_lines=2, weight="regular")
            _draw_lines(draw, llines, lfnt, y + 10, (235, 235, 235), stroke=2)
    elif kind in ("list", "check"):
        fnt = font(56)
        y = top
        for i, item in enumerate(items[:5], start=1):
            box_h = 108
            draw.rounded_rectangle((SAFE_LEFT, y, SAFE_RIGHT, y + box_h), radius=26, fill=(0, 0, 0, 120))
            draw.ellipse((SAFE_LEFT + 22, y + 22, SAFE_LEFT + 86, y + 86), fill=(accent if kind == "check" else primary) + (255,))
            if kind == "check":  # 폰트에 ✓ 글리프가 없을 수 있어 직접 그린다
                cx, cy = SAFE_LEFT + 54, y + 54
                draw.line([(cx - 17, cy + 1), (cx - 5, cy + 14), (cx + 18, cy - 13)], fill=(20, 20, 20), width=8, joint="curve")
            else:
                mfnt = font(46)
                draw.text((SAFE_LEFT + 54 - mfnt.getlength(str(i)) / 2, y + 25), str(i), font=mfnt, fill=(255, 255, 255))
            line = wrap(item, fnt, width - 140, max_lines=1)[0]
            draw.text((SAFE_LEFT + 112, y + 20), line, font=fnt, fill=text)
            y += box_h + 18
    elif kind == "versus" and len(items) == 2:
        gap = 96  # 가운데 VS 배지가 들어갈 자리
        box_w = (width - gap) // 2
        box_h = 330
        for idx, item in enumerate(items):
            x0 = SAFE_LEFT + idx * (box_w + gap)
            color = primary if idx == 0 else accent
            draw.rounded_rectangle((x0, top, x0 + box_w, top + box_h), radius=36, fill=(0, 0, 0, 140), outline=color + (255,), width=6)
            fnt, lines = fit_font(item, box_w - 56, (64, 58, 52, 46, 40), max_lines=3)
            block = len(lines) * _line_height(fnt)
            _draw_lines(draw, lines, fnt, top + (box_h - block) // 2, text, x=x0 + box_w // 2)
        vs = font(46)
        cx, cy, r = SAFE_LEFT + box_w + gap // 2, top + box_h // 2, 44
        draw.ellipse((cx - r, cy - r, cx + r, cy + r), fill=(255, 255, 255, 255))
        draw.text((cx - vs.getlength("VS") / 2, cy - vs.size * 0.68), "VS", font=vs, fill=(20, 20, 20))
    elif kind == "quote" and visual.get("value"):
        fnt, lines = fit_font(f"“{visual['value']}”", width - 60, (78, 70, 62, 56), max_lines=4)
        block = len(lines) * _line_height(fnt) + 70
        draw.rounded_rectangle((SAFE_LEFT, top, SAFE_RIGHT, top + block), radius=36, fill=(255, 255, 255, 235))
        _draw_lines(draw, lines, fnt, top + 35, (25, 25, 25))


def subtitle(chunk_words: list[textutil.Word], brand: dict) -> Image.Image:
    """화면 아래쪽 큰 자막. [[강조]] 어절은 강조색으로."""
    img = _blank()
    draw = ImageDraw.Draw(img)
    max_width = SAFE_RIGHT - SAFE_LEFT
    size = 84
    fnt = font(size)
    space = fnt.getlength(" ")

    def layout(fnt_):
        lines, current, current_w = [], [], 0.0
        for word in chunk_words:
            w = sum(fnt_.getlength(t) for t, _ in word)
            extra = w if not current else w + space
            if current and current_w + extra > max_width:
                lines.append((current, current_w))
                current, current_w, extra = [], 0.0, w
            current.append(word)
            current_w += extra
        if current:
            lines.append((current, current_w))
        return lines

    lines = layout(fnt)
    while (len(lines) > 2 or any(w > max_width for _, w in lines)) and size > 56:
        size -= 6
        fnt = font(size)
        space = fnt.getlength(" ")
        lines = layout(fnt)

    line_h = int(size * 1.3)
    y = SUBTITLE_Y - (len(lines) * line_h) // 2
    white = hex_rgb(brand["text"])
    accent = hex_rgb(brand["accent"])
    for words_in_line, line_w in lines:
        x = CENTER_X - line_w / 2
        for wi, word in enumerate(words_in_line):
            if wi:
                x += space
            for text, highlighted in word:
                draw.text((x, y), text, font=fnt, fill=accent if highlighted else white, stroke_width=9, stroke_fill=(0, 0, 0))
                x += fnt.getlength(text)
        y += line_h
    return img


def list_title_card(*, brand: dict, channel: str, series_label: str, list_title: str, subtitle_text: str) -> Image.Image:
    img = _blank()
    draw = ImageDraw.Draw(img)
    _pill(draw, (SAFE_LEFT, 420), series_label, font(40), hex_rgb(brand["primary"]) + (255,), (255, 255, 255))
    fnt, lines = fit_font(list_title, SAFE_RIGHT - SAFE_LEFT, (124, 112, 100, 90), max_lines=3)
    y = _draw_lines(draw, lines, fnt, 540, hex_rgb(brand["text"]), align="left", x=SAFE_LEFT, stroke=4)
    if subtitle_text:
        sfnt, slines = fit_font(subtitle_text, SAFE_RIGHT - SAFE_LEFT, (58, 52, 46), max_lines=2, weight="regular")
        _draw_lines(draw, slines, sfnt, y + 30, hex_rgb(brand["accent"]), align="left", x=SAFE_LEFT, stroke=2)
    draw.text((SAFE_LEFT, 182), channel, font=font(34), fill=(255, 255, 255, 190))
    return img


def list_page_card(*, brand: dict, channel: str, list_title: str, items: list[dict], start_number: int, total: int) -> Image.Image:
    """리스트 한 페이지(최대 5개 항목)."""
    img = _blank()
    draw = ImageDraw.Draw(img)
    text = hex_rgb(brand["text"])
    accent = hex_rgb(brand["accent"])
    tfnt, tlines = fit_font(list_title, SAFE_RIGHT - SAFE_LEFT, (60, 54, 48), max_lines=2)
    y = _draw_lines(draw, tlines, tfnt, 240, accent, align="left", x=SAFE_LEFT, stroke=3)
    y += 30
    ifnt, dfnt = font(58), font(40, "regular")
    for offset, item in enumerate(items):
        number = start_number + offset
        detail = str(item.get("detail", "")).strip()
        box_h = 150 if detail else 112
        draw.rounded_rectangle((SAFE_LEFT, y, SAFE_RIGHT, y + box_h), radius=28, fill=(0, 0, 0, 130))
        nfnt = font(52)
        label = str(number)
        draw.text((SAFE_LEFT + 30, y + 22), label, font=nfnt, fill=accent)
        tx = SAFE_LEFT + 30 + max(nfnt.getlength(str(total)), nfnt.getlength(label)) + 28
        line = wrap(str(item.get("text", "")), ifnt, SAFE_RIGHT - tx - 24, max_lines=1)[0]
        draw.text((tx, y + 20), line, font=ifnt, fill=text)
        if detail:
            dline = wrap(detail, dfnt, SAFE_RIGHT - tx - 24, max_lines=1)[0]
            draw.text((tx, y + 92), dline, font=dfnt, fill=(210, 210, 210))
        y += box_h + 20
    draw.text((SAFE_LEFT, 182), channel, font=font(34), fill=(255, 255, 255, 190))
    return img


def profile_image(channel: str, brand: dict, size: int = 800) -> Image.Image:
    """채널 프로필 사진(원형으로 잘려 보이므로 가운데에 배치)."""
    img = background(brand, "profile", (size, size)).convert("RGBA")
    draw = ImageDraw.Draw(img)
    r = int(size * 0.36)
    cx = cy = size // 2
    draw.ellipse((cx - r, cy - r, cx + r, cy + r), fill=hex_rgb(brand["primary"], 255))
    fnt, lines = fit_font(channel, int(r * 1.6), (int(size * 0.2), int(size * 0.16), int(size * 0.13)), max_lines=2)
    block = len(lines) * _line_height(fnt)
    _draw_lines(draw, lines, fnt, cy - block // 2 - int(fnt.size * 0.12), hex_rgb(brand["text"]), x=cx)
    coin = int(size * 0.08)  # 원형으로 잘려도 보이도록 큰 원 테두리 안쪽에 둔다
    ox, oy = cx + int(r * 0.68), cy - int(r * 0.68)
    draw.ellipse((ox - coin, oy - coin, ox + coin, oy + coin), fill=hex_rgb(brand["accent"], 255))
    return img.convert("RGB")


def banner_image(channel: str, tagline: str, brand: dict) -> Image.Image:
    """채널 배너 2560x1440. 모든 기기에서 보이는 가운데 1546x423 영역에 글자를 둔다."""
    w, h = 2560, 1440
    img = background(brand, "banner", (w, h)).convert("RGBA")
    draw = ImageDraw.Draw(img)
    safe_w, safe_h = 1546, 423
    x0, y0 = (w - safe_w) // 2, (h - safe_h) // 2
    fnt, lines = fit_font(channel, safe_w, (150, 130, 110), max_lines=1)
    y = _draw_lines(draw, lines, fnt, y0 + 40, hex_rgb(brand["text"]), x=w // 2, stroke=4)
    if tagline:
        tfnt, tlines = fit_font(tagline, safe_w, (72, 64, 56), max_lines=1, weight="regular")
        _draw_lines(draw, tlines, tfnt, y + 20, hex_rgb(brand["accent"]), x=w // 2, stroke=2)
    bar_w = 260
    draw.rounded_rectangle((w // 2 - bar_w // 2, y0 + safe_h - 40, w // 2 + bar_w // 2, y0 + safe_h - 26), radius=7, fill=hex_rgb(brand["primary"], 255))
    return img.convert("RGB")

