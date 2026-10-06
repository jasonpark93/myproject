"""코드로 그리는 그림. 지금은 청약통장(‘통장 깨기’ 연출: 금이 가고 두 동강 나는 장면)."""

from __future__ import annotations

import numpy as np
from PIL import Image, ImageDraw, ImageFilter

from .cards import SS, Img, clamp01
from .graphics import weight_font

PAD = 40  # 그림자 여백


def _lerp(a, b, f):
    return tuple(int(round(x + (y - x) * f)) for x, y in zip(a, b))


class Passbook:
    """통장 한 권. whole(온전한 통장), crack(p)(금이 p만큼 간 선), left/right(갈라진 두 쪽), shards(파편)."""

    def __init__(self, w: int = 560, h: int = 360, th: dict | None = None, label: str = "주택청약종합저축",
                 since: str = "가입일 2019.03.04", count: str = "납입 84회", seed: int = 4):
        th = th or {}
        self.bw, self.bh = w, h
        self.w, self.h = w + PAD * 2, h + PAD * 2
        rng = np.random.default_rng(seed)
        cover_top, cover_bottom = (22, 156, 138), (11, 104, 93)
        gold = (240, 206, 120)
        big = Image.new("RGBA", (self.w * SS, self.h * SS), (0, 0, 0, 0))
        S = SS
        ox, oy = PAD * S, PAD * S
        # 그림자
        sh = Image.new("L", big.size, 0)
        ImageDraw.Draw(sh).rounded_rectangle([ox + 10 * S, oy + 22 * S, ox + (w + 6) * S, oy + (h + 14) * S], radius=26 * S, fill=95)
        sh = sh.filter(ImageFilter.GaussianBlur(14 * S))
        shadow = Image.new("RGBA", big.size, (60, 45, 30, 0))
        shadow.putalpha(sh)
        big = Image.alpha_composite(big, shadow)
        d = ImageDraw.Draw(big)
        # 속지 (오른쪽 아래로 살짝 보이는 종이)
        d.rounded_rectangle([ox + 12 * S, oy + 10 * S, ox + (w + 8) * S, oy + (h + 7) * S], radius=22 * S, fill=(250, 246, 236, 255), outline=(214, 204, 184, 255), width=2 * S)
        for k in (3, 6):
            d.line([(ox + (w + 8 - k) * S, oy + 30 * S), (ox + (w + 8 - k) * S, oy + (h - 10) * S)], fill=(222, 212, 192, 255), width=S)
        # 표지 (세로 그라데이션)
        cover = Image.new("RGBA", (w * S, h * S), (0, 0, 0, 0))
        grad = np.linspace(0, 1, h * S)[:, None]
        rgb = np.array(cover_top, np.float32) * (1 - grad[..., None]) + np.array(cover_bottom, np.float32) * grad[..., None]
        arr = np.zeros((h * S, w * S, 4), np.uint8)
        arr[..., :3] = np.broadcast_to(rgb, (h * S, w * S, 3)).astype(np.uint8)
        mask = Image.new("L", (w * S, h * S), 0)
        ImageDraw.Draw(mask).rounded_rectangle([0, 0, w * S - 1, h * S - 1], radius=24 * S, fill=255)
        arr[..., 3] = np.asarray(mask)
        cover = Image.fromarray(arr, "RGBA")
        cd = ImageDraw.Draw(cover)
        cd.rectangle([0, 0, 34 * S, h * S], fill=(0, 0, 0, 38))  # 제본 부분
        cd.line([(34 * S, 0), (34 * S, h * S)], fill=(255, 255, 255, 40), width=2 * S)
        # 금박 집 모양 + 글자
        hx, hy, hs = 74 * S, 54 * S, 64 * S
        cd.polygon([(hx, hy + hs * 0.45), (hx + hs / 2, hy), (hx + hs, hy + hs * 0.45)], fill=gold + (255,))
        cd.rectangle([hx + hs * 0.14, hy + hs * 0.42, hx + hs * 0.86, hy + hs], fill=gold + (255,))
        cd.rectangle([hx + hs * 0.42, hy + hs * 0.64, hx + hs * 0.58, hy + hs], fill=cover_top + (255,))
        cd.text((hx + hs + 20 * S, hy + 8 * S), "청약통장", font=weight_font(32 * S, "extrabold"), fill=gold + (255,))
        title_font = weight_font(50 * S, "black")
        while title_font.getlength(label) > (w - 110) * S and title_font.size > 20 * S:
            title_font = weight_font(int(title_font.size * 0.92), "black")
        cd.text((74 * S, 150 * S), label, font=title_font, fill=(255, 255, 255, 255))
        cd.line([(74 * S, 236 * S), ((w - 40) * S, 236 * S)], fill=gold + (200,), width=3 * S)
        small = weight_font(28 * S, "bold")
        cd.text((74 * S, 262 * S), since, font=small, fill=(225, 240, 232, 255))
        cd.text(((w - 40) * S - small.getlength(count), 262 * S), count, font=small, fill=(225, 240, 232, 255))
        cd.text((74 * S, 304 * S), "★ 1순위", font=weight_font(28 * S, "extrabold"), fill=gold + (255,))
        big.alpha_composite(cover, (ox, oy))
        self.whole_pil = big.resize((self.w, self.h), Image.LANCZOS)
        self.whole = Img.from_pil(self.whole_pil)
        # 금: 위에서 아래로 지그재그
        cx = PAD + w * 0.53
        n = 8
        xs = [cx + (rng.uniform(-28, 28) if 0 < k < n else 0) + (k / n) * (-26) for k in range(n + 1)]
        ys = [PAD - 4 + (h + 8) * k / n for k in range(n + 1)]
        self.crack_pts = list(zip(xs, ys))
        self._crack = {}
        left_poly = [(0, 0), (xs[0], 0)] + self.crack_pts + [(xs[-1], self.h), (0, self.h)]
        right_poly = [(self.w, 0), (xs[0], 0)] + self.crack_pts + [(xs[-1], self.h), (self.w, self.h)]
        self.left = self._half(left_poly)
        self.right = self._half(right_poly)
        # 파편: 금 따라 작은 조각들
        self.shards = []
        colors = [cover_top, cover_bottom, gold, (250, 246, 236)]
        for k in range(7):
            px, py = self.crack_pts[1 + k % (n - 1)]
            s = rng.uniform(14, 26)
            im = Image.new("RGBA", (int(s * 2 * SS), int(s * 2 * SS)), (0, 0, 0, 0))
            pts = [(s * SS + s * SS * np.cos(a) * rng.uniform(0.5, 1), s * SS + s * SS * np.sin(a) * rng.uniform(0.5, 1)) for a in np.linspace(0, 2 * np.pi, 4)[:-1] + rng.uniform(0, 6)]
            ImageDraw.Draw(im).polygon(pts, fill=colors[k % len(colors)] + (255,))
            shard = Img.from_pil(im.resize((int(s * 2), int(s * 2)), Image.LANCZOS))
            vx = rng.uniform(140, 420) * (1 if k % 2 else -1)
            vy = rng.uniform(-520, -220)
            self.shards.append((shard, px, py, vx, vy, rng.uniform(-400, 400)))

    def _half(self, poly) -> Img:
        mask = Image.new("L", (self.w * SS, self.h * SS), 0)
        ImageDraw.Draw(mask).polygon([(x * SS, y * SS) for x, y in poly], fill=255)
        mask = mask.resize((self.w, self.h), Image.LANCZOS)
        im = self.whole_pil.copy()
        a = np.asarray(im.getchannel("A"), np.float32) * (np.asarray(mask, np.float32) / 255)
        im.putalpha(Image.fromarray(a.astype(np.uint8)))
        edge = Image.new("RGBA", im.size, (0, 0, 0, 0))  # 깨진 단면을 살짝 어둡게
        ImageDraw.Draw(edge).line(self.crack_pts, fill=(20, 40, 36, 150), width=4)
        edge.putalpha(Image.fromarray((np.asarray(edge.getchannel("A"), np.float32) * np.asarray(mask, np.float32) / 255).astype(np.uint8)))
        return Img.from_pil(Image.alpha_composite(im, edge))

    def crack(self, progress: float) -> Img:
        """금이 위에서부터 progress만큼 간 선."""
        p = round(clamp01(progress), 2)
        if p not in self._crack:
            im = Image.new("RGBA", (self.w * SS, self.h * SS), (0, 0, 0, 0))
            d = ImageDraw.Draw(im)
            pts = self.crack_pts
            total = len(pts) - 1
            upto = p * total
            seg = [pts[0]]
            for k in range(1, len(pts)):
                if k <= upto:
                    seg.append(pts[k])
                else:
                    f = upto - (k - 1)
                    if f > 0:
                        x0, y0 = pts[k - 1]
                        x1, y1 = pts[k]
                        seg.append((x0 + (x1 - x0) * f, y0 + (y1 - y0) * f))
                    break
            if len(seg) > 1:
                d.line([(x * SS, y * SS) for x, y in seg], fill=(24, 30, 28, 255), width=6 * SS, joint="curve")
                d.line([(x * SS + 3 * SS, y * SS) for x, y in seg], fill=(255, 255, 255, 110), width=2 * SS)
            self._crack[p] = Img.from_pil(im.resize((self.w, self.h), Image.LANCZOS))
        return self._crack[p]
