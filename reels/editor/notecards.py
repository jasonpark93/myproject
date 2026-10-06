"""노트 테마에 맞춘 새 카드 (네온 테마에서도 동작):
- hook   첫 2초 훅: 손글씨 '잠깐!' + 청약통장 그림이 쩍 갈라짐 + 큰 글씨
- chart  꺾은선 그래프: 선이 그려지고 마지막 점에 빨간 펜 동그라미 + 포스트잇 배지 + 출처
- stack  합계 막대: 가점 84점 = 32 + 35 + 17, 중요한 칸에 펜 동그라미와 손글씨 메모
- phone  은행 앱 화면: 금액 변경(취소선 → 새 금액) → 버튼 톡 → '완료' 알림 + 옆에 포스트잇
- notes  포스트잇 2~3장: 제목 + 큰 숫자 + 조건
- cta    마무리: 큰 글씨 + 저장 버튼 톡 + 투표 버튼 두 개

모든 시각(at)은 대본 단어에 맞춰진다.
"""

from __future__ import annotations

import math
import re

import numpy as np
from PIL import Image, ImageDraw

from .cards import (
    CARD_W, SS, STICKY_PAD, Canvas, Card, Img, _big, _small, clamp01, col, disc, ease_out_back, ease_out_cubic, hand, hero,
    icon, is_note, pen_circle, pen_line, plain, pop, rounded, soft_shadow, sticker, sticky_pil, text,
)
from .util import EditorError

HERO_LINE = re.compile(r"\s*\[\[.+\]\]\s*")


def _line_times(card: Card, at, n: int, defaults: list) -> list:
    at = at or []
    if isinstance(at, str):
        at = [at]
    return [card.at(at[i] if i < len(at) else None, d) for i, d in enumerate(defaults[:n])]


def _sticky_color(th: dict, name, i: int = 0) -> tuple:
    pal = th.get("sticky") or {}
    if isinstance(name, str) and name in pal:
        return pal[name]
    if name is not None:
        return col(th, name)
    order = ["yellow", "mint", "pink", "blue"]
    return pal.get(order[i % len(order)], th["cream"])


def slap(t: float, dur: float = 0.26) -> tuple:
    """포스트잇·배지가 '착' 붙는 움직임: (크기, 투명도). 크게 떴다가 제자리로."""
    if t < 0:
        return 0.0, 0.0
    q = ease_out_back(t / dur, 2.2)
    return 1.35 - 0.35 * q, clamp01(t / (dur * 0.4))


class _Lines:
    """여러 줄 큰 글씨: [[ ]]로 감싼 줄은 형광펜(노트)·네온 글씨, 나머지는 보통 글씨."""

    def __init__(self, lines: list, th: dict, hero_size: int, plain_size: int, max_w: int):
        self.th, self.hero_size, self.max_w = th, hero_size, max_w
        self.items = []
        for ln in lines:
            if HERO_LINE.fullmatch(ln):
                raw = plain(ln).strip()
                self.items.append(("hero", raw, hero(raw, hero_size, th, max_w=max_w, marker=0 if is_note(th) else 1)))
            else:
                self.items.append(("plain", ln, text(ln, plain_size, th, max_w=max_w)))
        self._anim = {}

    def heights(self) -> list:
        return [img.h - (14 if k == "hero" else 8) for k, _, img in self.items]

    def image(self, i: int, t_since: float) -> Img:
        kind, raw, img = self.items[i]
        if kind != "hero" or not is_note(self.th):
            return img
        p = round(clamp01((t_since - 0.12) / 0.32), 2)
        key = (i, p)
        if key not in self._anim:
            self._anim[key] = hero(raw, self.hero_size, self.th, max_w=self.max_w, marker=p)
        return self._anim[key]


# ---------- 훅 ----------
class HookCard(Card):
    """첫 2초 훅. top(손글씨 한마디) + object(passbook: 청약통장 그림, 또는 스티커 이름) + lines(큰 글씨).
    crack_at에 통장에 금이 가고 두 동강 난다 (화면 흔들림 + 쩍 소리)."""

    def __init__(self, spec, th, timing):
        super().__init__(spec, th, timing)
        from . import illust

        self.w = CARD_W
        self.top_img = hand(spec["top"], 124, th["pen"]).rotated(-7) if spec.get("top") else None
        self.book = None
        self.obj = None
        obj = spec.get("object", "passbook")
        obj_h = 0
        if obj == "passbook":
            self.book = illust.Passbook(int(spec.get("object_w", 560)), int(spec.get("object_h", 360)), th,
                                        label=spec.get("object_label", "주택청약종합저축"),
                                        since=spec.get("since", "가입일 2019.03.04"), count=spec.get("count", "납입 84회"))
            obj_h = self.book.h
        elif obj:
            self.obj = sticker(obj, int(spec.get("object_size", 340)))
            obj_h = self.obj.h if self.obj else 0
        self.lines = _Lines(spec.get("lines") or [], th, int(spec.get("size", 150)), 80, self.w - 30)
        y = self.top_img.h * 0.5 if self.top_img else 0
        self.y_obj = y
        y += obj_h - 16
        self.ys = []
        for h in self.lines.heights():
            self.ys.append(y)
            y += h
        self.h = int(y + 16)
        self.crack_t = self.at(spec.get("crack_at"), 0.9) if self.book else 1e9
        self.when = _line_times(self, spec.get("at"), len(self.lines.items), [0.35 + 0.45 * i for i in range(len(self.lines.items))])
        self.sfx_events = [("pop", 0.05)] + ([("crack", self.crack_t)] if self.book else []) + [("pop", w) for w in self.when]

    def shake(self, t):
        p = t - self.crack_t
        if p < 0 or p > 0.38:
            return 0.0, 0.0
        amp = 18 * (1 - p / 0.38) ** 2
        return amp * math.sin(p * 2 * math.pi * 21), amp * 0.6 * math.cos(p * 2 * math.pi * 16)

    def draw(self, t):
        c = Canvas(self.w, self.h)
        bob = 5 * math.sin(2 * math.pi * 0.55 * t)
        if self.book is not None:
            b = self.book
            x0, y0 = (self.w - b.w) / 2, self.y_obj + bob
            p = t - self.crack_t
            if p < 0:  # 첫 화면(표지)부터 보이도록 거의 다 커진 상태로 시작
                s, a = pop(t + 0.2, 0.32, 0.75)
                c.put(b.whole, x0, y0, alpha=a, scale=s)
            elif p < 0.16:
                c.put(b.whole, x0, y0)
                c.put(b.crack(p / 0.16), x0, y0)
            else:
                u = p - 0.16
                q = ease_out_cubic(u / 0.45)
                for half, sign in ((b.left, -1), (b.right, 1)):
                    img = half.rotated(sign * 8 * q) if q > 0.01 else half
                    c.put(img, x0 + (b.w - img.w) / 2 + sign * 54 * q, y0 + (b.h - img.h) / 2 + 24 * q)
                if u < 0.9:
                    for shard, px, py, vx, vy, spin in b.shards:
                        img = shard.rotated(round(spin * u / 15) * 15)
                        c.put(img, x0 + px + vx * u - img.w / 2, y0 + py + vy * u + 1100 * u * u - img.h / 2, alpha=1 - clamp01((u - 0.35) / 0.4))
        elif self.obj is not None:
            s, a = pop(t - 0.02, 0.32, 0.6)
            c.put(self.obj, (self.w - self.obj.w) / 2, self.y_obj + bob, alpha=a, scale=s)
        if self.top_img is not None:
            s, a = pop(t - 0.04, 0.24, 1.7)  # 크게 '쾅' 찍히며 제자리로
            c.put(self.top_img, 18, 0, alpha=a, scale=s)
        for i, (y, w) in enumerate(zip(self.ys, self.when)):
            if t < w:
                continue
            kind = self.lines.items[i][0]
            s, a = pop(t - w, 0.28, 0.6 if kind == "hero" else 0.85)
            img = self.lines.image(i, t - w)
            c.put(img, (self.w - img.w) / 2, y, alpha=a, scale=s)
        return c.img()


# ---------- 꺾은선 그래프 ----------
class ChartCard(Card):
    """points: [{"x": "2022.6", "v": 2860, "label": "2,860만"}, ...], pos(0~1 가로 위치), range([최소, 최대]),
    badge("1년 새\\n-63만 명"), badge_at, source(출처). 선은 at부터 draw초 동안 그려진다."""

    def __init__(self, spec, th, timing):
        super().__init__(spec, th, timing)
        pts = spec.get("points") or []
        if len(pts) < 2:
            raise EditorError("chart 카드에는 points가 2개 이상 필요합니다.")
        self.pts = pts
        self.w = CARD_W
        self.head = self.header(spec.get("title", ""), spec.get("icon", "chart"), size=48, sticker_name=spec.get("sticker")) if spec.get("title") else None
        self.top = 140 if self.head else 40
        x0, y0 = 100, self.top + 90
        x1, y1 = self.w - 120, y0 + 300
        self.plot = (x0, y0, x1, y1)
        vals = [float(p["v"]) for p in pts]
        lo, hi = spec.get("range") or (min(vals) - (max(vals) - min(vals)) * 0.25, max(vals))
        span = (hi - lo) or 1.0
        pos = spec.get("pos") or [i / (len(pts) - 1) for i in range(len(pts))]
        self.xy = [(x0 + (x1 - x0) * float(f), y1 - (y1 - y0) * (float(v) - lo) / span) for f, v in zip(pos, vals)]
        self.color = col(th, spec.get("color"), "pen")
        self.xlabels = [text(str(p.get("x", "")), 30, th, fill=th["muted"], weight="bold") for p in pts]
        self.vlabels = [text(p["label"], 38, th, weight="black") if p.get("label") else None for p in pts]
        self.notes = [hand(p["note"], 56, th["pen"]) if p.get("note") else None for p in pts]
        self.foot = text(spec["source"], 28, th, fill=th["muted"], weight="bold", max_w=self.w - 100) if spec.get("source") else None
        self.h = int(y1 + 70 + (self.foot.h + 6 if self.foot else 0))
        bg = Canvas(self.w, self.h)
        bg.put(self.box(self.w, self.h), 0, 0)
        if self.head:
            bg.put(self.head, 50, 70 - self.head.h / 2)
        for k in range(4):  # 옅은 가로 눈금
            gy = y0 + (y1 - y0) * k / 3
            bg.put(rounded(x1 - x0 + 40, 3, 1, th["track"]), x0 - 20, gy)
        for (px, _), lab in zip(self.xy, self.xlabels):
            bg.put(lab, px - lab.w / 2, y1 + 12)
        if self.foot:
            bg.put(self.foot, 40, self.h - self.foot.h - 14)
        self.static = bg.img()
        self.t0 = self.at(spec.get("at"), 0.3)
        self.dur = float(spec.get("draw", 1.1))
        seglen = [math.dist(self.xy[i], self.xy[i + 1]) for i in range(len(self.xy) - 1)]
        total = sum(seglen) or 1.0
        acc = np.concatenate(([0], np.cumsum(seglen))) / total
        self.reach = [self.t0 + self.dur * float(a) for a in acc]  # 선이 각 점에 닿는 시각
        self.badge_t = self.at(spec.get("badge_at"), self.reach[-1] + 0.25)
        self.badge = None
        if spec.get("badge"):
            lines = str(spec["badge"]).split("\n")
            head = text(lines[0], 30, th, weight="extrabold") if len(lines) > 1 else None
            big = text(lines[-1], 58, th, weight="black", fill=th["pen"], max_w=320)
            bw = max(big.w, head.w if head else 0) + 40
            bh = big.h + (head.h if head else 0) + 30
            note = Canvas(bw + STICKY_PAD * 2, bh + STICKY_PAD * 2)
            note.put(Img.from_pil(sticky_pil(bw, bh, _sticky_color(th, spec.get("badge_color", "pink")), seed=5)), 0, 0)
            yy = STICKY_PAD + 14
            if head:
                note.put(head, STICKY_PAD + (bw - head.w) / 2, yy)
                yy += head.h - 10
            note.put(big, STICKY_PAD + (bw - big.w) / 2, yy)
            self.badge = note.img().rotated(4)
        # 배지는 그래프가 비어 있는 쪽에: 내려가는 그래프면 오른쪽 위(마지막 숫자는 점 아래로), 올라가면 왼쪽 위
        self.falling = float(pts[-1]["v"]) < float(pts[0]["v"])
        pos = spec.get("badge_pos") or ("tr" if self.falling else "tl")
        if self.badge is not None:  # 위쪽이면 카드 제목 옆 모서리에 붙인다 (그래프 선을 가리지 않게)
            bx = x0 - 10 if pos in ("bl", "tl") else self.w - self.badge.w - 10
            by = y1 - self.badge.h + 6 if pos in ("bl", "br") else self.top - 34
            self.badge_xy = (bx, by)
        self.dot = disc(26, self.color)
        self.dot_in = disc(12, th["card"])
        self._anim = {}
        self.sfx_events = [("pop", r) for r in self.reach[1:]] + ([("pop", self.badge_t)] if self.badge else [])

    def _line(self, p: float) -> Img:
        p = round(clamp01(p), 2)
        if p not in self._anim:
            x0, y0, x1, y1 = self.plot
            w, h = self.w, self.h
            im = Image.new("RGBA", (w * SS, h * SS), (0, 0, 0, 0))
            d = ImageDraw.Draw(im, "RGBA")
            pts = self.xy
            seglen = [math.dist(pts[i], pts[i + 1]) for i in range(len(pts) - 1)]
            left = p * sum(seglen)
            path = [pts[0]]
            for i, L in enumerate(seglen):
                if left >= L:
                    path.append(pts[i + 1])
                    left -= L
                else:
                    f = left / L if L else 0
                    path.append((pts[i][0] + (pts[i + 1][0] - pts[i][0]) * f, pts[i][1] + (pts[i + 1][1] - pts[i][1]) * f))
                    break
            if len(path) > 1:
                area = [(x * SS, y * SS) for x, y in path] + [(path[-1][0] * SS, y1 * SS), (path[0][0] * SS, y1 * SS)]
                d.polygon(area, fill=tuple(self.color) + (34,))
                d.line([(x * SS, y * SS) for x, y in path], fill=tuple(self.color) + (255,), width=9 * SS, joint="curve")
            self._anim[p] = Img.from_pil(im.resize((w, h), Image.LANCZOS))
        return self._anim[p]

    def state(self, t):
        q = lambda x, d=0.4: round(min(max(x, -0.01), d), 2)  # noqa: E731
        return (round(clamp01((t - self.t0) / self.dur), 2),) + tuple(q(t - r) for r in self.reach) + (q(t - self.badge_t, 0.6),)

    def draw(self, t):
        c = Canvas(self.w, self.h)
        c.put(self.static, 0, 0)
        if t >= self.t0:
            c.put(self._line((t - self.t0) / self.dur), 0, 0)
        last_box = None
        for i, ((px, py), r) in enumerate(zip(self.xy, self.reach)):
            if t < r:
                continue
            s, a = pop(t - r, 0.22, 0.4)
            c.put(self.dot, px - 13, py - 13, alpha=a, scale=s)
            c.put(self.dot_in, px - 6, py - 6, alpha=a)
            lab = self.vlabels[i]
            box = (px - 14, py - 14, px + 14, py + 14)
            if lab is not None:
                ls, la = pop(t - r - 0.05, 0.25, 0.7)
                below = self.falling and i == len(self.xy) - 1
                ly = py + 4 if below else py - lab.h - 12
                lx = min(max(px - lab.w / 2, 20), self.w - lab.w - 20)
                c.put(lab, lx, ly, alpha=la, scale=ls)
                box = (min(lx + 10, px - 14), min(ly + 8, py - 14), max(lx + lab.w - 10, px + 14), max(py + 14, ly + lab.h - 8))
            if i == len(self.xy) - 1:
                last_box = box
            if self.notes[i] is not None:
                c.put(self.notes[i], px + 24, py - 20, alpha=clamp01((t - r - 0.15) / 0.25))
        if self.badge is not None and t >= self.badge_t:
            if last_box is not None:  # 마지막 점(+숫자)에 빨간 펜 동그라미
                bx0, by0, bx1, by1 = last_box
                ring = pen_circle(int(bx1 - bx0 + 70), int(by1 - by0 + 56), self.th["pen"], 7, clamp01((t - self.badge_t) / 0.35), seed=7)
                c.put(ring, (bx0 + bx1) / 2 - ring.w / 2, (by0 + by1) / 2 - ring.h / 2)
            s, a = slap(t - self.badge_t - 0.15)
            c.put(self.badge, self.badge_xy[0], self.badge_xy[1], alpha=a, scale=s)
        return c.img()


# ---------- 합계 막대 ----------
class StackCard(Card):
    """parts: [{"label": "무주택 기간", "value": 32}, ..., {"label": "통장\\n가입 기간", "value": 17, "focus": true, "at": "17점"}],
    total, unit("점"), note(손글씨 메모)·note_at, sub(아래 작은 설명)."""

    BAR_H = 150

    def __init__(self, spec, th, timing):
        super().__init__(spec, th, timing)
        parts = spec.get("parts") or []
        if not parts:
            raise EditorError("stack 카드에는 parts가 필요합니다.")
        self.parts = parts
        self.w = CARD_W
        self.head = self.header(spec.get("title", ""), spec.get("icon", "chart"), size=50, sticker_name=spec.get("sticker")) if spec.get("title") else None
        self.top = 150 if self.head else 40
        total = float(spec.get("total") or sum(float(p["value"]) for p in parts))
        unit = spec.get("unit", "점")
        x0, x1 = 56, self.w - 56
        self.bar_y = self.top + 10
        pal = [_sticky_color(th, k) for k in ("blue", "mint", "pink", "yellow")] if is_note(th) else [th["track"], th["cell_border"], th["dim"]]
        self.segs = []
        x = float(x0)
        lab_h = 0
        for i, p in enumerate(parts):
            wdt = (x1 - x0) * float(p["value"]) / total
            focus = bool(p.get("focus"))
            color = col(th, p["color"]) if p.get("color") else (th["accent"] if focus else pal[i % len(pal)])
            seg = rounded(max(20, int(wdt) - 8), self.BAR_H, 16, color)
            val = text(p.get("value_text", f"{float(p['value']):g}{unit}"), 56, th, weight="black",
                       fill=th["on_accent"] if focus else th["text"], max_w=max(40, int(wdt) - 20))
            lab = text(p.get("label", ""), 34, th, weight="extrabold", fill=th["text"] if focus else th["muted"], max_w=max(120, int(wdt) + 40))
            lab_h = max(lab_h, lab.h)
            self.segs.append({"x": x, "w": wdt, "img": seg, "val": val, "lab": lab, "focus": focus})
            x += wdt
        self.lab_y = self.bar_y + self.BAR_H + (26 if any(p.get("focus") for p in parts) else 6)
        y = self.lab_y + lab_h + 6
        self.note = hand(spec["note"], 78, th["pen"], max_w=560) if spec.get("note") else None
        self.note_y = y
        if self.note:
            y += self.note.h
        self.sub = text(spec["sub"], 32, th, fill=th["muted"], weight="bold", max_w=self.w - 100) if spec.get("sub") else None
        self.sub_y = y + 4
        if self.sub:
            y += self.sub.h + 10
        self.h = int(y + 30)
        self.bg = self.box(self.w, self.h)
        defaults = self.spread(len(parts), 0.3, 0.55)
        self.when = [self.at(p.get("at"), d) for p, d in zip(parts, defaults)]
        self.note_t = self.at(spec.get("note_at"), max(self.when) + 0.6)
        self.focus = next((s for s in self.segs if s["focus"]), None)
        self.sfx_events = [("pop", w) for w in self.when] + ([("ding", self.note_t)] if self.note or self.focus else [])

    def state(self, t):
        q = lambda x, d=0.35: round(min(max(x, -0.01), d), 2)  # noqa: E731
        return tuple(q(t - w) for w in self.when) + (q(t - self.note_t, 0.75),)

    def draw(self, t):
        c = Canvas(self.w, self.h)
        c.put(self.bg, 0, 0)
        if self.head:
            c.put(self.head, 50, 72 - self.head.h / 2)
        for s, w in zip(self.segs, self.when):
            p = t - w
            if p < 0:
                continue
            q = ease_out_cubic(p / 0.3)
            img = s["img"]
            cut = max(1, int(img.w * q))
            c.put(Img(img.pm[:, :cut], img.a[:, :cut]), s["x"] + 4, self.bar_y)
            v = s["val"]
            vs, va = pop(p - 0.15, 0.22, 0.6)
            c.put(v, s["x"] + (s["w"] - v.w) / 2, self.bar_y + (self.BAR_H - v.h) / 2, alpha=va, scale=vs)
            lab = s["lab"]
            lx = min(max(s["x"] + (s["w"] - lab.w) / 2, 20), self.w - lab.w - 20)
            c.put(lab, lx, self.lab_y, alpha=clamp01((p - 0.1) / 0.25))
        p = t - self.note_t
        if p >= 0:
            if self.focus is not None:
                f = self.focus
                ring = pen_circle(int(f["w"] + 56), self.BAR_H + 44, self.th["pen"], 8, clamp01(p / 0.35), seed=11)
                c.put(ring, f["x"] + f["w"] / 2 - ring.w / 2, self.bar_y + self.BAR_H / 2 - ring.h / 2)
            if self.note is not None:
                c.put(self.note, self.w - self.note.w - 40, self.note_y, alpha=clamp01((p - 0.3) / 0.25))
        if self.sub:
            c.put(self.sub, 50, self.sub_y)
        return c.img()


# ---------- 은행 앱 화면 ----------
class PhoneCard(Card):
    """screen(화면 제목), rows([{k, v}]), field(금액 칸 이름), from → to(change_at에 바뀜), button·tap_at, toast, hint(작은 안내),
    note(옆 포스트잇 손글씨)·note_at. 특정 은행이 아니라 일반 앱 모양."""

    PW, PH = 540, 800

    def __init__(self, spec, th, timing):
        super().__init__(spec, th, timing)
        self.w = CARD_W
        self.h = self.PH + 40
        ink, white, gray = (24, 24, 28), (252, 252, 253), (128, 128, 136)
        px, py = 24, 14
        sx, sy, sw, sh = px + 16, py + 16, self.PW - 32, self.PH - 32
        self.screen = (sx, sy, sw, sh)
        c = Canvas(self.w, self.h)
        shadow, pad = soft_shadow(self.PW, self.PH, r=70, blur=18, alpha=0.32)
        c.put(shadow, px - pad, py - pad + 16)
        c.put(rounded(self.PW, self.PH, 70, ink), px, py)
        c.put(rounded(sw, sh, 56, white), sx, sy)
        c.put(rounded(126, 36, 18, ink), sx + (sw - 126) / 2, sy + 14)
        c.put(text("9:41", 26, th, weight="extrabold", fill=ink), sx + 40, sy + 8)
        c.put(rounded(44, 22, 6, white, border=ink, bw=3), sx + sw - 92, sy + 22)
        c.put(rounded(30, 12, 3, ink), sx + sw - 85, sy + 27)
        c.put(_chevron(34, ink), sx + 30, sy + 80)
        title = text(spec.get("screen", "자동이체 변경"), 36, th, weight="black", fill=ink, max_w=sw - 140)
        c.put(title, sx + (sw - title.w) / 2, sy + 76)
        y = sy + 160
        for row in spec.get("rows") or []:
            k = text(row.get("k", ""), 30, th, weight="bold", fill=gray)
            v = text(row.get("v", ""), 32, th, weight="extrabold", fill=ink, max_w=sw - k.w - 90)
            c.put(k, sx + 30, y)
            c.put(v, sx + sw - v.w - 30, y)
            c.put(rounded(sw - 60, 2, 1, (232, 232, 236)), sx + 30, y + k.h - 4)
            y += k.h + 8
        lab = text(spec.get("field", "월 납입 금액"), 30, th, weight="bold", fill=gray)
        c.put(lab, sx + 30, y + 8)
        self.field = (sx + 30, y + lab.h + 4, sw - 60, 130)
        fx, fy, fw, fh = self.field
        c.put(rounded(fw, fh, 24, (243, 243, 247), border=(222, 222, 230), bw=2), fx, fy)
        self.hint = text(spec["hint"], 28, th, weight="bold", fill=gray, max_w=fw) if spec.get("hint") else None
        if self.hint:
            c.put(self.hint, fx + 6, fy + fh + 10)
        bh = 104
        self.btn = (sx + 30, sy + sh - bh - 44, sw - 60, bh)
        bx, by, bw, _ = self.btn
        self.btn_img = rounded(bw, bh, 26, th["accent"])
        self.btn_down = rounded(bw, bh, 26, tuple(int(v * 0.82) for v in th["accent"]))
        self.btn_txt = text(spec.get("button", "변경하기"), 40, th, weight="black", fill=(255, 255, 255))
        self.static = c.img()
        self.from_img = text(spec.get("from", ""), 62, th, weight="black", fill=ink)
        self.to_img = text(spec.get("to", ""), 62, th, weight="black", fill=th["accent"] if not is_note(th) else (16, 120, 104))
        self.strike = {}
        self.toast = None
        if spec.get("toast"):
            tt = text(spec["toast"], 34, th, weight="extrabold", fill=(255, 255, 255))
            tc = Canvas(tt.w + 110, tt.h + 34)
            tc.put(rounded(tt.w + 110, tt.h + 34, (tt.h + 34) / 2, (40, 40, 46)), 0, 0)
            tc.put(icon("check", 40, th, color=(16, 170, 140)), 22, (tt.h + 34 - 40) / 2)
            tc.put(tt, 80, 17)
            self.toast = tc.img()
        self.memo = None
        if spec.get("note"):
            words = hand(spec["note"], 70, th["text"], max_w=300)
            mw, mh = 330, max(230, words.h + 60)
            m = Canvas(mw + STICKY_PAD * 2, mh + STICKY_PAD * 2)
            m.put(Img.from_pil(sticky_pil(mw, mh, _sticky_color(th, spec.get("note_color", "yellow")), seed=9)), 0, 0)
            m.put(words, STICKY_PAD + (mw - words.w) / 2, STICKY_PAD + (mh - words.h) / 2 + 6)
            self.memo = m.img().rotated(5)
        self.change_t = self.at(spec.get("change_at"), 0.8)
        self.tap_t = self.at(spec.get("tap_at"), self.change_t + 0.7)
        self.toast_t = self.tap_t + 0.3
        self.note_t = self.at(spec.get("note_at"), self.toast_t + 0.4)
        self.sfx_events = [("pop", self.change_t), ("tap", self.tap_t), ("ding", self.toast_t)] + ([("pop", self.note_t)] if self.memo else [])

    def state(self, t):
        q = lambda x, d=0.5: round(min(max(x, -0.01), d), 2)  # noqa: E731
        return (q(t - self.change_t, 0.7), q(t - self.tap_t, 0.45), q(t - self.toast_t, 0.3), q(t - self.note_t, 0.4))

    def draw(self, t):
        c = Canvas(self.w, self.h)
        c.put(self.static, 0, 0)
        fx, fy, fw, fh = self.field
        p = t - self.change_t
        old = self.from_img
        if p < 0:
            c.put(old, fx + 26, fy + (fh - old.h) / 2)
        else:  # 옛 금액에 빨간 줄 → 작아지며 위로, 새 금액 등장
            q = ease_out_cubic(clamp01((p - 0.2) / 0.3))
            small = old.scaled(1 - 0.45 * q)
            c.put(small, fx + 26, fy + (fh - old.h) / 2 - 34 * q, alpha=1 - 0.45 * q)
            key = round(clamp01(p / 0.2), 2)
            if key not in self.strike:
                self.strike[key] = pen_line(old.w - 10, self.th["pen"], 7, key, slope=-0.04)
            st = self.strike[key]
            c.put(st, fx + 30, fy + (fh - old.h) / 2 + old.h / 2 - st.h / 2 - 34 * q, alpha=1 - 0.3 * q)
            if p >= 0.3:
                s, a = pop(p - 0.3, 0.28, 0.6)
                new = self.to_img
                c.put(new, fx + fw - new.w - 26, fy + (fh - new.h) / 2 + 12, alpha=a, scale=s)
        bx, by, bw, bh = self.btn
        pressed = 0 <= t - self.tap_t < 0.18
        c.put(self.btn_down if pressed else self.btn_img, bx, by)
        c.put(self.btn_txt, bx + (bw - self.btn_txt.w) / 2, by + (bh - self.btn_txt.h) / 2)
        u = t - self.tap_t
        if 0 <= u < 0.45:  # 톡 누른 자리에 퍼지는 원
            r = int(40 + 220 * ease_out_cubic(u / 0.45))
            c.put(disc(r * 2, (255, 255, 255)), bx + bw * 0.62 - r, by + bh / 2 - r, alpha=0.45 * (1 - u / 0.45))
            c.put(disc(56, (30, 30, 30)), bx + bw * 0.62 - 28, by + bh / 2 - 28, alpha=0.25 * (1 - u / 0.45))
        if self.toast is not None and t >= self.toast_t:
            q = ease_out_back(clamp01((t - self.toast_t) / 0.3), 1.4)
            sx, sy, sw, sh = self.screen
            c.put(self.toast, sx + (sw - self.toast.w) / 2, sy + 150 + (1 - q) * 40, alpha=clamp01((t - self.toast_t) / 0.12))
        if self.memo is not None and t >= self.note_t:
            s, a = slap(t - self.note_t)
            c.put(self.memo, self.w - self.memo.w + 16, 150, alpha=a, scale=s)
        return c.img()


# ---------- 포스트잇 메모 ----------
class NotesCard(Card):
    """notes: [{"head": "소득공제", "big": "최대 120만 원", "small": "무주택 세대주\\n총급여 7천만 원 이하", "color": "yellow",
    "sticker": "money", "at": "120만"}, ...] (2~3장). title이 있으면 위에 큰 글씨."""

    def __init__(self, spec, th, timing):
        super().__init__(spec, th, timing)
        notes = spec.get("notes") or []
        if not notes:
            raise EditorError("notes 카드에는 notes가 필요합니다.")
        n = len(notes)
        self.w = 960
        self.title = text(spec["title"], 66, th, max_w=self.w - 40) if spec.get("title") else None
        gap = 18
        cw = int((self.w - gap * (n - 1)) / n) - STICKY_PAD * 2
        ch = int(spec.get("note_h", 440))
        self.notes = []
        tilts = (-3.0, 2.5, -2.0)
        for i, nt in enumerate(notes):
            c = Canvas(cw + STICKY_PAD * 2, ch + STICKY_PAD * 2)
            c.put(Img.from_pil(sticky_pil(cw, ch, _sticky_color(th, nt.get("color"), i), seed=20 + i)), 0, 0)
            x, y = STICKY_PAD + 30, STICKY_PAD + 34
            st = sticker(nt.get("sticker"), 118, outline=6, shadow=0.15)
            if nt.get("head"):
                head = text(nt["head"], 46, th, weight="extrabold", max_w=cw - 50 - (st.w - 40 if st else 0), align="left")
                c.put(head, x - 14, y)
                y += head.h + 4
            if nt.get("big"):
                big = text(nt["big"] if "[[" in nt["big"] else f"[[{nt['big']}]]", 84, th, weight="black", mark="highlight" if is_note(th) else "color", max_w=cw - 30, align="left")
                c.put(big, x - 16, y)
            if nt.get("small"):
                small = text(nt["small"], 36, th, weight="bold", fill=th["body"], max_w=cw - 40, align="left")
                c.put(small, x - 14, STICKY_PAD + ch - small.h - 18)
            if st is not None:
                c.put(st, cw + STICKY_PAD * 2 - st.w + 14, -6)
            self.notes.append(c.img().rotated(tilts[i % len(tilts)]))
        self.top = self.title.h + 6 if self.title else 0
        self.h = int(self.top + max(img.h for img in self.notes) + 10)
        self.xs = [i * self.w / n for i in range(n)]
        defaults = self.spread(n, 0.3, 0.6)
        self.when = [self.at(nt.get("at"), d) for nt, d in zip(notes, defaults)]
        self.title_t = self.at(spec.get("title_at"), 0.05)
        self.sfx_events = [("pop", w) for w in self.when]

    def state(self, t):
        return (round(min(max(t - self.title_t, -0.01), 0.4), 2),) + tuple(round(min(max(t - w, -0.01), 0.35), 2) for w in self.when)

    def draw(self, t):
        c = Canvas(self.w, self.h)
        if self.title:
            s, a = pop(t - self.title_t, 0.25, 0.85)
            c.put(self.title, (self.w - self.title.w) / 2, 0, alpha=a, scale=s)
        slot = self.w / len(self.notes)
        for img, x, w in zip(self.notes, self.xs, self.when):
            if t < w:
                continue
            s, a = slap(t - w)
            c.put(img, x + (slot - img.w) / 2, self.top, alpha=a, scale=s)
        return c.img()


def _chevron(size: int, color: tuple) -> Img:
    im = _big(size, size)
    S = size * SS
    ImageDraw.Draw(im).line([(0.66 * S, 0.12 * S), (0.3 * S, 0.5 * S), (0.66 * S, 0.88 * S)], fill=tuple(color) + (255,), width=int(0.13 * S), joint="curve")
    return _small(im, size, size)


# ---------- 마무리 (저장 + 투표) ----------
def _bookmark(size: int, color: tuple, filled: bool) -> Img:
    im = _big(size, size)
    d = ImageDraw.Draw(im)
    S = size * SS
    pts = [(0.26 * S, 0.14 * S), (0.74 * S, 0.14 * S), (0.74 * S, 0.88 * S), (0.5 * S, 0.68 * S), (0.26 * S, 0.88 * S)]
    if filled:
        d.polygon(pts, fill=tuple(color) + (255,))
    d.line(pts + [pts[0]], fill=tuple(color) + (255,), width=int(0.07 * S), joint="curve")
    return _small(im, size, size)


class CtaCard(Card):
    """lines(큰 글씨) + 저장 버튼(save_at에 톡 → 채워짐) + question(손글씨) + options(투표 버튼 두 개)·options_at."""

    def __init__(self, spec, th, timing):
        super().__init__(spec, th, timing)
        self.w = CARD_W
        self.lines = _Lines(spec.get("lines") or [], th, int(spec.get("size", 130)), 72, self.w - 40)
        y = 0
        self.ys = []
        for h in self.lines.heights():
            self.ys.append(y)
            y += h
        self.btn_d = 190
        self.btn_y = y + 20
        btn = Canvas(self.btn_d + 40, self.btn_d + 40)
        shadow, pad = soft_shadow(self.btn_d, self.btn_d, r=self.btn_d / 2, blur=12, alpha=0.25)
        btn.put(shadow, 20 - pad, 20 - pad + 8)
        btn.put(disc(self.btn_d, (255, 255, 255), ring=th["text"], rw=5), 20, 20)
        self.btn = btn.img()
        self.mark_off = _bookmark(110, th["text"], False)
        self.mark_on = _bookmark(110, th["accent"], True)
        self.save_lab = text(spec.get("save_label", "저장"), 40, th, weight="black")
        y = self.btn_y + self.btn_d + 40 + self.save_lab.h
        self.question = hand(spec["question"], 74, th["pen"]) if spec.get("question") else None
        self.q_y = y
        if self.question:
            y += self.question.h - 6
        self.opts = []
        for i, o in enumerate(spec.get("options") or []):
            tx = text(o, 46, th, weight="black")
            ow, oh = max(300, tx.w + 80), 116
            oc = Canvas(ow, oh)
            oc.put(rounded(ow, oh, 30, th["card"], border=th["text"], bw=5), 0, 0)
            oc.put(tx, (ow - tx.w) / 2, (oh - tx.h) / 2)
            self.opts.append(oc.img())
        self.opt_y = y + 6
        if self.opts:
            y += 116 + 20
        self.h = int(y + 10)
        self.when = _line_times(self, spec.get("at"), len(self.lines.items), [0.05 + 0.35 * i for i in range(len(self.lines.items))])
        self.btn_t = self.at(spec.get("button_at"), max(self.when or [0]) + 0.3)
        self.save_t = self.at(spec.get("save_at"), self.btn_t + 0.6)
        self.q_t = self.at(spec.get("question_at"), self.save_t + 0.6)
        self.opt_t = self.at(spec.get("options_at"), self.q_t + 0.3)
        self.sfx_events = [("pop", w) for w in self.when] + [("tap", self.save_t)] + ([("pop", self.opt_t)] if self.opts else [])

    def state(self, t):
        q = lambda x, d=0.45: round(min(max(x, -0.01), d), 2)  # noqa: E731
        return tuple(q(t - w) for w in self.when) + (q(t - self.btn_t), q(t - self.save_t, 0.5), q(t - self.q_t), q(t - self.opt_t))

    def draw(self, t):
        c = Canvas(self.w, self.h)
        for i, (y, w) in enumerate(zip(self.ys, self.when)):
            if t < w:
                continue
            kind = self.lines.items[i][0]
            s, a = pop(t - w, 0.28, 0.6 if kind == "hero" else 0.85)
            img = self.lines.image(i, t - w)
            c.put(img, (self.w - img.w) / 2, y, alpha=a, scale=s)
        if t >= self.btn_t:
            s, a = pop(t - self.btn_t, 0.3, 0.5)
            cx = self.w / 2
            u = t - self.save_t
            press = 0.92 if 0 <= u < 0.12 else 1.0
            c.put(self.btn, cx - self.btn.w / 2, self.btn_y, alpha=a, scale=s * press)
            mark = self.mark_on if u >= 0.08 else self.mark_off
            ms = s * press * (1.18 - 0.18 * ease_out_back(clamp01((u - 0.08) / 0.3))) if u >= 0.08 else s * press
            c.put(mark, cx - mark.w / 2, self.btn_y + 20 + (self.btn_d - mark.h) / 2, alpha=a, scale=ms)
            if 0 <= u < 0.5:
                r = int(self.btn_d / 2 + 140 * ease_out_cubic(u / 0.5))
                c.put(disc(r * 2, None, ring=self.th["accent"], rw=6), cx - r, self.btn_y + 20 + self.btn_d / 2 - r, alpha=0.8 * (1 - u / 0.5))
            c.put(self.save_lab, cx - self.save_lab.w / 2, self.btn_y + self.btn_d + 34, alpha=a)
        if self.question is not None and t >= self.q_t:
            s, a = pop(t - self.q_t, 0.25, 0.8)
            c.put(self.question, (self.w - self.question.w) / 2, self.q_y, alpha=a, scale=s)
        if self.opts and t >= self.opt_t:
            total = sum(o.w for o in self.opts) + 40 * (len(self.opts) - 1)
            x = (self.w - total) / 2
            for k, o in enumerate(self.opts):
                s, a = pop(t - self.opt_t - 0.12 * k, 0.28, 0.6)
                c.put(o, x, self.opt_y, alpha=a, scale=s)
                x += o.w + 40
        return c.img()


NOTE_CARDS = {"hook": HookCard, "chart": ChartCard, "stack": StackCard, "phone": PhoneCard, "notes": NotesCard, "cta": CtaCard}


# ---------- 장면 위에 붙이는 스티커 ----------
class Stickers:
    """장면 위 다꾸 스티커: [{"name": "fire", "pos": [0.94, 0.02], "size": 150, "at": "단어", "angle": 8}] (pos는 카드 기준 비율)."""

    def __init__(self, items: list, th: dict, timing):
        self.items = []
        self.sfx_events = []
        for i, it in enumerate(items or []):
            it = it if isinstance(it, dict) else {"name": str(it)}
            img = sticker(it.get("name"), int(it.get("size", 150)))
            if img is None:
                continue
            img = img.rotated(float(it.get("angle", (8, -10, 6)[i % 3])))
            when = timing.at(it.get("at"), 0.35 + 0.3 * i)
            self.items.append((img, tuple(it.get("pos", (0.94, 0.02))), when))
            self.sfx_events.append(("pop", when))

    def draw(self, canvas, box: tuple, t: float) -> None:
        x0, y0, w, h = box
        for img, (fx, fy), when in self.items:
            if t < when:
                continue
            s, a = slap(t - when)
            cx = min(max(x0 + fx * w, img.w / 2 + 8), 1080 - img.w / 2 - 8)
            cy = y0 + fy * h
            canvas.put(img, cx - img.w / 2, cy - img.h / 2, alpha=a, scale=s)
