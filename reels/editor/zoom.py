"""줌 계획: 문장 역할 → 펀치/슬로우/없음, 그리고 프레임마다의 확대 배율.

- 펀치 줌(기본 115%): 훅·숫자·반전·결론·CTA 문장 시작에 즉시, 문장 끝까지 유지.
- 슬로우 줌(100→108%): 설명 문장 동안 천천히.
- 같은 종류가 연달아 나오지 않게 바꾸고, 문장이 바뀌면 원래 크기로 돌아온다.
- 문장 경계 근처에 컷이 있으면 줌 전환을 그 컷 순간에 맞춘다(점프컷이 '카메라 전환'처럼 보이게).
- 문장 안에서 군말 등을 잘라 생긴 점프컷은 살짝(6%) 크기를 바꿔 튀는 느낌을 가린다.
"""

from __future__ import annotations

import numpy as np

from . import FPS

PUNCH_ROLES = {"hook", "number", "twist", "conclusion", "cta"}
PUNCH_PRIORITY = {"hook": 0, "cta": 1, "twist": 2, "conclusion": 3, "number": 4}
ZOOM_LABEL = {"punch": "펀치", "slow": "슬로우", "none": "없음"}


def resolve_types(sentences: list, cfg: dict) -> list:
    """sentences: [{'role','zoom','start','end'}] (완성본 시간) → ['punch'|'slow'|'none']."""
    if not cfg.get("enabled", True):
        return ["none"] * len(sentences)
    desired = []
    for s in sentences:
        if s.get("zoom") in ("punch", "slow", "none"):
            desired.append([s["zoom"], True])
        else:
            kind = "punch" if s.get("role") in PUNCH_ROLES else "slow"
            if s["end"] - s["start"] < 0.6 and s.get("role") != "hook":
                kind = "none"
            desired.append([kind, False])
    limit = cfg.get("max_punch")
    if limit is not None:
        auto = [i for i, (k, ex) in enumerate(desired) if k == "punch" and not ex]
        fixed = sum(1 for k, ex in desired if k == "punch" and ex)
        allowed = max(0, int(limit) - fixed)
        ranked = sorted(auto, key=lambda i: (PUNCH_PRIORITY.get(sentences[i].get("role"), 9), i))
        keep = set(ranked[:allowed])
        for i in auto:
            if i not in keep:
                desired[i][0] = "slow"
    out, prev = [], None
    for kind, explicit in desired:
        if not explicit and kind == prev and kind != "none":
            kind = "slow" if kind == "punch" else "none"
        out.append(kind)
        prev = kind
    return out


def boundaries(sentences: list, joins: list, duration: float) -> list:
    """문장 k가 화면을 차지하기 시작하는 시각 B_k. 문장 사이에 컷이 있으면 그 컷 순간."""
    out = [0.0]
    for prev, cur in zip(sentences, sentences[1:]):
        lo, hi = prev["end"] - 0.05, cur["start"] + 0.05
        inside = [j for j in joins if lo <= j <= hi]
        if inside:
            out.append(max(inside))
        else:
            gap = max(0.0, cur["start"] - prev["end"])
            out.append(max(prev["end"], cur["start"] - min(0.04, gap / 2)))
    out.append(duration)
    return out


def curve(sentences: list, kinds: list, bounds: list, joins: list, n_frames: int, cfg: dict):
    """프레임별 배율 배열과 보고서용 이벤트 목록."""
    z = np.ones(n_frames, np.float64)
    punch = float(cfg.get("punch", 1.15))
    slow = float(cfg.get("slow", 1.08))
    step = float(cfg.get("mask_step", 1.06))
    events = []
    t = np.arange(n_frames) / FPS
    for k, (s, kind) in enumerate(zip(sentences, kinds)):
        b0, b1 = bounds[k], bounds[k + 1]
        sel = (t >= b0 - 1e-9) & (t < b1 - 1e-9)
        if not sel.any():
            continue
        if kind == "punch":
            z[sel] = punch
        elif kind == "slow":
            ramp_end = max(b0 + 0.5, s["end"])
            z[sel] = 1.0 + (slow - 1.0) * np.clip((t[sel] - b0) / (ramp_end - b0), 0.0, 1.0)
        if kind != "none":
            events.append({"t": round(b0, 3), "kind": kind, "sentence": s.get("id"), "level": punch if kind == "punch" else slow})
        if cfg.get("mask_cuts", True) and step > 1.0:
            inner = [j for j in joins if b0 + 0.1 < j < b1 - 0.1]
            toggled = False
            for n, j in enumerate(inner):
                toggled = not toggled
                end = inner[n + 1] if n + 1 < len(inner) else b1
                part = (t >= j - 1e-9) & (t < end - 1e-9)
                if toggled:
                    z[part] = np.where(z[part] >= 1.1, z[part] / step, z[part] * step)
    return z, events


def shots(bounds: list, joins: list, duration: float) -> list:
    """화면 구도가 고정되는 구간들 (줌 전환·컷 사이)."""
    marks = sorted({0.0, duration, *[b for b in bounds if 0 < b < duration], *[j for j in joins if 0 < j < duration]})
    return [(a, b) for a, b in zip(marks, marks[1:]) if b - a > 1e-6]
