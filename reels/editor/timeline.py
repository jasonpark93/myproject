"""남길 구간(세그먼트) 계산과 원본 시간 ↔ 완성본 시간 변환.

규칙
- 0.4초 이상 쉼은 잘라서 문장 사이 쉼을 gap(기본 0.15초)만 남긴다.
- 잘린 단어(군말·NG·대본 밖) 자리는 항상 자른다.
- 단어 사이 짧은 틈에 인식 안 된 소리(음…, 숨소리)가 따로 있으면 그것도 자른다.
- 단어 시간이 침묵을 덮고 있으면(인식 오차) 소리 에너지로 찾아 자른다.
- 컷 경계는 실제 소리 시작·끝에 맞추고 1/30초 격자에 맞춰 영상·음성이 같은 지점에서 잘리게 한다.
"""

from __future__ import annotations

import bisect
from dataclasses import dataclass

from . import FPS
from .analyze import CUT_PRIORITY


@dataclass
class Segment:
    a: float
    b: float
    cut_before: str = ""  # 이 구간 앞에서 무엇을 잘랐는지


def _grid(t: float) -> float:
    return round(t * FPS) / FPS


def _reason(words: list, lo: int, hi: int, gap: float, silence: float) -> str:
    reasons = {words[k].get("cut") for k in range(lo, hi) if words[k].get("cut")}
    for r in CUT_PRIORITY:
        if r in reasons:
            return r
    return "silence" if gap >= silence else "noise"


def _in_ranges(t0: float, t1: float, ranges: list) -> bool:
    return any(a < t1 and t0 < b for a, b in ranges)


def build(words: list, env, settings: dict, duration: float, keep_ranges=(), drop_ranges=()) -> list:
    gap = float(settings["gap"])
    silence = float(settings["silence"])
    lead = float(settings.get("lead", 0.1))
    tail = float(settings.get("tail", 0.35))
    drop_ranges = [tuple(r) for r in drop_ranges]
    for w in words:  # 수동으로 지운 구간의 단어
        if not w.get("cut") and _in_ranges(w["s"], w["e"], drop_ranges):
            w["cut"] = "manual"
    kept = [i for i, w in enumerate(words) if not w.get("cut")]
    if not kept:
        return []

    # 1) 이어서 말한 덩어리(run) 나누기
    head = _reason(words, 0, kept[0], 99.0, silence) if kept[0] > 0 else "silence"
    runs, reasons = [[kept[0]]], [head]
    for prev, cur in zip(kept, kept[1:]):
        g = words[cur]["s"] - words[prev]["e"]
        split = None
        if cur != prev + 1:
            split = _reason(words, prev + 1, cur, g, silence)
        elif g >= silence:
            split = "silence"
        elif env is not None and g > 0.2:
            p_off = env.offset(words[prev]["e"], 0.1, 0.1) or words[prev]["e"]
            c_on = env.onset(words[cur]["s"], 0.1, 0.1) or words[cur]["s"]
            stray = [(s, e) for s, e in env.spans(words[prev]["e"], words[cur]["s"]) if s > p_off + 0.04 and e < c_on - 0.04 and e - s >= 0.1]
            if stray:
                split = "noise"
        if split:
            runs.append([cur])
            reasons.append(split)
        else:
            runs[-1].append(cur)

    # 2) 각 덩어리의 시작·끝을 실제 소리에 맞추고 여백을 붙인다
    segs = []
    for n, (run, why) in enumerate(zip(runs, reasons)):
        first, last = run[0], run[-1]
        start, end = words[first]["s"], words[last]["e"]
        lo_limit = words[first - 1]["e"] + 0.02 if first > 0 else 0.0
        hi_limit = words[last + 1]["s"] - 0.02 if last + 1 < len(words) else duration
        if env is not None:
            on = env.onset(start, 0.25, 0.15)
            if on is not None and on >= lo_limit and on < words[first]["e"]:
                start = on
            off = env.offset(end, 0.15, 0.3)
            if off is not None and off <= hi_limit and off > words[last]["s"] + 0.05:
                end = off
        if n == 0:
            a = max(0.0, max(lo_limit, start - lead))
        else:
            a = start - min(gap * 0.4, max(0.0, start - lo_limit))
        if n == len(runs) - 1:
            b = min(duration, min(hi_limit, end + tail) if last + 1 < len(words) else end + tail)
        else:
            b = end + min(gap * 0.6, max(0.0, hi_limit - end))
        segs.append(Segment(a, b, why))

    # 3) 단어 시간이 덮고 있는 긴 침묵도 자른다
    if env is not None:
        split_segs = []
        for seg in segs:
            pieces = [seg]
            for q0, q1 in env.quiet_spans(seg.a + 0.05, seg.b - 0.05, min_len=silence + 0.15):
                last_piece = pieces[-1]
                if q0 - last_piece.a < 0.15 or seg.b - q1 < 0.15:
                    continue
                pieces[-1] = Segment(last_piece.a, q0 + gap * 0.6, last_piece.cut_before)
                pieces.append(Segment(q1 - gap * 0.4, seg.b, "silence"))
            split_segs.extend(pieces)
        segs = split_segs

    # 4) 강제로 살린 구간 / 지운 구간 반영
    for a, b in keep_ranges:
        segs.append(Segment(float(a), float(b), "manual"))
    segs.sort(key=lambda s: s.a)
    merged = []
    for seg in segs:
        if merged and seg.a <= merged[-1].b + 0.01:
            merged[-1].b = max(merged[-1].b, seg.b)
        else:
            merged.append(Segment(seg.a, seg.b, seg.cut_before))
    out = []
    for seg in merged:
        pieces = [(seg.a, seg.b, seg.cut_before)]
        for da, db in drop_ranges:
            nxt = []
            for a, b, why in pieces:
                if db <= a or da >= b:
                    nxt.append((a, b, why))
                    continue
                if da > a:
                    nxt.append((a, da, why))
                if db < b:
                    nxt.append((db, b, "manual"))
            pieces = nxt
        out.extend(Segment(a, b, why) for a, b, why in pieces)

    # 5) 1/30초 격자에 맞춘다
    final = []
    for seg in out:
        a, b = _grid(max(0.0, seg.a)), _grid(min(duration, seg.b))
        if final and a <= final[-1].b + 1e-6:
            final[-1].b = max(final[-1].b, b)
            continue
        if b - a >= 2 / FPS - 1e-6:
            final.append(Segment(a, b, seg.cut_before))
    return final


class Timeline:
    """세그먼트를 이어 붙인 완성본 시간축."""

    def __init__(self, segments: list):
        self.segments = segments
        self.starts = []
        total = 0.0
        for seg in segments:
            self.starts.append(total)
            total += seg.b - seg.a
        self.duration = total

    def to_out(self, t: float) -> float:
        """원본 시간 → 완성본 시간. 잘린 곳이면 다음 구간의 시작으로 붙인다."""
        for seg, start in zip(self.segments, self.starts):
            if t < seg.a:
                return start
            if t <= seg.b:
                return start + (t - seg.a)
        return self.duration

    def to_src(self, t: float) -> float:
        k = max(0, bisect.bisect_right(self.starts, t) - 1)
        if not self.segments:
            return 0.0
        seg = self.segments[k]
        return seg.a + min(t - self.starts[k], seg.b - seg.a)

    def index_at(self, t: float) -> int:
        return max(0, bisect.bisect_right(self.starts, t + 1e-9) - 1)

    def joins(self) -> list:
        """잘린 지점(완성본 시간)."""
        return self.starts[1:]

    def cuts(self, words: list, source_duration: float) -> list:
        """잘린 부분 목록 (원본 시간 기준) — 보고서·복구용."""
        out = []
        prev_b = 0.0
        for k, seg in enumerate(self.segments):
            if seg.a - prev_b > 1e-3:
                inside = [w["w"] for w in words if w["s"] >= prev_b - 0.01 and w["e"] <= seg.a + 0.01]
                out.append({"src": [round(prev_b, 3), round(seg.a, 3)], "out": round(self.starts[k], 3), "reason": seg.cut_before or "silence", "text": " ".join(inside)})
            prev_b = seg.b
        if source_duration - prev_b > 1e-3:
            after = [k for k, w in enumerate(words) if w["s"] >= prev_b - 0.01]
            reason = _reason(words, after[0], after[-1] + 1, 99.0, 0.0) if after else "silence"
            inside = [words[k]["w"] for k in after]
            out.append({"src": [round(prev_b, 3), round(source_duration, 3)], "out": round(self.duration, 3), "reason": reason, "text": " ".join(inside)})
        return out
