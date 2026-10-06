"""소리 분석(말소리 구간 찾기)과 음성 처리(음량 맞춤·믹스).

음성 인식(Whisper)은 '음', '어' 같은 군말을 자주 빼먹고 단어 시간도 0.1~0.2초씩 틀린다.
그래서 컷 위치는 글자가 아니라 실제 소리 에너지로 한 번 더 맞춘다.
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from pathlib import Path

import numpy as np

from . import SR
from .media import write_wav
from .util import EditorError, need, run

HOP = 0.01  # 10ms 단위로 분석


def _runs(mask: np.ndarray):
    """연속 구간 (값, 시작, 끝) 목록."""
    if mask.size == 0:
        return []
    change = np.flatnonzero(np.diff(mask.astype(np.int8))) + 1
    starts = np.concatenate(([0], change))
    ends = np.concatenate((change, [mask.size]))
    return [(bool(mask[s]), int(s), int(e)) for s, e in zip(starts, ends)]


def smooth_activity(active: np.ndarray, max_gap: int, min_len: int) -> np.ndarray:
    out = active.copy()
    runs = _runs(out)
    for i, (value, s, e) in enumerate(runs):  # 말소리 사이 아주 짧은 끊김은 이어 붙인다
        if not value and 0 < i < len(runs) - 1 and e - s < max_gap:
            out[s:e] = True
    for value, s, e in _runs(out):  # 너무 짧은 잡음은 지운다
        if value and e - s < min_len:
            out[s:e] = False
    return out


@dataclass
class Envelope:
    db: np.ndarray
    floor: float
    loud: float
    thr: float
    active: np.ndarray
    hop: float = HOP

    @classmethod
    def from_samples(cls, x: np.ndarray, sr: int = SR) -> "Envelope":
        hop = int(round(sr * HOP))
        win = hop * 2
        x = np.asarray(x, np.float32)
        if x.size < win:
            x = np.pad(x, (0, win - x.size))
        n = (x.size - win) // hop + 1
        window = np.hanning(win).astype(np.float32)
        freqs = np.fft.rfftfreq(win, 1 / sr)
        band = (freqs >= 100) & (freqs <= 5000)  # 사람 목소리 대역만 (저음 웅웅·고음 쉬익 제외)
        view = np.lib.stride_tricks.sliding_window_view(x, win)
        energy = np.empty(n, np.float64)
        for b in range(0, n, 4000):
            idx = np.arange(b, min(n, b + 4000)) * hop
            spec = np.fft.rfft(view[idx] * window, axis=1)
            energy[b : b + idx.size] = (np.abs(spec[:, band]) ** 2).sum(axis=1)
        db = (10 * np.log10(energy / (win * 2.0) + 1e-12)).astype(np.float32)
        floor = float(np.percentile(db, 10))
        loud = float(np.percentile(db, 95))
        thr = floor + max(10.0, 0.35 * (loud - floor))
        thr = min(thr, loud - 18.0) if loud - floor > 28 else thr
        active = smooth_activity(db > thr, max_gap=8, min_len=5)
        return cls(db=db, floor=floor, loud=loud, thr=float(thr), active=active)

    # ---------- 저장 ----------
    def save(self, path: Path) -> None:
        np.savez_compressed(path, db=self.db, active=self.active, meta=np.array([self.floor, self.loud, self.thr, self.hop]))

    @classmethod
    def load(cls, path: Path) -> "Envelope":
        data = np.load(path)
        floor, loud, thr, hop = (float(v) for v in data["meta"])
        return cls(db=data["db"], floor=floor, loud=loud, thr=thr, active=data["active"].astype(bool), hop=hop)

    # ---------- 조회 ----------
    @property
    def duration(self) -> float:
        return self.db.size * self.hop

    def idx(self, t: float) -> int:
        return int(min(max(round(t / self.hop), 0), self.db.size))

    def activity(self, t0: float, t1: float) -> float:
        i0, i1 = self.idx(t0), self.idx(t1)
        if i1 <= i0:
            return 0.0
        return float(self.active[i0:i1].mean())

    def onset(self, t: float, before: float = 0.25, after: float = 0.2):
        """t 근처에서 말소리가 시작되는 순간. 없으면 None."""
        i0, i1 = self.idx(t - before), self.idx(t + after)
        best = None
        for i in range(max(i0, 1), min(i1, self.active.size)):
            if self.active[i] and not self.active[i - 1]:
                if best is None or abs(i * self.hop - t) < abs(best - t):
                    best = i * self.hop
        if best is None and i0 == 0 and self.active.size and self.active[0] and t < after:
            best = 0.0
        return best

    def offset(self, t: float, before: float = 0.2, after: float = 0.3):
        """t 근처에서 말소리가 끝나는 순간. 없으면 None."""
        i0, i1 = self.idx(t - before), self.idx(t + after)
        best = None
        for i in range(max(i0, 1), min(i1, self.active.size)):
            if self.active[i - 1] and not self.active[i]:
                if best is None or abs(i * self.hop - t) < abs(best - t):
                    best = i * self.hop
        return best

    def spans(self, t0: float = 0.0, t1: float | None = None):
        """말소리 구간 목록 [(시작, 끝)]."""
        i0 = self.idx(t0)
        i1 = self.idx(self.duration if t1 is None else t1)
        out = []
        for value, s, e in _runs(self.active[i0:i1]):
            if value:
                out.append(((i0 + s) * self.hop, (i0 + e) * self.hop))
        return out

    def quiet_spans(self, t0: float, t1: float, min_len: float, margin_db: float = 4.0):
        """[t0, t1] 안의 조용한 구간(느슨한 기준). 단어 시간이 침묵을 덮고 있을 때 찾는 용도."""
        i0, i1 = self.idx(t0), self.idx(t1)
        quiet = self.db[i0:i1] < (self.thr - margin_db)
        out = []
        for value, s, e in _runs(quiet):
            if value and (e - s) * self.hop >= min_len:
                out.append(((i0 + s) * self.hop, (i0 + e) * self.hop))
        return out


# ---------- 음성 처리 ----------
VOICE_CHAIN = "highpass=f=80,afftdn=nr=10:nf=-50:tn=1,acompressor=threshold=-22dB:ratio=3:attack=8:release=160:knee=4"


def limiter(limit: float) -> str:
    """피크 제한기. 지원되면 지연 보정(latency)을 켜서 소리가 밀리지 않게 한다."""
    text = run([need("ffmpeg"), "-hide_banner", "-h", "filter=alimiter"], check=False).stdout.decode("utf-8", "replace")
    extra = ":latency=1" if "latency" in text else ""
    return f"alimiter=limit={limit:.4f}:level=0:attack=5:release=60{extra}"


def measure_loudness(path: Path, chain: str = "") -> float | None:
    """통합 음량(LUFS). chain을 주면 그 필터를 거친 소리를 잰다."""
    af = f"{chain},ebur128=framelog=quiet" if chain else "ebur128=framelog=quiet"
    proc = run([need("ffmpeg"), "-hide_banner", "-nostdin", "-i", path, "-af", af, "-f", "null", "-"], check=False)
    found = re.findall(r"I:\s+(-?[\d.]+|-inf) LUFS", proc.stderr.decode("utf-8", "replace"))
    if not found or found[-1] == "-inf":
        return None
    return float(found[-1])


def normalize_voice(src: Path, dst: Path, target: float = -14.0, true_peak: float = -1.5) -> dict:
    """잡음 줄이기 + 압축으로 목소리 크기를 고르게 → 목표 음량까지 올리고 → 피크는 제한기로 누른다.
    (loudnorm 선형 모드는 피크 때문에 목표보다 작게 끝나는 일이 잦아 직접 맞춘다)"""
    ffmpeg = need("ffmpeg")
    lim = limiter(10 ** ((true_peak - 0.5) / 20))
    measured = measure_loudness(src, VOICE_CHAIN)
    if measured is None or measured < -70:
        run([ffmpeg, "-hide_banner", "-nostdin", "-y", "-i", src, "-af", VOICE_CHAIN, "-ar", str(SR), "-ac", "1", "-c:a", "pcm_f32le", dst])
        return {"input_i": measured, "output_i": measured, "gain_db": 0.0}
    gain = target - measured
    got = None
    for _ in range(3):
        run([ffmpeg, "-hide_banner", "-nostdin", "-y", "-i", src, "-af", f"{VOICE_CHAIN},volume={gain:.2f}dB,{lim}", "-ar", str(SR), "-ac", "1", "-c:a", "pcm_f32le", dst])
        got = measure_loudness(dst)
        if got is None or abs(got - target) <= 0.3:
            break
        gain += target - got
    return {"input_i": round(measured, 2), "output_i": got, "gain_db": round(gain, 2)}


def rms_db(x: np.ndarray, active_only: bool = True) -> float:
    x = np.asarray(x, np.float64)
    if x.size == 0:
        return -120.0
    if active_only:
        frame = 480
        n = x.size // frame
        if n >= 4:
            frames = x[: n * frame].reshape(n, frame)
            levels = np.sqrt((frames**2).mean(axis=1) + 1e-12)
            keep = levels >= np.percentile(levels, 60)
            x = frames[keep].ravel()
    return float(20 * np.log10(np.sqrt((x**2).mean()) + 1e-12))


def mix(voice: np.ndarray, events: list, sounds: dict, gains: dict, volume: float) -> np.ndarray:
    """목소리 위에 효과음을 얹는다. events: [{'type','t'}], sounds: 이름→샘플, gains: 이름→dB."""
    out = np.array(voice, np.float32, copy=True)
    scale = float(volume)
    for ev in events:
        sample = sounds.get(ev["type"])
        if sample is None or scale <= 0:
            continue
        start = int(round(ev["start"] * SR))
        if start >= out.size:
            continue
        gain = scale * 10 ** (gains.get(ev["type"], -12.0) / 20)
        seg = sample
        if start < 0:
            seg = seg[-start:]
            start = 0
        seg = seg[: max(0, out.size - start)]
        out[start : start + seg.size] += seg * gain
    return out


def write_mix(path: Path, samples: np.ndarray) -> None:
    write_wav(path, samples)
