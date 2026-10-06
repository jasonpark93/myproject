"""효과음: 휙(whoosh)·띵(ding)·뽁(pop)·쩍(crack, 깨질 때)·톡(tap, 화면 누를 때).

reels/sfx/ 에 whoosh.wav, ding.wav, pop.wav, crack.wav, tap.wav(또는 mp3·m4a)를 넣으면 그 파일을 쓰고, 없으면 여기서 직접 만든다.
효과음은 목소리보다 작게, 10초에 최대 3개까지만 (많으면 싸구려처럼 들린다).
"""

from __future__ import annotations

from pathlib import Path

import numpy as np

from . import ROOT, SR
from .media import decode_audio

KINDS = ("whoosh", "ding", "pop", "crack", "tap")
LABEL = {"whoosh": "휙", "ding": "띵", "pop": "뽁", "crack": "쩍", "tap": "톡"}
PRIORITY = {"crack": 0, "whoosh": 0, "ding": 1, "tap": 1, "pop": 2}
# 목소리(말하는 부분 평균)보다 몇 dB 작게 넣을지
UNDER_VOICE_DB = {"whoosh": 9.0, "ding": 11.0, "pop": 8.0, "crack": 6.0, "tap": 9.0}
PEAK_CAP_DB = -6.0  # 효과음 순간 최대치가 이보다 크지 않게 (짧은 '뽁'이 귀를 찌르지 않도록)
SFX_DIR = ROOT / "sfx"
EXTS = (".wav", ".mp3", ".m4a", ".aac", ".aif", ".aiff", ".ogg", ".flac")


def _normalize(x: np.ndarray, peak: float = 0.9) -> np.ndarray:
    m = float(np.max(np.abs(x))) or 1.0
    return (x / m * peak).astype(np.float32)


def synth_ding(sr: int = SR) -> np.ndarray:
    """맑은 종소리 '띵' (기음 + 종 특유의 배음, 지수 감쇠)."""
    t = np.arange(int(0.9 * sr)) / sr
    f0 = 1318.5  # E6
    sig = np.zeros_like(t)
    for ratio, amp, decay in ((1.0, 1.0, 0.45), (2.0, 0.32, 0.22), (2.76, 0.22, 0.16), (5.4, 0.07, 0.07)):
        sig += amp * np.sin(2 * np.pi * f0 * ratio * t) * np.exp(-t / decay)
    sig *= 1 - np.exp(-t / 0.0015)
    return _normalize(sig)


def synth_whoosh(sr: int = SR, seed: int = 7) -> np.ndarray:
    """바람 가르는 '휙': 잡음의 통과 대역을 낮은 음→높은 음으로 쓸어 올린다."""
    dur = 0.45
    n = int(dur * sr)
    rng = np.random.default_rng(seed)
    noise = rng.standard_normal(n + 2048)
    win, hop = 1024, 256
    window = np.hanning(win)
    frames = 1 + (noise.size - win) // hop
    freqs = np.fft.rfftfreq(win, 1 / sr)
    out = np.zeros(noise.size)
    norm = np.zeros(noise.size)
    for k in range(frames):
        pos = k / max(1, frames - 1)
        center = 350 * (3000 / 350) ** min(1.0, pos / 0.75)  # 350Hz → 3kHz
        width = 0.9  # 옥타브
        mask = np.exp(-0.5 * (np.log2(np.maximum(freqs, 1) / center) / width) ** 2)
        seg = noise[k * hop : k * hop + win] * window
        spec = np.fft.rfft(seg) * mask
        out[k * hop : k * hop + win] += np.fft.irfft(spec, win) * window
        norm[k * hop : k * hop + win] += window**2
    sig = (out / np.maximum(norm, 1e-6))[:n]
    t = np.arange(n) / n
    env = np.where(t < 0.68, (t / 0.68) ** 2.2, np.exp(-(t - 0.68) / 0.09))
    return _normalize(sig * env)


def synth_pop(sr: int = SR) -> np.ndarray:
    """물방울 터지는 '뽁': 빠르게 떨어지는 음높이 + 짧은 클릭."""
    t = np.arange(int(0.14 * sr)) / sr
    freq = 250 + 1000 * np.exp(-t / 0.018)
    phase = 2 * np.pi * np.cumsum(freq) / sr
    body = np.sin(phase) * np.exp(-t / 0.035) * (1 - np.exp(-t / 0.0008))
    click = np.zeros_like(t)
    m = int(0.002 * sr)
    click[:m] = np.random.default_rng(3).standard_normal(m) * np.linspace(1, 0, m) * 0.5
    return _normalize(body + click)


def synth_crack(sr: int = SR) -> np.ndarray:
    """무언가 '쩍' 갈라지는 소리: 날카로운 잡음 터짐 + 낮은 쿵 + 잔금 가는 자글거림."""
    rng = np.random.default_rng(11)
    n = int(0.42 * sr)
    t = np.arange(n) / sr
    burst = rng.standard_normal(n) * np.exp(-t / 0.018)
    burst = np.diff(burst, prepend=0.0)  # 고음 위주(날카롭게)
    thump = np.sin(2 * np.pi * (95 + 60 * np.exp(-t / 0.02)) * t) * np.exp(-t / 0.07) * 0.9
    crackle = np.zeros(n)
    for _ in range(26):  # 잔금: 작은 딸깍들이 0.25초 동안 흩어짐
        at = int(rng.uniform(0.005, 0.25) * sr)
        m = int(0.003 * sr)
        if at + m < n:
            crackle[at : at + m] += rng.standard_normal(m) * np.linspace(1, 0, m) * rng.uniform(0.2, 0.7) * np.exp(-at / sr / 0.15)
    sig = burst * 1.2 + thump + crackle
    sig *= 1 - np.exp(-t / 0.0006)
    return _normalize(sig)


def synth_tap(sr: int = SR) -> np.ndarray:
    """화면을 '톡' 누르는 소리: 아주 짧은 딸깍 + 높은 틱."""
    n = int(0.08 * sr)
    t = np.arange(n) / sr
    click = np.random.default_rng(5).standard_normal(n) * np.exp(-t / 0.0025)
    tick = np.sin(2 * np.pi * 2300 * t) * np.exp(-t / 0.012) * 0.6
    return _normalize(np.diff(click, prepend=0.0) + tick)


SYNTH = {"whoosh": synth_whoosh, "ding": synth_ding, "pop": synth_pop, "crack": synth_crack, "tap": synth_tap}
# 소리 안에서 '터지는' 순간(초): 화면 변화와 이 순간을 맞춘다
SYNTH_HIT = {"whoosh": 0.30, "ding": 0.0, "pop": 0.0, "crack": 0.0, "tap": 0.0}


def user_file(kind: str, folder: Path = SFX_DIR):
    for ext in EXTS:
        path = folder / f"{kind}{ext}"
        if path.exists():
            return path
    return None


def load(folder: Path = SFX_DIR) -> tuple[dict, dict, dict]:
    """(소리, 터지는 순간, 출처) — 사용자 파일이 있으면 그걸 쓴다."""
    sounds, hits, origin = {}, {}, {}
    for kind in KINDS:
        path = user_file(kind, folder)
        if path is not None:
            x = decode_audio(path)
            x = x[: int(2.0 * SR)]  # 효과음은 2초까지만
            fade = min(x.size, int(0.03 * SR))
            if fade:
                x[-fade:] *= np.linspace(1, 0, fade, dtype=np.float32)
            sounds[kind] = _normalize(x)
            hits[kind] = float(np.argmax(np.abs(x)) / SR)
            origin[kind] = path.name
        else:
            sounds[kind] = SYNTH[kind]()
            hits[kind] = SYNTH_HIT[kind]
            origin[kind] = "자동 생성"
    return sounds, hits, origin


def gain_db(sample: np.ndarray, kind: str, voice_rms_db: float) -> float:
    """목소리 평균보다 UNDER_VOICE_DB만큼 작게, 그리고 순간 최대치는 PEAK_CAP_DB 이하로."""
    from .audio import rms_db

    by_rms = voice_rms_db - UNDER_VOICE_DB.get(kind, 10.0) - rms_db(sample, active_only=False)
    peak = float(np.max(np.abs(sample))) or 1.0
    by_peak = PEAK_CAP_DB - 20 * np.log10(peak)
    return float(min(by_rms, by_peak))


def thin(events: list, max_per_10s: int, min_gap: float = 0.7) -> list:
    """중요한 것(휙 > 띵 > 뽁)부터 고르되, 0.7초 안에 겹치거나 10초에 max개를 넘으면 뺀다."""
    chosen = []
    for ev in sorted(events, key=lambda e: (0 if e.get("fixed") else 1, PRIORITY.get(e["type"], 9), e["t"])):
        if any(abs(ev["t"] - c["t"]) < min_gap for c in chosen):
            continue
        window = [c for c in chosen if abs(c["t"] - ev["t"]) < 10.0]
        times = sorted([c["t"] for c in window] + [ev["t"]])
        crowded = any(sum(1 for x in times if s <= x < s + 10.0) > max_per_10s for s in times)
        if crowded and not ev.get("fixed"):
            continue
        chosen.append(ev)
    return sorted(chosen, key=lambda e: e["t"])
