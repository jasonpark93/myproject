"""참고 영상(잘 된 릴스) 분석: 컷·줌 리듬, 자막 위치·크기·강조색, 말 속도·쉼, 음량을 숫자로 뽑는다.

결과(reference.md + 장면 모음 이미지)를 보고 우리 설정(gap, zoom, caption...)을 맞춘다.
"""

from __future__ import annotations

import subprocess
from pathlib import Path

import numpy as np

from . import ROOT
from .audio import Envelope, measure_loudness
from .media import decode_audio, probe, sdr_filter
from .util import fmt_time, need, run, write_json, write_text

AW, AH = 270, 480  # 움직임 분석 해상도 (세로 9:16 기준)


def _frames(path: Path, width: int, height: int, fps: float, sdr: str, gray: bool):
    fmt = "gray" if gray else "bgr24"
    chain = ",".join(f for f in (sdr, f"fps={fps}", f"scale={width}:{height}:force_original_aspect_ratio=decrease,pad={width}:{height}:(ow-iw)/2:(oh-ih)/2", f"format={fmt}") if f)
    cmd = [need("ffmpeg"), "-v", "error", "-nostdin", "-i", str(path), "-map", "0:v:0", "-vf", chain, "-f", "rawvideo", "-"]
    proc = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
    size = width * height * (1 if gray else 3)
    shape = (height, width) if gray else (height, width, 3)
    while True:
        buf = proc.stdout.read(size)
        if len(buf) < size:
            break
        yield np.frombuffer(buf, np.uint8).reshape(shape)
    proc.wait()


def _scale(matcher, a, b):
    """두 프레임 특징점으로 확대 비율과 맞는 점 개수를 구한다."""
    import cv2

    kp_a, des_a = a
    kp_b, des_b = b
    if des_a is None or des_b is None or len(kp_a) < 8 or len(kp_b) < 8:
        return 1.0, 0
    matches = matcher.match(des_a, des_b)
    if len(matches) < 8:
        return 1.0, 0
    src = np.float32([kp_a[m.queryIdx].pt for m in matches])
    dst = np.float32([kp_b[m.trainIdx].pt for m in matches])
    m, inliers = cv2.estimateAffinePartial2D(src, dst, ransacReprojThreshold=1.5)
    if m is None or inliers is None:
        return 1.0, 0
    return float(np.sqrt(abs(np.linalg.det(m[:, :2])))), int(inliers.sum())


def motion(path: Path, fps: float, sdr: str, lag: int = 30):
    """프레임 사이 변화량·확대 비율 → (시간, 변화량, 직전 대비 배율, 맞는 점 수, lag프레임 전 대비 배율).
    자막·제목 글자는 확대되지 않으므로 얼굴이 있는 가운데 높이만 본다."""
    import cv2

    orb = cv2.ORB_create(600)
    matcher = cv2.BFMatcher(cv2.NORM_HAMMING, crossCheck=True)
    mask = np.zeros((AH, AW), np.uint8)
    mask[int(AH * 0.22) : int(AH * 0.62), :] = 255
    history, prev = [], None
    rows = []
    for i, frame in enumerate(_frames(path, AW, AH, fps, sdr, gray=True)):
        feats = orb.detectAndCompute(frame, mask)
        if prev is not None:
            diff = float(np.mean(cv2.absdiff(frame, prev)))
            scale, good = _scale(matcher, history[-1], feats)
            long_scale = None
            if len(history) >= lag:
                long_scale, long_good = _scale(matcher, history[-lag], feats)
                if long_good < 12:
                    long_scale = None
            rows.append((i / fps, diff, scale, good, long_scale))
        prev = frame
        history.append(feats)
        history = history[-lag:]
    return rows


def events(rows: list, fps: float = 30.0, lag: int = 30):
    """하드컷·점프컷·펀치 줌(순간 확대/축소)·슬로우 줌 구간 찾기."""
    if not rows:
        return [], [], [], []
    diffs = np.array([r[1] for r in rows])
    base = float(np.median(diffs)) + 1e-6
    hard, jump, punch = [], [], []
    for t, diff, scale, good, _ in rows:
        spike = diff > max(base * 4, 6.0)
        if abs(np.log(scale)) > 0.035 and good >= 10:
            punch.append((t, scale))
        elif spike and good < 10:
            hard.append(t)
        elif spike:
            jump.append(t)
    slow = []
    cuts = sorted([t for t, _ in punch] + hard + jump)
    run_start, last = None, None
    for t, diff, scale, good, long_scale in rows:
        steady = long_scale is not None and 1.008 < long_scale < 1.1 and not any(t - lag / fps - 0.02 <= c <= t for c in cuts)
        if steady:
            run_start = t if run_start is None else run_start
            last = t
        elif run_start is not None and t - (last or t) > 0.25:
            if last - run_start >= 0.8:
                slow.append((run_start, last, None))
            run_start, last = None, None
    if run_start is not None and last - run_start >= 0.8:
        slow.append((run_start, last, None))
    out = []
    span = lag / fps
    for a, b, _ in slow:  # 1초 전 대비 배율로 속도를 구하고, 시작은 1초 앞당긴다(비교 창 길이만큼 늦게 잡히므로)
        rates = [ls for t, _, _, _, ls in rows if a <= t <= b and ls is not None]
        rate = float(np.median(rates)) if rates else 1.0
        start = max([c for c in cuts if c <= a] + [a - span])
        out.append((start, b, rate ** ((b - start) / span)))
    return hard, jump, punch, out


def caption_bands(path: Path, sdr: str, every: float = 0.5):
    """흰 글씨 + 검은 테두리 덩어리가 자주 나오는 높이 → 자막 위치·글자 높이·노란 강조 비율."""
    import cv2

    w, h = 540, 960
    centers, heights, yellow_frames, frames = [], [], 0, 0
    kernel = np.ones((5, 5), np.uint8)
    for frame in _frames(path, w, h, 1 / every, sdr, gray=False):
        frames += 1
        hsv = cv2.cvtColor(frame, cv2.COLOR_BGR2HSV)
        white = ((hsv[..., 2] > 215) & (hsv[..., 1] < 45)).astype(np.uint8)
        yellow = ((hsv[..., 0] > 18) & (hsv[..., 0] < 38) & (hsv[..., 1] > 110) & (hsv[..., 2] > 180)).astype(np.uint8)
        dark = (hsv[..., 2] < 60).astype(np.uint8)
        near_dark = cv2.dilate(dark, kernel)
        text = ((white | yellow) & near_dark).astype(np.float32)
        rows = text.mean(axis=1)
        band = rows > 0.03
        if not band.any():
            continue
        best, cur = None, None
        for y, on in enumerate(band):
            if on and cur is None:
                cur = y
            if (not on or y == h - 1) and cur is not None:
                seg = (cur, y)
                mass = rows[seg[0] : seg[1]].sum()
                if seg[1] - seg[0] >= 12 and (best is None or mass > best[2]):
                    best = (seg[0], seg[1], mass)
                cur = None
        if best is None:
            continue
        y0, y1, _ = best
        centers.append(1 - (y0 + y1) / 2 / h)
        heights.append((y1 - y0) / h * 1920)
        if (yellow[y0:y1] & near_dark[y0:y1]).sum() > 30:
            yellow_frames += 1
    return centers, heights, yellow_frames, frames


def contact_sheet(path: Path, duration: float, out: Path, sdr: str, count: int = 12) -> None:
    times = [duration * (i + 0.5) / count for i in range(count)]
    tiles = []
    for t in times:
        chain = ",".join(f for f in (sdr, "scale=270:480:force_original_aspect_ratio=decrease,pad=270:480:(ow-iw)/2:(oh-ih)/2") if f)
        data = run([need("ffmpeg"), "-v", "error", "-nostdin", "-ss", f"{t:.2f}", "-i", str(path), "-frames:v", "1", "-vf", chain, "-f", "rawvideo", "-pix_fmt", "rgb24", "-"]).stdout
        if len(data) == 270 * 480 * 3:
            tiles.append((t, np.frombuffer(data, np.uint8).reshape(480, 270, 3)))
    if not tiles:
        return
    from PIL import Image, ImageDraw

    cols = 6
    rows_n = (len(tiles) + cols - 1) // cols
    sheet = Image.new("RGB", (270 * cols, 480 * rows_n), (0, 0, 0))
    draw = ImageDraw.Draw(sheet)
    for n, (t, tile) in enumerate(tiles):
        x, y = (n % cols) * 270, (n // cols) * 480
        sheet.paste(Image.fromarray(tile), (x, y))
        draw.rectangle([x, y, x + 74, y + 22], fill=(0, 0, 0))
        draw.text((x + 4, y + 4), fmt_time(t), fill=(255, 255, 0))
    sheet.save(out, quality=88)


def analyze_reference(path: Path, log=print) -> Path:
    p = probe(path)
    sdr, _ = sdr_filter(p)
    name = "ref-" + "".join(ch if ch.isalnum() else "_" for ch in path.stem)[:40]
    work = ROOT / "work" / name
    work.mkdir(parents=True, exist_ok=True)
    log(f"참고 영상 분석: {path.name} ({fmt_time(p.duration)})")
    fps = min(30.0, p.fps or 30.0)
    lag = max(1, int(round(fps)))  # 1초 전 화면과 비교해야 천천히 커지는 줌이 보인다
    rows = motion(path, fps, sdr, lag)
    hard, jump, punch, slow = events(rows, fps, lag)
    centers, heights, yellow_frames, cap_frames = caption_bands(path, sdr)
    speech = {}
    if p.has_audio:
        x = decode_audio(path, duration=p.duration)
        env = Envelope.from_samples(x)
        spans = env.spans()
        gaps = [b[0] - a[1] for a, b in zip(spans, spans[1:])]
        speech = {
            "first_sound": round(spans[0][0], 2) if spans else None,
            "speech_ratio": round(float(env.active.mean()), 2),
            "median_gap": round(float(np.median(gaps)), 2) if gaps else None,
            "gaps_over_0.3s_per_10s": round(sum(1 for g in gaps if g > 0.3) / max(1e-6, p.duration) * 10, 1),
            "loudness": measure_loudness(path),
        }
    zooms = len(punch) + len(slow)
    per10 = lambda n: round(n / max(1e-6, p.duration) * 10, 1)  # noqa: E731
    stats = {
        "duration": round(p.duration, 2),
        "hard_cuts": [round(t, 2) for t in hard],
        "jump_cuts": [round(t, 2) for t in jump],
        "punch_zooms": [(round(t, 2), round(s, 3)) for t, s in punch],
        "slow_zooms": [(round(a, 2), round(b, 2), round(z, 3)) for a, b, z in slow],
        "visual_changes_per_10s": per10(len(hard) + len(jump) + zooms),
        "caption_from_bottom": round(float(np.median(centers)), 3) if centers else None,
        "caption_height_px": round(float(np.median(heights))) if heights else None,
        "caption_frames_ratio": round(len(centers) / max(1, cap_frames), 2),
        "highlight_frames_ratio": round(yellow_frames / max(1, len(centers)), 2) if centers else 0,
        **speech,
    }
    write_json(work / "reference.json", stats)
    contact_sheet(path, p.duration, work / "reference_sheet.jpg", sdr)
    lines = [
        f"# 참고 영상 분석 — {path.name}",
        "",
        f"- 길이 {fmt_time(p.duration)}, {p.width}x{p.height}, {p.fps:g}fps",
        f"- 화면 변화: 10초에 {stats['visual_changes_per_10s']}번 (하드컷 {len(hard)} · 점프컷 {len(jump)} · 펀치 줌 {len(punch)} · 슬로우 줌 {len(slow)})",
        f"- 펀치 줌 배율: {', '.join(f'{fmt_time(t)} ×{s:.2f}' for t, s in punch[:12]) or '없음'}",
        f"- 슬로우 줌: {', '.join(f'{fmt_time(a)}~{fmt_time(b)} ×{z:.2f}' for a, b, z in slow[:8]) or '없음'}",
        f"- 자막: 화면 아래에서 {stats['caption_from_bottom']} 높이, 글자 높이 약 {stats['caption_height_px']}px(1920 기준), 자막 있는 화면 {stats['caption_frames_ratio'] * 100:.0f}%, 노란 강조 {stats['highlight_frames_ratio'] * 100:.0f}%",
    ]
    if speech:
        lines.append(
            f"- 말: 첫 소리 {speech['first_sound']}초, 말하는 비율 {speech['speech_ratio'] * 100:.0f}%, 쉼(0.3초↑) 10초에 {speech['gaps_over_0.3s_per_10s']}번, 쉼 중앙값 {speech['median_gap']}초, 음량 {speech['loudness']} LUFS"
        )
    lines += ["", f"장면 모음: `{work / 'reference_sheet.jpg'}`", "", "※ 자동 측정이라 오차가 있습니다. 장면 모음 이미지를 함께 보고 판단하세요."]
    text = "\n".join(lines) + "\n"
    write_text(work / "reference.md", text)
    log(text)
    return work
