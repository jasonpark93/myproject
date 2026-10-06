"""얼굴 위치 추적 (OpenCV YuNet). 줌할 때 얼굴이 화면 밖으로 나가지 않게, 가로 영상은 얼굴 중심으로 자른다."""

from __future__ import annotations

from pathlib import Path

import numpy as np

from .media import Probe
from .util import need

MODEL = Path(__file__).resolve().parent / "models" / "face_detection_yunet_2023mar.onnx"
DETECT_WIDTH = 480


def _detector(width: int, height: int):
    try:
        import cv2
    except ImportError:
        return None
    if not MODEL.exists() or not hasattr(cv2, "FaceDetectorYN"):
        return None
    try:
        return cv2.FaceDetectorYN.create(str(MODEL), "", (width, height), 0.6, 0.3, 5000)
    except Exception:
        return None


def track(p: Probe, sdr: str = "", every: float = 0.5, log=print):
    """{'t': [...], 'x': [...], 'y': [...], 'h': [...]} (화면 대비 0~1) 또는 얼굴을 못 찾으면 None."""
    import subprocess

    width = DETECT_WIDTH
    height = int(round(p.height * width / p.width / 2)) * 2
    det = _detector(width, height)
    if det is None:
        log("얼굴 인식 기능을 쓸 수 없어 화면 가운데를 기준으로 줌합니다 (opencv-python-headless 설치 필요).")
        return None
    chain = ",".join(f for f in (sdr, f"fps=1/{every}", f"scale={width}:{height}", "format=bgr24") if f)
    cmd = [need("ffmpeg"), "-v", "error", "-nostdin", "-i", p.path, "-map", "0:v:0", "-vf", chain, "-f", "rawvideo", "-"]
    proc = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
    size = width * height * 3
    ts, xs, ys, hs = [], [], [], []
    n = 0
    while True:
        buf = proc.stdout.read(size)
        if len(buf) < size:
            break
        frame = np.frombuffer(buf, np.uint8).reshape(height, width, 3)
        _, faces = det.detect(frame)
        t = n * every + (p.video_start - p.audio_start)
        n += 1
        if faces is None or len(faces) == 0:
            continue
        x, y, w, h = max(faces, key=lambda f: f[2] * f[3])[:4]
        ts.append(t)
        xs.append((x + w / 2) / width)
        ys.append((y + h / 2) / height)
        hs.append(h / height)
    proc.wait()
    if len(ts) < max(2, n // 5):
        log("얼굴을 거의 찾지 못해 화면 가운데를 기준으로 줌합니다.")
        return None
    return {"t": [round(v, 3) for v in ts], "x": [round(float(v), 4) for v in xs], "y": [round(float(v), 4) for v in ys], "h": [round(float(v), 4) for v in hs]}


def median_in(face: dict | None, t0: float, t1: float, default=(0.5, 0.42)):
    """구간 [t0, t1] 동안 얼굴 중심(중앙값). 구간 안에 검출이 없으면 가장 가까운 검출."""
    if not face or not face["t"]:
        return default
    t = np.asarray(face["t"])
    sel = (t >= t0 - 0.25) & (t <= t1 + 0.25)
    if not sel.any():
        k = int(np.argmin(np.abs(t - (t0 + t1) / 2)))
        return face["x"][k], face["y"][k]
    return float(np.median(np.asarray(face["x"])[sel])), float(np.median(np.asarray(face["y"])[sel]))
