"""ffprobe/ffmpeg 래퍼: 원본 정보, 오디오 디코딩, WAV 저장, HDR 변환 필터."""

from __future__ import annotations

import functools
import json
import struct
from dataclasses import asdict, dataclass
from pathlib import Path

import numpy as np

from . import SR
from .util import EditorError, need, run


@dataclass
class Probe:
    path: str
    duration: float
    width: int  # 회전을 반영한 화면 기준 가로
    height: int
    fps: float
    rotation: int
    has_audio: bool
    video_start: float
    audio_start: float
    color_transfer: str
    vcodec: str

    @property
    def hdr(self) -> bool:
        return self.color_transfer in ("arib-std-b67", "smpte2084")

    @property
    def vertical(self) -> bool:
        return self.height >= self.width

    def to_dict(self) -> dict:
        return asdict(self)

    @classmethod
    def from_dict(cls, data: dict) -> "Probe":
        return cls(**{k: data[k] for k in cls.__dataclass_fields__})


def _rate(text: str) -> float:
    try:
        num, _, den = str(text).partition("/")
        value = float(num) / float(den or 1)
        return value if value > 0 else 0.0
    except (ValueError, ZeroDivisionError):
        return 0.0


def _rotation(stream: dict) -> int:
    for side in stream.get("side_data_list") or []:
        if "rotation" in side:
            return int(round(float(side["rotation"]))) % 360
    rotate = (stream.get("tags") or {}).get("rotate")
    return int(rotate) % 360 if rotate else 0


def probe(path: Path) -> Probe:
    path = Path(path)
    if not path.exists():
        raise EditorError(f"파일이 없습니다: {path}")
    out = run([need("ffprobe"), "-v", "error", "-show_streams", "-show_format", "-of", "json", path]).stdout
    info = json.loads(out)
    streams = info.get("streams") or []
    video = next((s for s in streams if s.get("codec_type") == "video" and not (s.get("disposition") or {}).get("attached_pic")), None)
    audio = next((s for s in streams if s.get("codec_type") == "audio"), None)
    if video is None:
        raise EditorError(f"영상 트랙이 없는 파일입니다: {path.name}")
    rotation = _rotation(video)
    width, height = int(video["width"]), int(video["height"])
    if rotation in (90, 270):
        width, height = height, width
    duration = float((info.get("format") or {}).get("duration") or video.get("duration") or 0)
    fps = _rate(video.get("avg_frame_rate")) or _rate(video.get("r_frame_rate")) or 30.0
    return Probe(
        path=str(path.resolve()),
        duration=duration,
        width=width,
        height=height,
        fps=round(fps, 3),
        rotation=rotation,
        has_audio=audio is not None,
        video_start=float(video.get("start_time") or 0),
        audio_start=float((audio or {}).get("start_time") or 0),
        color_transfer=str(video.get("color_transfer") or ""),
        vcodec=str(video.get("codec_name") or ""),
    )


def decode_audio(path: Path, sr: int = SR, duration: float | None = None) -> np.ndarray:
    """모노 float32. 오디오가 없으면 길이에 맞는 무음."""
    cmd = [need("ffmpeg"), "-v", "error", "-nostdin", "-i", path, "-map", "0:a:0", "-ac", "1", "-ar", str(sr), "-f", "f32le", "-"]
    proc = run(cmd, check=False)
    if proc.returncode != 0 or not proc.stdout:
        if duration:
            return np.zeros(int(duration * sr), np.float32)
        raise EditorError(f"오디오를 읽지 못했습니다: {Path(path).name}")
    return np.frombuffer(proc.stdout, np.float32).copy()


def write_wav(path: Path, samples: np.ndarray, sr: int = SR) -> None:
    """32비트 float WAV (중간 파일용, 음질 손실 없음)."""
    data = np.ascontiguousarray(samples, dtype="<f4")
    if data.ndim == 1:
        channels = 1
    else:
        channels = data.shape[1]
    payload = data.tobytes()
    header = b"RIFF" + struct.pack("<I", 36 + len(payload)) + b"WAVE"
    header += b"fmt " + struct.pack("<IHHIIHH", 16, 3, channels, sr, sr * channels * 4, channels * 4, 32)
    header += b"data" + struct.pack("<I", len(payload))
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    with open(path, "wb") as f:
        f.write(header)
        f.write(payload)


@functools.lru_cache(maxsize=None)
def ffmpeg_features() -> tuple[frozenset, frozenset]:
    ffmpeg = need("ffmpeg")
    filters = run([ffmpeg, "-hide_banner", "-filters"], check=False).stdout.decode("utf-8", "replace")
    encoders = run([ffmpeg, "-hide_banner", "-encoders"], check=False).stdout.decode("utf-8", "replace")

    def names(text: str) -> frozenset:
        found = set()
        for line in text.splitlines():
            parts = line.split()
            if len(parts) >= 2 and len(parts[0]) <= 6 and not line.startswith(" ="):
                found.add(parts[1])
        return frozenset(found)

    return names(filters), names(encoders)


def has_filter(name: str) -> bool:
    return name in ffmpeg_features()[0]


def has_encoder(name: str) -> bool:
    return name in ffmpeg_features()[1]


def sdr_filter(p: Probe) -> tuple[str, str | None]:
    """아이폰 HDR(HLG/Dolby Vision) 원본을 일반 화면(SDR) 색으로 바꾸는 필터. (필터, 경고)"""
    if not p.hdr:
        return "", None
    if has_filter("zscale") and has_filter("tonemap"):
        chain = (
            "zscale=t=linear:npl=100,format=gbrpf32le,zscale=p=bt709,"
            "tonemap=tonemap=hable:desat=0,zscale=t=bt709:m=bt709:r=tv,format=yuv420p"
        )
        return chain, None
    return "", "원본이 HDR 영상인데 ffmpeg에 색 변환 기능(zscale)이 없어 색이 바래 보일 수 있습니다. 아이폰 설정 > 카메라 > 비디오 녹화 > 'HDR 비디오'를 끄고 찍는 걸 권장합니다."
