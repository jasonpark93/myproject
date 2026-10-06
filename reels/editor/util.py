"""공통 도구: 외부 명령 실행, 시간 표기, 해시, JSON 저장."""

from __future__ import annotations

import hashlib
import json
import os
import re
import shutil
import subprocess
from pathlib import Path


class EditorError(RuntimeError):
    """사용자에게 그대로 보여줄 수 있는 오류."""


INSTALL_HINT = {
    "ffmpeg": "ffmpeg가 필요합니다. Mac: 'brew install ffmpeg' / Windows: 'winget install Gyan.FFmpeg' 후 터미널을 다시 여세요.",
    "ffprobe": "ffprobe가 필요합니다(ffmpeg에 같이 들어 있습니다). Mac: 'brew install ffmpeg' / Windows: 'winget install Gyan.FFmpeg'.",
}


def need(tool: str) -> str:
    path = shutil.which(tool)
    if not path:
        raise EditorError(INSTALL_HINT.get(tool, f"{tool}을(를) 찾지 못했습니다."))
    return path


def run(cmd, *, input: bytes | None = None, check: bool = True) -> subprocess.CompletedProcess:
    proc = subprocess.run([str(c) for c in cmd], input=input, capture_output=True)
    if check and proc.returncode != 0:
        tail = proc.stderr.decode("utf-8", "replace").strip().splitlines()[-12:]
        raise EditorError(f"{Path(str(cmd[0])).name} 실패 (코드 {proc.returncode}):\n" + "\n".join(tail))
    return proc


def fmt_time(t: float) -> str:
    """0:07.3 형태. 사용자가 '0:12 다시 살려줘'처럼 말하는 단위와 맞춘다."""
    t = max(0.0, float(t))
    minutes = int(t // 60)
    seconds = t - minutes * 60
    if seconds >= 59.95:
        minutes, seconds = minutes + 1, 0.0
    return f"{minutes}:{seconds:04.1f}"


_TIME = re.compile(r"^\s*(?:(\d+):)?(\d+(?:\.\d+)?)\s*(?:초)?\s*$")


def parse_time(text: str) -> float:
    """'0:12', '12', '12.5초', '1:02.3' → 초."""
    m = _TIME.match(str(text))
    if not m:
        raise EditorError(f"시간 형식을 이해하지 못했습니다: {text!r} (예: 0:12, 12.5)")
    minutes = int(m.group(1) or 0)
    return minutes * 60 + float(m.group(2))


def stable_hash(*parts) -> str:
    h = hashlib.sha1()
    for part in parts:
        h.update(json.dumps(part, sort_keys=True, ensure_ascii=False, default=str).encode("utf-8"))
        h.update(b"\0")
    return h.hexdigest()[:16]


def file_hash(path: Path, limit: int = 4 << 20) -> str:
    """파일 앞부분 + 크기로 만든 빠른 지문 (대용량 영상도 즉시)."""
    path = Path(path)
    h = hashlib.sha1()
    h.update(str(path.stat().st_size).encode())
    with open(path, "rb") as f:
        h.update(f.read(limit))
    return h.hexdigest()[:16]


def read_json(path: Path):
    return json.loads(Path(path).read_text(encoding="utf-8"))


def write_json(path: Path, data) -> None:
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    tmp = path.with_suffix(path.suffix + ".tmp")
    tmp.write_text(json.dumps(data, ensure_ascii=False, indent=1) + "\n", encoding="utf-8")
    os.replace(tmp, path)


def write_text(path: Path, text: str) -> None:
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(text, encoding="utf-8")


def clamp(v: float, lo: float, hi: float) -> float:
    return lo if v < lo else hi if v > hi else v
