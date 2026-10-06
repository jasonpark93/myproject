import copy
import json
import shutil
from pathlib import Path

import pytest

from studio import config

FIXTURES = Path(__file__).parent / "fixtures"
HAS_FFMPEG = shutil.which("ffmpeg") is not None and shutil.which("ffprobe") is not None
needs_ffmpeg = pytest.mark.skipif(not HAS_FFMPEG, reason="ffmpeg가 필요합니다")


@pytest.fixture
def cfg():
    """사용자가 channel.json을 바꿔도 테스트가 깨지지 않도록 고정된 설정을 쓴다."""
    raw = copy.deepcopy(config.load(FIXTURES / "channel.json"))
    raw["voice"]["provider"] = "silent"
    raw["broll"] = "none"
    return config.validate(raw)


def _load(name):
    return json.loads((FIXTURES / name).read_text(encoding="utf-8"))


@pytest.fixture
def narrated():
    return _load("narrated.json")


@pytest.fixture
def listed():
    return _load("list.json")


@pytest.fixture
def short_narrated(narrated):
    """렌더 테스트용으로 장면을 줄인 에피소드."""
    ep = copy.deepcopy(narrated)
    ep["script"]["scenes"] = ep["script"]["scenes"][:3]
    return ep
