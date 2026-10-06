"""문장 → 음성.

- google: Google Cloud Text-to-Speech (Chirp 3 HD 한국어 음성, 매달 100만 자까지 무료)
- elevenlabs: 더 자연스러운 음성이 필요할 때 (유료 플랜)
- silent: API 키 없이 화면만 미리 볼 때 쓰는 무음

API 키는 URL이 아니라 헤더로 보내서 오류 메시지에 남지 않게 한다.
"""

from __future__ import annotations

import base64
import hashlib
import io
import json
import time
import urllib.error
import urllib.request
import wave
from pathlib import Path
from typing import Callable

from . import textutil

GOOGLE_URL = "https://texttospeech.googleapis.com/v1"
ELEVEN_URL = "https://api.elevenlabs.io/v1"


class TTSError(RuntimeError):
    pass


def _request(opener: Callable, request: urllib.request.Request, what: str, retries: int = 3, sleep=time.sleep) -> bytes:
    for attempt in range(1, retries + 1):
        try:
            with opener(request, timeout=60) as response:
                return response.read()
        except urllib.error.HTTPError as e:
            detail = e.read()[:300].decode("utf-8", "replace")
            if e.code in (429, 500, 502, 503, 504) and attempt < retries:
                sleep(2**attempt)
                continue
            raise TTSError(f"{what}: HTTP {e.code} {detail}") from None
        except (urllib.error.URLError, TimeoutError, ConnectionError) as e:
            if attempt < retries:
                sleep(2**attempt)
                continue
            raise TTSError(f"{what}: 연결 실패 ({getattr(e, 'reason', e)})") from None
    raise AssertionError("unreachable")


class GoogleTTS:
    name = "google"

    def __init__(self, api_key: str, voice: str, *, opener: Callable = urllib.request.urlopen, sleep=time.sleep):
        if not api_key:
            raise TTSError("GOOGLE_TTS_API_KEY가 없습니다. shorts/README.md의 'Google Cloud 설정'을 참고하세요.")
        self._key = api_key
        self.voice = voice
        self._open = opener
        self._sleep = sleep

    @property
    def cache_id(self) -> str:
        return f"google:{self.voice}"

    def synthesize(self, text: str) -> tuple[bytes, str]:
        body = {
            "input": {"text": text},
            "voice": {"languageCode": "ko-KR", "name": self.voice},
            "audioConfig": {"audioEncoding": "LINEAR16", "sampleRateHertz": 24000},
        }
        request = urllib.request.Request(
            f"{GOOGLE_URL}/text:synthesize",
            data=json.dumps(body).encode("utf-8"),
            headers={"Content-Type": "application/json; charset=utf-8", "X-Goog-Api-Key": self._key},
            method="POST",
        )
        payload = json.loads(_request(self._open, request, "Google TTS", sleep=self._sleep))
        audio = base64.b64decode(payload.get("audioContent", ""))
        if not audio:
            raise TTSError("Google TTS: 빈 음성이 돌아왔습니다.")
        return audio, "wav"

    def voices(self) -> list[dict]:
        request = urllib.request.Request(
            f"{GOOGLE_URL}/voices?languageCode=ko-KR", headers={"X-Goog-Api-Key": self._key}
        )
        return json.loads(_request(self._open, request, "Google TTS 목소리 목록", sleep=self._sleep)).get("voices", [])


class ElevenLabsTTS:
    name = "elevenlabs"

    def __init__(self, api_key: str, voice_id: str, model: str, *, opener: Callable = urllib.request.urlopen, sleep=time.sleep):
        if not api_key:
            raise TTSError("ELEVENLABS_API_KEY가 없습니다.")
        self._key = api_key
        self.voice_id = voice_id
        self.model = model
        self._open = opener
        self._sleep = sleep

    @property
    def cache_id(self) -> str:
        return f"elevenlabs:{self.voice_id}:{self.model}"

    def synthesize(self, text: str) -> tuple[bytes, str]:
        request = urllib.request.Request(
            f"{ELEVEN_URL}/text-to-speech/{self.voice_id}?output_format=mp3_44100_128",
            data=json.dumps({"text": text, "model_id": self.model}).encode("utf-8"),
            headers={"Content-Type": "application/json", "Accept": "audio/mpeg", "xi-api-key": self._key},
            method="POST",
        )
        return _request(self._open, request, "ElevenLabs", sleep=self._sleep), "mp3"


class SilentTTS:
    """무음. 실제 낭독 속도와 비슷한 길이를 만들어 자막 타이밍을 미리 확인할 수 있게 한다."""

    name = "silent"
    cache_id = "silent"

    def synthesize(self, text: str) -> tuple[bytes, str]:
        seconds = max(0.6, textutil.spoken_length(text) / 7.0)
        rate = 24000
        buffer = io.BytesIO()
        with wave.open(buffer, "wb") as out:
            out.setnchannels(1)
            out.setsampwidth(2)
            out.setframerate(rate)
            out.writeframes(b"\x00\x00" * int(seconds * rate))
        return buffer.getvalue(), "wav"


class Cached:
    """같은 문장·같은 목소리는 다시 합성하지 않는다 (비용·시간 절약)."""

    def __init__(self, inner, cache_dir: Path):
        self.inner = inner
        self.dir = Path(cache_dir)
        self.hits = 0

    @property
    def name(self) -> str:
        return self.inner.name

    def synthesize(self, text: str) -> tuple[bytes, str]:
        key = hashlib.sha256(f"{self.inner.cache_id}\n{text}".encode("utf-8")).hexdigest()[:24]
        for ext in ("wav", "mp3"):
            path = self.dir / f"{key}.{ext}"
            if path.exists():
                self.hits += 1
                return path.read_bytes(), ext
        audio, ext = self.inner.synthesize(text)
        self.dir.mkdir(parents=True, exist_ok=True)
        (self.dir / f"{key}.{ext}").write_bytes(audio)
        return audio, ext


def from_config(cfg: dict, env: dict, cache_dir: Path | None = None):
    voice = cfg["voice"]
    provider = voice["provider"]
    if provider == "google":
        engine = GoogleTTS(env.get("GOOGLE_TTS_API_KEY", ""), voice["google_voice"])
    elif provider == "elevenlabs":
        engine = ElevenLabsTTS(env.get("ELEVENLABS_API_KEY", ""), voice["elevenlabs_voice_id"], voice["elevenlabs_model"])
    else:
        engine = SilentTTS()
    return Cached(engine, cache_dir) if cache_dir and provider != "silent" else engine
