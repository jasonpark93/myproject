"""외부 서비스(TTS, Pexels, YouTube) 클라이언트를 가짜 HTTP로 검증한다."""

import base64
import io
import json
import urllib.error
import wave
from datetime import datetime, timezone

import pytest

from studio import broll, tts, youtube


class Response:
    def __init__(self, body=b"", headers=None):
        self.body = body if isinstance(body, bytes) else json.dumps(body).encode()
        self.headers = headers or {}

    def read(self):
        return self.body

    def __enter__(self):
        return self

    def __exit__(self, *exc):
        return False


def recorder(responses):
    calls = []

    def opener(request, timeout):
        calls.append(request)
        item = responses.pop(0)
        if isinstance(item, Exception):
            raise item
        return item

    return opener, calls


def http_error(code, body=b"{}"):
    return urllib.error.HTTPError("https://example/?key=SECRET", code, "err", {}, io.BytesIO(body))


def _wav_bytes(seconds=0.5, rate=24000):
    buf = io.BytesIO()
    with wave.open(buf, "wb") as w:
        w.setnchannels(1)
        w.setsampwidth(2)
        w.setframerate(rate)
        w.writeframes(b"\x00\x00" * int(seconds * rate))
    return buf.getvalue()


# ---------- TTS ----------
def test_google_tts_request():
    audio = _wav_bytes()
    opener, calls = recorder([Response({"audioContent": base64.b64encode(audio).decode()})])
    engine = tts.GoogleTTS("SECRET", "ko-KR-Chirp3-HD-Charon", opener=opener)
    data, ext = engine.synthesize("안녕하세요")
    assert (data, ext) == (audio, "wav")
    req = calls[0]
    assert req.full_url == "https://texttospeech.googleapis.com/v1/text:synthesize"
    assert "SECRET" not in req.full_url and req.get_header("X-goog-api-key") == "SECRET"
    body = json.loads(req.data)
    assert body["voice"] == {"languageCode": "ko-KR", "name": "ko-KR-Chirp3-HD-Charon"}
    assert body["audioConfig"]["audioEncoding"] == "LINEAR16"


def test_google_tts_retries_then_reports_without_key():
    sleeps = []
    opener, _ = recorder([http_error(503), http_error(403, b'{"error": "API key not valid"}')])
    engine = tts.GoogleTTS("SECRET", "v", opener=opener, sleep=sleeps.append)
    with pytest.raises(tts.TTSError) as info:
        engine.synthesize("문장")
    assert "403" in str(info.value) and "SECRET" not in str(info.value)
    assert sleeps == [2]


def test_elevenlabs_request():
    opener, calls = recorder([Response(b"ID3mp3")])
    data, ext = tts.ElevenLabsTTS("KEY", "voice123", "eleven_multilingual_v2", opener=opener).synthesize("문장")
    assert (data, ext) == (b"ID3mp3", "mp3")
    assert calls[0].full_url.startswith("https://api.elevenlabs.io/v1/text-to-speech/voice123?")
    assert calls[0].get_header("Xi-api-key") == "KEY"


def test_silent_tts_length():
    data, ext = tts.SilentTTS().synthesize("가" * 70)
    with wave.open(io.BytesIO(data)) as w:
        assert abs(w.getnframes() / w.getframerate() - 10.0) < 0.01


def test_cache_avoids_second_call(tmp_path):
    class Counting:
        name, cache_id, calls = "google", "google:v", 0

        def synthesize(self, text):
            self.calls += 1
            return b"RIFFdata", "wav"

    inner = Counting()
    cached = tts.Cached(inner, tmp_path)
    assert cached.synthesize("같은 문장") == cached.synthesize("같은 문장") == (b"RIFFdata", "wav")
    assert inner.calls == 1 and cached.hits == 1


def test_missing_keys_are_explained():
    with pytest.raises(tts.TTSError, match="GOOGLE_TTS_API_KEY"):
        tts.GoogleTTS("", "v")


# ---------- Pexels ----------
def _video(vid, duration, files):
    return {"id": vid, "duration": duration, "video_files": files}


def _file(fid, w, h, kind="video/mp4"):
    return {"id": fid, "width": w, "height": h, "file_type": kind, "link": f"https://videos.example/{fid}.mp4"}


def test_pick_file_prefers_portrait_near_1920():
    files = [_file(1, 1920, 1080), _file(2, 720, 1280), _file(3, 1080, 1920), _file(4, 2160, 3840)]
    assert broll.pick_file(files)["id"] == 3
    assert broll.pick_file([_file(1, 1920, 1080)]) is None


def test_pexels_find_downloads_and_caches(tmp_path):
    search = {"videos": [_video(10, 3, [_file(1, 1080, 1920)]), _video(11, 15, [_file(2, 1080, 1920)])]}
    opener, calls = recorder([Response(search), Response(b"MP4DATA")])
    pexels = broll.Pexels("KEY", tmp_path, opener=opener)
    video_id, path = pexels.find("apartment", seconds=6, seed="ep", exclude=set())
    assert video_id == 11  # 장면보다 짧은 영상은 뒤로
    assert path.read_bytes() == b"MP4DATA"
    assert calls[0].get_header("Authorization") == "KEY" and "orientation=portrait" in calls[0].full_url

    opener2, calls2 = recorder([Response(search)])
    again = broll.Pexels("KEY", tmp_path, opener=opener2).find("apartment", seconds=6, seed="ep", exclude=set())
    assert again == (11, path) and len(calls2) == 1  # 이미 받은 파일은 다시 받지 않는다


def test_pexels_excludes_used_and_handles_empty(tmp_path):
    search = {"videos": [_video(10, 30, [_file(1, 1080, 1920)])]}
    opener, _ = recorder([Response(search)])
    assert broll.Pexels("KEY", tmp_path, opener=opener).find("x", seconds=5, seed="s", exclude={10}) is None


# ---------- YouTube ----------
def test_build_metadata_schedules_future(cfg, narrated):
    ep = dict(narrated, publish_at="2026-10-13T19:00:00+09:00")
    meta = youtube.build_metadata(ep, cfg, "설명", now=datetime(2026, 10, 12, tzinfo=timezone.utc))
    assert meta["status"] == {
        "privacyStatus": "private",
        "selfDeclaredMadeForKids": False,
        "containsSyntheticMedia": False,
        "publishAt": "2026-10-13T10:00:00Z",
    }
    assert meta["snippet"]["tags"] == ["청약", "청약가점", "내집마련"]
    assert meta["snippet"]["categoryId"] == "27" and meta["snippet"]["defaultAudioLanguage"] == "ko"
    late = youtube.build_metadata(ep, cfg, "설명", now=datetime(2026, 10, 13, 9, 50, tzinfo=timezone.utc))
    assert "publishAt" not in late["status"]  # 시각이 거의 지났으면 비공개로만 올린다


def test_upload_flow(tmp_path):
    video = tmp_path / "v.mp4"
    video.write_bytes(b"0" * 1000)
    opener, calls = recorder(
        [
            Response({"access_token": "TOKEN"}),
            Response(b"", headers={"Location": "https://upload.example/session"}),
            Response({"id": "abc123"}),
        ]
    )
    yt = youtube.YouTube("cid", "secret", "refresh", opener=opener)
    assert yt.upload(video, {"snippet": {}, "status": {}}) == "abc123"
    token, start, put = calls
    assert b"grant_type=refresh_token" in token.data
    assert start.get_header("Authorization") == "Bearer TOKEN" and start.get_header("X-upload-content-length") == "1000"
    assert put.full_url == "https://upload.example/session" and put.get_method() == "PUT" and put.data == b"0" * 1000


def test_channel_stats():
    opener, calls = recorder(
        [
            Response({"items": [{"snippet": {"title": "돈한입"}, "statistics": {"subscriberCount": "120", "viewCount": "5000"}, "contentDetails": {"relatedPlaylists": {"uploads": "UU1"}}}]}),
            Response({"items": [{"contentDetails": {"videoId": "v1"}}, {"contentDetails": {"videoId": "v2"}}]}),
            Response(
                {
                    "items": [
                        {"id": "v1", "snippet": {"title": "A", "publishedAt": "2026-10-10T10:00:00Z"}, "statistics": {"viewCount": "300", "likeCount": "5"}},
                        {"id": "v2", "snippet": {"title": "B", "publishedAt": "2026-10-11T10:00:00Z"}, "statistics": {"viewCount": "900"}},
                    ]
                }
            ),
        ]
    )
    stats = youtube.channel_stats("KEY", "UC1", opener=opener)
    assert stats["subscribers"] == 120 and [v["views"] for v in stats["videos"]] == [300, 900]
    assert all(c.get_header("X-goog-api-key") == "KEY" for c in calls)
