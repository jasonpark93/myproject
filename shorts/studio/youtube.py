"""YouTube Data API: 예약 업로드(OAuth)와 채널 통계(API 키).

주의: Google Cloud 프로젝트가 YouTube API 감사(audit)를 통과하기 전에는 API로 올린 영상이
'비공개'로 잠깁니다. 감사 전에는 upload를 "manual"로 두고 직접 올리세요 (README 참고).
"""

from __future__ import annotations

import json
import os
import urllib.error
import urllib.parse
import urllib.request
from datetime import datetime, timedelta, timezone
from pathlib import Path
from typing import Callable

TOKEN_URL = "https://oauth2.googleapis.com/token"
AUTH_URL = "https://accounts.google.com/o/oauth2/v2/auth"
UPLOAD_URL = "https://www.googleapis.com/upload/youtube/v3/videos?uploadType=resumable&part=snippet,status"
API_URL = "https://www.googleapis.com/youtube/v3"
SCOPES = "https://www.googleapis.com/auth/youtube.upload https://www.googleapis.com/auth/youtube.readonly"


class YouTubeError(RuntimeError):
    pass


def _read(opener: Callable, request: urllib.request.Request, what: str):
    try:
        with opener(request, timeout=300) as response:
            return response.read(), response.headers
    except urllib.error.HTTPError as e:
        detail = e.read()[:400].decode("utf-8", "replace")
        raise YouTubeError(f"{what}: HTTP {e.code} {detail}") from None
    except (urllib.error.URLError, TimeoutError, ConnectionError) as e:
        raise YouTubeError(f"{what}: 연결 실패 ({getattr(e, 'reason', e)})") from None


def build_metadata(episode: dict, cfg: dict, description: str, *, now: datetime | None = None) -> dict:
    """업로드 정보. 공개 시각이 아직 남았으면 '비공개 + 예약 공개', 이미 지났으면 비공개로만 올린다."""
    now = now or datetime.now(timezone.utc)
    script = episode["script"]
    yt = cfg["youtube"]
    tags = [t.lstrip("#") for t in script.get("hashtags") or []]
    status = {
        "privacyStatus": "private",
        "selfDeclaredMadeForKids": bool(yt["made_for_kids"]),
        "containsSyntheticMedia": bool(yt["contains_synthetic_media"]),
    }
    when = datetime.fromisoformat(episode["publish_at"]).astimezone(timezone.utc)
    if when - now > timedelta(minutes=15):
        status["publishAt"] = when.strftime("%Y-%m-%dT%H:%M:%SZ")
    return {
        "snippet": {
            "title": script["title"][:100],
            "description": description[:4900],
            "tags": tags[:15],
            "categoryId": str(yt["category_id"]),
            "defaultLanguage": yt["default_language"],
            "defaultAudioLanguage": yt["default_language"],
        },
        "status": status,
    }


class YouTube:
    def __init__(self, client_id: str, client_secret: str, refresh_token: str, *, opener: Callable = urllib.request.urlopen):
        if not (client_id and client_secret and refresh_token):
            raise YouTubeError("YOUTUBE_CLIENT_ID / YOUTUBE_CLIENT_SECRET / YOUTUBE_REFRESH_TOKEN이 필요합니다.")
        self._id, self._secret, self._refresh = client_id, client_secret, refresh_token
        self._open = opener
        self._token: str | None = None

    def access_token(self) -> str:
        if self._token:
            return self._token
        body = urllib.parse.urlencode(
            {"client_id": self._id, "client_secret": self._secret, "refresh_token": self._refresh, "grant_type": "refresh_token"}
        ).encode()
        raw, _ = _read(self._open, urllib.request.Request(TOKEN_URL, data=body, method="POST"), "토큰 갱신")
        token = json.loads(raw).get("access_token")
        if not token:
            raise YouTubeError("토큰 갱신 응답에 access_token이 없습니다.")
        self._token = token
        return token

    def upload(self, video: Path, metadata: dict) -> str:
        video = Path(video)
        size = video.stat().st_size
        start = urllib.request.Request(
            UPLOAD_URL,
            data=json.dumps(metadata).encode("utf-8"),
            headers={
                "Authorization": f"Bearer {self.access_token()}",
                "Content-Type": "application/json; charset=UTF-8",
                "X-Upload-Content-Type": "video/mp4",
                "X-Upload-Content-Length": str(size),
            },
            method="POST",
        )
        _, headers = _read(self._open, start, "업로드 시작")
        location = headers.get("Location")
        if not location:
            raise YouTubeError("업로드 주소(Location)를 받지 못했습니다.")
        put = urllib.request.Request(
            location, data=video.read_bytes(), headers={"Content-Type": "video/mp4", "Content-Length": str(size)}, method="PUT"
        )
        raw, _ = _read(self._open, put, "영상 전송")
        video_id = json.loads(raw).get("id")
        if not video_id:
            raise YouTubeError("업로드는 끝났지만 영상 ID가 없습니다.")
        return video_id


def channel_stats(api_key: str, channel_id: str, *, opener: Callable = urllib.request.urlopen, limit: int = 50) -> dict:
    """공개 통계(조회수·좋아요·댓글)와 최근 업로드 목록. API 키만 있으면 된다."""
    if not (api_key and channel_id):
        raise YouTubeError("YOUTUBE_API_KEY와 YOUTUBE_CHANNEL_ID가 필요합니다.")

    def get(path: str, **params) -> dict:
        query = urllib.parse.urlencode(params)
        request = urllib.request.Request(f"{API_URL}/{path}?{query}", headers={"X-Goog-Api-Key": api_key})
        raw, _ = _read(opener, request, f"YouTube {path}")
        return json.loads(raw)

    channel = get("channels", part="statistics,contentDetails,snippet", id=channel_id).get("items") or []
    if not channel:
        raise YouTubeError(f"채널을 찾지 못했습니다: {channel_id}")
    uploads = channel[0]["contentDetails"]["relatedPlaylists"]["uploads"]
    items = get("playlistItems", part="contentDetails", playlistId=uploads, maxResults=min(limit, 50)).get("items") or []
    ids = [i["contentDetails"]["videoId"] for i in items]
    videos = []
    if ids:
        for v in get("videos", part="statistics,snippet,contentDetails", id=",".join(ids)).get("items") or []:
            stats = v.get("statistics", {})
            videos.append(
                {
                    "id": v["id"],
                    "title": v["snippet"]["title"],
                    "published_at": v["snippet"]["publishedAt"],
                    "views": int(stats.get("viewCount", 0)),
                    "likes": int(stats.get("likeCount", 0)),
                    "comments": int(stats.get("commentCount", 0)),
                    "duration": v.get("contentDetails", {}).get("duration", ""),
                }
            )
    s = channel[0].get("statistics", {})
    return {
        "title": channel[0]["snippet"]["title"],
        "subscribers": int(s.get("subscriberCount", 0)),
        "total_views": int(s.get("viewCount", 0)),
        "videos": videos,
    }


def authorize(client_id: str, client_secret: str, port: int = 8765, *, open_browser: bool = True) -> str:
    """내 컴퓨터에서 한 번 실행해 refresh token을 받는다 (python -m studio auth)."""
    import http.server
    import secrets
    import webbrowser

    state = secrets.token_urlsafe(16)
    redirect = f"http://127.0.0.1:{port}"
    url = AUTH_URL + "?" + urllib.parse.urlencode(
        {
            "client_id": client_id,
            "redirect_uri": redirect,
            "response_type": "code",
            "scope": SCOPES,
            "access_type": "offline",
            "prompt": "consent",
            "state": state,
        }
    )
    result: dict = {}

    class Handler(http.server.BaseHTTPRequestHandler):
        def do_GET(self):  # noqa: N802
            query = urllib.parse.parse_qs(urllib.parse.urlparse(self.path).query)
            result.update({k: v[0] for k, v in query.items()})
            self.send_response(200)
            self.send_header("Content-Type", "text/html; charset=utf-8")
            self.end_headers()
            self.wfile.write("인증이 끝났습니다. 이 창을 닫고 터미널로 돌아가세요.".encode("utf-8"))

        def log_message(self, *args):
            pass

    print("아래 주소를 브라우저에서 열고, 쇼츠 채널을 소유한 구글 계정으로 허용하세요:\n" + url)
    if open_browser and not os.environ.get("CI"):
        webbrowser.open(url)
    with http.server.HTTPServer(("127.0.0.1", port), Handler) as server:
        while "code" not in result and "error" not in result:
            server.handle_request()
    if result.get("state") != state or "code" not in result:
        raise YouTubeError(f"인증 실패: {result.get('error', 'state 불일치')}")
    body = urllib.parse.urlencode(
        {"code": result["code"], "client_id": client_id, "client_secret": client_secret, "redirect_uri": redirect, "grant_type": "authorization_code"}
    ).encode()
    raw, _ = _read(urllib.request.urlopen, urllib.request.Request(TOKEN_URL, data=body, method="POST"), "토큰 발급")
    token = json.loads(raw).get("refresh_token")
    if not token:
        raise YouTubeError("refresh_token이 없습니다. OAuth 동의 화면을 '프로덕션'으로 바꾸고 다시 시도하세요.")
    return token
