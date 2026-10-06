"""한국부동산원 '청약홈 분양정보 조회 서비스' (공공데이터포털, api.odcloud.kr) 클라이언트.

인증키는 data.go.kr 마이페이지의 '일반 인증키'를 그대로 쓴다. 'Encoding'/'Decoding' 어느 쪽을 넣어도 된다.
요청 URL에는 인증키가 들어가므로 로그나 예외 메시지에 URL을 절대 남기지 않는다.
"""

from __future__ import annotations

import json
import time
import urllib.error
import urllib.parse
import urllib.request
from typing import Callable, Iterator

BASE_URL = "https://api.odcloud.kr/api/ApplyhomeInfoDetailSvc/v1"
USER_AGENT = "cheongyak-site/1.0"

# kind → (표시 이름, 공고 상세 오퍼레이션, 주택형별 오퍼레이션)
KINDS = {
    "apt": {
        "label": "APT 분양",
        "detail": "getAPTLttotPblancDetail",
        "model": "getAPTLttotPblancMdl",
    },
    "remndr": {
        "label": "무순위·잔여세대",
        "detail": "getRemndrLttotPblancDetail",
        "model": "getRemndrLttotPblancMdl",
    },
    "urbty": {
        "label": "오피스텔·도시형·민간임대",
        "detail": "getUrbtyOfctlLttotPblancDetail",
        "model": "getUrbtyOfctlLttotPblancMdl",
    },
}

MAX_PAGES = 200


class ApiError(RuntimeError):
    """API 호출 실패. auth=True면 인증키 문제라 재시도해도 소용없다."""

    def __init__(self, message: str, *, auth: bool = False, status: int | None = None):
        super().__init__(message)
        self.auth = auth
        self.status = status


class Client:
    def __init__(
        self,
        service_key: str,
        *,
        base_url: str = BASE_URL,
        per_page: int = 100,
        timeout: float = 30,
        retries: int = 3,
        opener: Callable = urllib.request.urlopen,
        sleep: Callable[[float], None] = time.sleep,
    ):
        key = (service_key or "").strip()
        if not key:
            raise ApiError(
                "공공데이터포털 인증키(DATA_GO_KR_API_KEY)가 없습니다. GitHub 저장소 Settings → Secrets and variables → "
                "Actions 에 DATA_GO_KR_API_KEY 를 추가하세요 (cheongyak/README.md 2단계).",
                auth=True,
            )
        if "%" in key:  # 'Encoding' 인증키를 넣은 경우 원래 값으로 되돌려 이중 인코딩을 막는다
            key = urllib.parse.unquote(key)
        self._key = key
        self.base_url = base_url.rstrip("/")
        self.per_page = per_page
        self.timeout = timeout
        self.retries = retries
        self._open = opener
        self._sleep = sleep
        self.calls = 0

    def get(self, operation: str, params: dict) -> dict:
        query = urllib.parse.urlencode({**params, "serviceKey": self._key})
        request = urllib.request.Request(
            f"{self.base_url}/{operation}?{query}",
            headers={"User-Agent": USER_AGENT, "Accept": "application/json"},
        )
        for attempt in range(1, self.retries + 1):
            self.calls += 1
            try:
                with self._open(request, timeout=self.timeout) as response:
                    body = response.read()
            except urllib.error.HTTPError as e:
                detail = _error_detail(e)
                if e.code in (401, 403):
                    raise ApiError(
                        f"{operation}: 인증 실패 (HTTP {e.code}) {detail} — 인증키와 활용신청 승인 상태를 확인하세요.",
                        auth=True,
                        status=e.code,
                    ) from None
                if e.code >= 500 and attempt < self.retries:
                    self._sleep(2**attempt)
                    continue
                raise ApiError(f"{operation}: HTTP {e.code} {detail}", status=e.code) from None
            except (urllib.error.URLError, TimeoutError, ConnectionError) as e:
                if attempt < self.retries:
                    self._sleep(2**attempt)
                    continue
                reason = getattr(e, "reason", e)
                raise ApiError(f"{operation}: 연결 실패 ({reason})") from None

            try:
                payload = json.loads(body)
            except ValueError:
                snippet = body[:200].decode("utf-8", "replace")
                auth = "SERVICE_KEY" in snippet or "인증" in snippet
                raise ApiError(f"{operation}: JSON이 아닌 응답 — {snippet}", auth=auth) from None
            if not isinstance(payload, dict) or "data" not in payload:
                raise ApiError(f"{operation}: 예상하지 못한 응답 형식 — {str(payload)[:200]}")
            return payload
        raise AssertionError("unreachable")

    def iter_records(self, operation: str, cond: dict | None = None, per_page: int | None = None) -> Iterator[dict]:
        """페이지를 넘기며 모든 레코드를 돌려준다. cond 예: {"RCRIT_PBLANC_DE::GTE": "2026-01-01"}"""
        per_page = per_page or self.per_page
        for page in range(1, MAX_PAGES + 1):
            params = {"page": page, "perPage": per_page}
            for key, value in (cond or {}).items():
                params[f"cond[{key}]"] = value
            payload = self.get(operation, params)
            rows = payload.get("data") or []
            yield from rows
            total = payload.get("matchCount")
            if total is None:
                total = payload.get("totalCount") or 0
            if not rows or page * per_page >= int(total):
                return


def _error_detail(error: urllib.error.HTTPError) -> str:
    try:
        body = error.read()[:300].decode("utf-8", "replace")
    except Exception:
        return ""
    try:
        data = json.loads(body)
        if isinstance(data, dict):
            return str(data.get("msg") or data.get("message") or data)
    except ValueError:
        pass
    return body.strip()
