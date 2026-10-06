import io
import json
import urllib.error
from urllib.parse import parse_qs, urlsplit

import pytest

from app.applyhome import ApiError, Client


class FakeResponse:
    def __init__(self, body: bytes):
        self.body = body

    def read(self):
        return self.body

    def __enter__(self):
        return self

    def __exit__(self, *exc):
        return False


def make_opener(responses, seen):
    def opener(request, timeout):
        seen.append(request.full_url)
        item = responses.pop(0)
        if isinstance(item, Exception):
            raise item
        return FakeResponse(item if isinstance(item, bytes) else json.dumps(item).encode())

    return opener


def http_error(code, body):
    return urllib.error.HTTPError("https://api.example/x?serviceKey=SECRET", code, "error", {}, io.BytesIO(body.encode()))


def test_paginates_with_match_count():
    seen = []
    pages = [
        {"data": [{"n": 1}, {"n": 2}], "matchCount": 3, "totalCount": 99, "page": 1, "perPage": 2},
        {"data": [{"n": 3}], "matchCount": 3, "totalCount": 99, "page": 2, "perPage": 2},
    ]
    client = Client("KEY", opener=make_opener(pages, seen), per_page=2)
    rows = list(client.iter_records("getAPTLttotPblancDetail", cond={"RCRIT_PBLANC_DE::GTE": "2026-01-01"}))
    assert [r["n"] for r in rows] == [1, 2, 3]
    first = parse_qs(urlsplit(seen[0]).query)
    assert first["page"] == ["1"] and first["perPage"] == ["2"]
    assert first["cond[RCRIT_PBLANC_DE::GTE]"] == ["2026-01-01"]
    assert parse_qs(urlsplit(seen[1]).query)["page"] == ["2"]


@pytest.mark.parametrize("key", ["ab+cd/ef==", "ab%2Bcd%2Fef%3D%3D"])
def test_encoding_and_decoding_keys_are_sent_identically(key):
    seen = []
    client = Client(key, opener=make_opener([{"data": [], "totalCount": 0}], seen))
    list(client.iter_records("op"))
    assert "serviceKey=ab%2Bcd%2Fef%3D%3D" in seen[0]


def test_empty_key_is_rejected():
    with pytest.raises(ApiError) as info:
        Client("  ")
    assert info.value.auth


def test_auth_failure_does_not_leak_key():
    client = Client("SECRET", opener=make_opener([http_error(401, '{"code": -4, "msg": "등록되지 않은 인증키 입니다."}')], []))
    with pytest.raises(ApiError) as info:
        client.get("op", {})
    assert info.value.auth and info.value.status == 401
    assert "등록되지 않은 인증키" in str(info.value)
    assert "SECRET" not in str(info.value)


def test_server_error_is_retried():
    sleeps = []
    responses = [http_error(500, "oops"), {"data": [{"n": 1}], "totalCount": 1}]
    client = Client("KEY", opener=make_opener(responses, []), sleep=sleeps.append)
    assert client.get("op", {})["data"] == [{"n": 1}]
    assert sleeps == [2]
    assert client.calls == 2


def test_bad_request_is_not_retried():
    client = Client("KEY", opener=make_opener([http_error(400, '{"msg": "bad cond"}')], []), sleep=lambda s: None)
    with pytest.raises(ApiError) as info:
        client.get("op", {})
    assert info.value.status == 400 and not info.value.auth
    assert client.calls == 1


def test_connection_errors_give_up_after_retries():
    errors = [urllib.error.URLError("timed out") for _ in range(3)]
    client = Client("KEY", opener=make_opener(errors, []), sleep=lambda s: None)
    with pytest.raises(ApiError, match="연결 실패"):
        client.get("op", {})
    assert client.calls == 3


def test_xml_service_key_error_is_auth_error():
    body = b"<OpenAPI_ServiceResponse><returnAuthMsg>SERVICE_KEY_IS_NOT_REGISTERED_ERROR</returnAuthMsg></OpenAPI_ServiceResponse>"
    client = Client("KEY", opener=make_opener([body], []))
    with pytest.raises(ApiError) as info:
        client.get("op", {})
    assert info.value.auth
