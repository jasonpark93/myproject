import io
import json
import urllib.error

from app import notify


class FakeResponse:
    def __init__(self, payload):
        self.payload = payload

    def read(self):
        return json.dumps(self.payload).encode()

    def __enter__(self):
        return self

    def __exit__(self, *exc):
        return False


def test_message_format(cfg, db, today):
    text = notify.message(cfg, db["notices"]["apt/2099000001"], today)
    assert text.splitlines() == [
        "🏠 <b>[서울] 샘플 예시아파트 1단지</b>",
        "APT 분양 · 총 1,234세대 · 민영",
        "📅 특별공급 10/5(월) · 1순위 해당지역 10/6(화) · 당첨자 발표 10/14(수)",
        "💰 최고 분양가 9억 8,000만원 ~ 18억 2,000만원",
        "👉 https://example.github.io/myproject/apt/2099000001/",
    ]


def test_message_escapes_html(cfg, db, today):
    notice = {**db["notices"]["apt/2099000001"], "name": "A&B <단지>"}
    assert "<b>[서울] A&amp;B &lt;단지&gt;</b>" in notify.message(cfg, notice, today)


def test_send_all_counts_and_hides_token():
    requests, sleeps = [], []

    def opener(request, timeout):
        requests.append(json.loads(request.data))
        if len(requests) == 2:
            raise urllib.error.HTTPError(request.full_url, 400, "Bad", {}, io.BytesIO(b'{"ok":false,"description":"chat not found"}'))
        return FakeResponse({"ok": True})

    sent, errors = notify.send_all("123:SECRET", "@channel", ["a", "b", "c"], opener=opener, sleep=sleeps.append)
    assert sent == 2
    assert errors == ["HTTP 400 chat not found"]
    assert all("SECRET" not in e for e in errors)
    assert [r["chat_id"] for r in requests] == ["@channel"] * 3
    assert requests[0]["parse_mode"] == "HTML"
    assert sleeps == [notify.SEND_INTERVAL] * 2


def test_send_all_caps_messages():
    sent, errors = notify.send_all(
        "t", "c", ["m"] * (notify.MAX_MESSAGES + 5), opener=lambda r, timeout: FakeResponse({"ok": True}), sleep=lambda s: None
    )
    assert sent == notify.MAX_MESSAGES
    assert errors == ["5건은 한도(15건)를 넘어 보내지 않았습니다."]
