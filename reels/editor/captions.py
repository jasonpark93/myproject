"""자막: 의미 단위로 한 줄(기본 12자 이내)씩 끊고, 숫자·키워드·결과 단어를 강조한다."""

from __future__ import annotations

import re

from .transcript import norm

GOOD_BREAK = re.compile(
    r"(은|는|이|가|을|를|에|에서|에게|께|로|으로|와|과|랑|하고|고|서|면|며|데|지만|니까|는데|도|만|요|다|죠|게|해서|하면|든|면서|려면|으면|라서|까지|부터|처럼|보다|인데|던|고요|구요|거든요)$"
)
UNIT_START = re.compile(r"^(원|만|억|천|명|개|번|배|살|세|년|개월|달|주|일|시간|분|초|%|퍼센트|위|등|평|층|점|가지|단계|회|건|곳)")
NUM_HL = re.compile(
    r"(?:약\s?)?\d[\d,.]*\s?(?:천만|백만|십만|만|억|천|백)?\s?"
    r"(?:만원|억원|원|명|개|번|배|살|세|년|개월|달|주|일|시간|분|초|%|퍼센트|순위|위|등급|등|평|층|점|가지|단계|회|건|곳)?"
)
KNUM_HL = re.compile(r"(?:한|두|세|네|다섯|여섯|일곱|여덟|아홉|열|스무)\s?(?:가지|배|번|개|명|달|해|살|군데|곳|단계)")
RESULT_WORDS = ("당첨", "합격", "성공", "무료", "공짜", "절약", "손해", "이득", "대박", "꿀팁", "비밀", "주의", "금지", "필수", "정답", "결과", "무조건", "반드시", "절대")
# 앞말에 붙어야 뜻이 통하는 말 (앞에서 끊지 않는다): '깨는 / 대신' ✗
DEPENDENT = ("대신", "때", "것", "수", "줄", "만큼", "동안", "정도", "이상", "이하", "중", "후", "뒤", "덕분", "때문")


def display(text: str) -> str:
    """화면용: 마침표·쉼표·말줄임 제거 (물음표·느낌표는 남김)."""
    text = re.sub(r"(\.{2,}|…)", " ", text)
    text = re.sub(r"(?<!\d)[.,]|[.,](?!\d)|[。、]", "", text)  # 9.5억·2,574만의 점·쉼표는 남긴다
    return re.sub(r"\s+", " ", text).strip()


def visible_len(text: str) -> int:
    return len(re.sub(r"\s", "", text))


def chunk(tokens: list, max_chars: int = 12) -> list:
    """tokens: [(원문, 화면용)] → [(시작, 끝)] (끝 포함). 길이와 끊는 자리를 함께 따져 가장 자연스러운 조합을 고른다."""
    n = len(tokens)
    if n == 0:
        return []
    lens = [visible_len(t[1]) for t in tokens]
    target = max(4.0, max_chars * 0.75)

    def break_cost(i: int) -> float:  # tokens[i] 뒤에서 끊는 비용
        if i >= n - 1:
            return 0.0
        raw = tokens[i][0].rstrip()
        cost = 0.0 if raw.endswith((",", "?", "!", ".")) or GOOD_BREAK.search(norm(raw) or raw) else 1.2
        if re.search(r"\d[\d,.]*(천|만|억|백|십)*$", tokens[i][1]) and UNIT_START.match(tokens[i + 1][1]):
            cost += 4.0  # 숫자와 단위 사이는 끊지 않는다
        nxt = norm(tokens[i + 1][1])
        if nxt.startswith(DEPENDENT) or nxt in ("새", "만에"):
            cost += 2.5
        if re.match(r"\d", tokens[i + 1][1]) and not raw.endswith((",", ".", "?", "!")):
            cost += 2.0  # 숫자 바로 앞에서 끊지 않는다: 월 2만 원, 분양가 9.5억, 최대 120만 원
        return cost

    best = [0.0] + [float("inf")] * n
    back = [0] * (n + 1)
    for i in range(1, n + 1):
        for j in range(i - 1, -1, -1):
            length = sum(lens[j:i])
            if length > max_chars + 2 and i - j > 1:  # 12자는 기준, 의미 단위를 지키려면 2자까지 넘칠 수 있다
                break
            cost = best[j] + 0.5 + 0.06 * (length - target) ** 2 + break_cost(i - 1)
            cost += 0.8 * max(0, length - max_chars)
            # 쉼표·문장 끝을 줄 가운데에 끼우지 않는다 (의미 단위가 섞임)
            cost += 1.0 * sum(1 for k in range(j, i - 1) if tokens[k][0].rstrip().endswith((",", ".", "?", "!")))
            if length <= 2 and n > 1:
                cost += 2.0
            if cost < best[i]:
                best[i], back[i] = cost, j
    out, i = [], n
    while i > 0:
        out.append((back[i], i - 1))
        i = back[i]
    return out[::-1]


def find_highlights(text: str, keywords=()) -> list:
    """자막 한 줄에서 강조할 부분 (최대 1개): 키워드 > 숫자 > 결과 단어."""
    for kw in sorted((k for k in keywords if k), key=len, reverse=True):
        if kw in text:
            return [kw]
    for pattern in (NUM_HL, KNUM_HL):
        for m in pattern.finditer(text):
            span = m.group(0).strip()
            if re.search(r"\d|한|두|세|네|다섯|여섯|일곱|여덟|아홉|열|스무", span) and visible_len(span) >= 1:
                return [span]
    for word in RESULT_WORDS:
        if word in text:
            return [word]
    return []


def runs(text: str, highlights: list) -> list:
    """[(글자, 강조여부)] — 강조 부분을 색으로 나눠 그리기 위한 조각."""
    spans = []
    for hl in highlights:
        start = text.find(hl)
        if start >= 0 and hl:
            spans.append((start, start + len(hl)))
    spans.sort()
    out, pos = [], 0
    for a, b in spans:
        if a < pos:
            continue
        if a > pos:
            out.append((text[pos:a], False))
        out.append((text[a:b], True))
        pos = b
    if pos < len(text):
        out.append((text[pos:], False))
    return [r for r in out if r[0]]


def chunk_text(words: list, a: int, b: int, script: str | None) -> str:
    """자막 글자: 대본이 있으면 대본의 띄어쓰기 그대로, 없으면 인식 결과를 이어서."""
    kept = [w for w in words[a : b + 1] if not w.get("cut")]
    if not kept:
        return ""
    if script and all("sp" in w for w in kept):
        lo = min(w["sp"][0] for w in kept)
        hi = max(w["sp"][1] for w in kept)
        return display(script[lo:hi])
    return display(" ".join(w["w"] for w in kept))


def auto_captions(words: list, a: int, b: int, max_chars: int, keywords=(), script: str | None = None) -> list:
    idx = [i for i in range(a, b + 1) if not words[i].get("cut")]
    tokens = [(words[i]["w"], display(words[i]["w"])) for i in idx]
    keep = [k for k, t in enumerate(tokens) if t[1]]
    idx = [idx[k] for k in keep]
    tokens = [tokens[k] for k in keep]
    out = []
    for lo, hi in chunk(tokens, max_chars):
        first, last = idx[lo], idx[hi]
        text = chunk_text(words, first, last, script)
        if text:
            out.append({"from": first, "to": last, "text": text, "hl": find_highlights(text, keywords)})
    return out


def schedule(caps: list, duration: float) -> list:
    """자막마다 화면에 떠 있을 구간(show)을 정한다: 다음 자막이 뜰 때까지 유지(깜빡임 방지), 첫 자막은 0초부터."""
    caps.sort(key=lambda c: c["t0"])
    for i, cap in enumerate(caps):
        nxt = caps[i + 1]["t0"] if i + 1 < len(caps) else None
        end = cap["t1"] + 0.3
        if nxt is not None and nxt - cap["t1"] < 0.6:
            end = nxt
        if nxt is not None:
            end = min(end, nxt)
        cap["show"] = [round(cap["t0"], 3), round(min(duration, max(end, cap["t0"] + 0.2)), 3)]
    if caps and caps[0]["t0"] < 0.6:
        caps[0]["show"][0] = 0.0
    return caps
