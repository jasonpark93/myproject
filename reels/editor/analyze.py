"""단어 시간표 → 편집 판단(군말·말더듬·NG·대본 밖 말) + 문장 나누기 + 문장 역할.

여기서 정한 값은 모두 plan.json에 기록되고, Claude나 사용자가 고친 뒤 다시 렌더할 수 있다.
"""

from __future__ import annotations

import re
from difflib import SequenceMatcher

from .transcript import norm

# ---------- 군말 ----------
FILLER_CHARS = set("음어으엄흠읍에")
SOFT_FILLERS = {"아", "그", "저", "뭐"}  # 망설이는 모양(길게 끌거나 뒤에 쉼)일 때만 군말
REDUP_OK = {"하나", "조금", "진짜", "정말", "빨리", "천천히", "계속", "자꾸", "많이", "너무", "따로", "다시", "꼭", "잘", "또", "좀", "막", "딱"}
SHORT_WORDS = {"이", "그", "저", "안", "못", "잘", "더", "다", "또", "한", "두", "세", "네", "몇", "첫", "새", "각", "전", "총", "약", "왜", "뭐", "꼭", "제", "내", "너", "난", "날", "좀", "막", "딱"}

CUT_LABEL = {
    "silence": "무음",
    "filler": "필러",
    "noise": "필러",  # 인식 안 된 '음…', 숨소리 등
    "stutter": "필러",
    "ng": "NG",
    "offscript": "대본 외",
    "manual": "수동",
}
CUT_PRIORITY = ["ng", "offscript", "manual", "stutter", "filler", "noise", "silence"]


def is_filler(words: list, i: int) -> bool:
    w = words[i]
    n = norm(w["w"])
    if not n:
        return True  # 문장부호만 있는 조각
    if set(n) <= FILLER_CHARS and len(n) <= 4:
        return True
    if n in SOFT_FILLERS:
        dur = w["e"] - w["s"]
        before = w["s"] - words[i - 1]["e"] if i > 0 else 1.0
        after = words[i + 1]["s"] - w["e"] if i + 1 < len(words) else 1.0
        dragged = w["w"].rstrip().endswith(("...", "…", "~"))
        return dur >= 0.3 or dragged or after >= 0.25 or (before >= 0.25 and after >= 0.15)
    return False


def mark_fillers(words: list) -> int:
    count = 0
    for i, w in enumerate(words):
        if not w.get("cut") and is_filler(words, i):
            w["cut"] = "filler"
            count += 1
    return count


def _kept(words: list) -> list:
    return [i for i, w in enumerate(words) if not w.get("cut")]


def mark_stutters(words: list, max_gap: float = 1.0) -> int:
    """'그래서 그래서', '이번 달에 이번 달에', '청 청약통장은' → 앞쪽을 지운다."""
    count = 0
    changed = True
    while changed:
        changed = False
        kept = _kept(words)
        texts = [norm(words[i]["w"]) for i in kept]
        for n in (3, 2, 1):
            for j in range(len(kept) - 2 * n + 1):
                a, b = texts[j : j + n], texts[j + n : j + 2 * n]
                if a != b or not all(a):
                    continue
                if n == 1 and a[0] in REDUP_OK:
                    continue
                if words[kept[j + n]]["s"] - words[kept[j + n - 1]]["e"] > max_gap:
                    continue
                for k in kept[j : j + n]:
                    words[k]["cut"] = "stutter"
                count += n
                changed = True
                break
            if changed:
                break
        if changed:
            continue
        for j in range(len(kept) - 1):
            a, b = texts[j], texts[j + 1]
            if not a or a in SHORT_WORDS or a in REDUP_OK:
                continue
            if len(a) <= 3 and len(a) < len(b) and b.startswith(a):
                if words[kept[j + 1]]["s"] - words[kept[j]]["e"] <= 0.8:
                    words[kept[j]]["cut"] = "stutter"
                    count += 1
                    changed = True
                    break
    return count


# ---------- NG(다시 말하기) ----------
def _concat(words: list, idx: list):
    text, starts = "", []
    for i in idx:
        starts.append(len(text))
        text += norm(words[i]["w"])
    return text, starts


def _coverage(a: str, b: str) -> int:
    """a와 b가 순서대로 겹치는 글자 수 (2글자 이상 연속된 덩어리만 센다 — 우연히 같은 한 글자는 제외)."""
    blocks = SequenceMatcher(None, a, b, autojunk=False).get_matching_blocks()
    return sum(blk.size for blk in blocks if blk.size >= 2)


def find_restart(words: list, before: list, after: list, strict: bool):
    """after(쉼 뒤 말)가 before(쉼 앞 말)의 어느 지점부터 다시 말한 것인지 → before 안의 단어 위치."""
    xs, starts = _concat(words, before)
    ys, _ = _concat(words, after)
    if len(ys) < 4 or not xs:
        return None
    head = ys[:16]
    need_prefix = 6 if strict else 3
    need_cover = 0.8 if strict else 0.65
    best = None
    for wi, p in enumerate(starts):
        if p >= len(xs):
            continue
        prefix = 0
        for ca, cb in zip(xs[p:], head):
            if ca != cb:
                break
            prefix += 1
        if prefix < min(need_prefix, len(head)):
            span = min(len(head), len(xs) - p)
            fuzzy_ok = (
                not strict
                and span >= 6
                and xs[p] == head[0]
                and SequenceMatcher(None, xs[p : p + span], head[:span], autojunk=False).ratio() >= 0.75
            )
            if not fuzzy_ok:
                continue
        tail = xs[p:]
        matched = _coverage(tail, ys[: len(tail) + 4])
        if matched < 4:
            continue
        score = matched / max(1, min(len(tail), len(ys)))
        if score >= need_cover and (best is None or score >= best[0] - 1e-9):
            best = (score, wi)
    return None if best is None else best[1]


def mark_restarts(words: list, long_pause: float = 1.0, short_pause: float = 0.5, window: float = 12.0) -> int:
    """쉼(특히 2초 멈춤) 뒤에 같은 말을 다시 시작하면 앞의 테이크를 NG로 지운다. 마지막 테이크만 남는다."""
    count = 0
    kept = _kept(words)
    for pos in range(1, len(kept)):
        prev_i, cur_i = kept[pos - 1], kept[pos]
        pause = words[cur_i]["s"] - words[prev_i]["e"]
        if pause < short_pause:
            continue
        strict = pause < long_pause
        start_t = words[cur_i]["s"] - window
        before = [i for i in _kept(words) if start_t <= words[i]["s"] and words[i]["e"] <= words[prev_i]["e"] + 1e-6]
        after = []
        for i in kept[pos:]:
            if after and words[i]["s"] - words[after[-1]]["e"] >= long_pause:
                break
            after.append(i)
            if len(after) >= 30:
                break
        if not before:
            continue
        hit = find_restart(words, before, after, strict)
        if hit is None:
            continue
        for i in before[hit:]:
            words[i]["cut"] = "ng"
            count += 1
    return count


# ---------- 대본 맞추기 ----------
_MARK = re.compile(r"\[\[(.+?)\]\]")


def parse_script(text: str):
    """대본에서 [[강조]] 표시를 읽고 표시를 뺀 대본을 돌려준다."""
    keywords = [k.strip() for k in _MARK.findall(text) if k.strip()]
    plain = _MARK.sub(lambda m: m.group(1), text)
    lines = [ln for ln in plain.splitlines() if not ln.strip().startswith("#")]
    return re.sub(r"[ \t]+", " ", "\n".join(lines)).strip(), keywords


def align_script(words: list, script: str) -> dict:
    """인식 결과를 대본에 맞춘다: 오타는 대본 글자로(sp=대본 위치), 대본에 없는 말은 'offscript' 컷."""
    smap, s_norm = [], []
    for pos, ch in enumerate(script):
        n = norm(ch)
        if n:
            s_norm.append(n)
            smap.append(pos)
    s_text = "".join(s_norm)
    kept = _kept(words)
    t_text, starts = _concat(words, kept)
    if not s_text or not t_text:
        return {"offscript": 0, "fixed": 0}
    t2s = [None] * len(t_text)
    deleted = [False] * len(t_text)
    for tag, i1, i2, j1, j2 in SequenceMatcher(None, t_text, s_text, autojunk=False).get_opcodes():
        if tag == "equal":
            for k in range(i2 - i1):
                t2s[i1 + k] = j1 + k
        elif tag == "replace":
            for k in range(i2 - i1):
                t2s[i1 + k] = j1 + min(j2 - j1 - 1, k * (j2 - j1) // (i2 - i1))
        elif tag == "delete":
            for k in range(i1, i2):
                deleted[k] = True
    offscript = fixed = 0
    bounds = starts + [len(t_text)]
    for n, i in enumerate(kept):
        a, b = bounds[n], bounds[n + 1]
        if b <= a:
            continue
        mapped = [t2s[k] for k in range(a, b) if t2s[k] is not None]
        if not mapped and all(deleted[a:b]):
            words[i]["cut"] = "offscript"
            offscript += 1
            continue
        if not mapped:
            continue
        lo, hi = smap[min(mapped)], smap[max(mapped)] + 1
        while hi < len(script) and script[hi] in ".,?!…~·'\"”’)":
            hi += 1
        fixed_text = script[lo:hi].strip()
        if fixed_text and norm(fixed_text) != norm(words[i]["w"]):
            fixed += 1
        if fixed_text:
            words[i]["asr"] = words[i].get("asr", words[i]["w"])
            words[i]["w"] = re.sub(r"\s+", " ", fixed_text)
        words[i]["sp"] = [lo, hi]
    return {"offscript": offscript, "fixed": fixed}


# ---------- 문장 ----------
END_PUNCT = tuple(".?!。？！")
FINAL_ENDING = re.compile(r"(요|다|죠|까|네|니다|세요)[.?!~…]*$")


def split_sentences(words: list) -> list:
    """[(첫 단어, 끝 단어)] — 문장부호, 긴 쉼, 종결 어미 + 쉼, NG 경계에서 나눈다."""
    out, start, chars = [], 0, 0
    for i, w in enumerate(words):
        chars += len(norm(w["w"]))
        if i + 1 >= len(words):
            break
        nxt = words[i + 1]
        gap = nxt["s"] - w["e"]
        text = w["w"].rstrip()
        cut_edge = (w.get("cut") == "ng") != (nxt.get("cut") == "ng")
        boundary = (
            (text.endswith(END_PUNCT) and not text.endswith("..."))
            or gap >= 0.7
            or (FINAL_ENDING.search(text) is not None and gap >= 0.3)
            or cut_edge
            or (chars >= 40 and gap >= 0.35)
        )
        if boundary:
            out.append((start, i))
            start, chars = i + 1, 0
    if start < len(words):
        out.append((start, len(words) - 1))
    return out


# ---------- 문장 역할 → 줌·효과음 ----------
ROLES = ("hook", "number", "twist", "conclusion", "cta", "explain")
ROLE_LABEL = {"hook": "훅", "number": "숫자", "twist": "반전", "conclusion": "결론", "cta": "CTA", "explain": "설명"}
TWIST_START = ("근데", "그런데", "하지만", "그러나", "반면", "사실", "오히려", "문제는", "반전", "의외로", "그럼에도", "그치만", "근데요")
CONCL_START = ("결론", "정리하면", "요약하면", "결국", "즉", "한마디로", "따라서", "그러니까", "그래서")
CTA_WORDS = ("팔로우", "구독", "저장", "댓글", "좋아요", "공유", "알림", "프로필", "링크")
NUMBER = re.compile(
    r"\d|(?:^|\s)(?:한|두|세|네|다섯|여섯|일곱|여덟|아홉|열|스무|백|천|만|억)\s?(?:가지|개|번|배|살|해|달|명|군데|곳|단계|억|만)"
    r"|첫\s?번째|두\s?번째|세\s?번째|첫째|둘째|셋째|퍼센트"
)


def guess_role(text: str, position: int, total: int) -> str:
    plain = text.strip()
    if position == 0:
        return "hook"
    if any(k in plain for k in CTA_WORDS):
        return "cta"
    if plain.startswith(TWIST_START):
        return "twist"
    if plain.startswith(CONCL_START) or (position == total - 1 and total > 2):
        return "conclusion"
    if NUMBER.search(plain):
        return "number"
    return "explain"


def sentence_text(words: list, a: int, b: int, kept_only: bool = True) -> str:
    return " ".join(w["w"] for w in words[a : b + 1] if not (kept_only and w.get("cut"))).strip()
