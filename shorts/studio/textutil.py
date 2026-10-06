"""대본 글자 도우미. 내레이션 속 [[강조]] 표시를 자막 색상으로 바꾸고, 자막을 짧은 덩어리로 나눈다."""

from __future__ import annotations

import re

MARK = re.compile(r"\[\[(.+?)\]\]")
_TRAILING = ".,·…"

Word = list[tuple[str, bool]]  # 한 어절 = (글자, 강조 여부) 조각들


def strip_marks(text: str) -> str:
    """TTS에 넘길 순수 문장. '[[월 25만원]]까지' → '월 25만원까지'"""
    return MARK.sub(r"\1", text).replace("[[", "").replace("]]", "")


def words(text: str) -> list[Word]:
    """띄어쓰기 단위로 나누되, 어절 안에서 강조가 시작·끝나는 경우도 유지한다."""
    segments: list[tuple[str, bool]] = []
    pos = 0
    for match in MARK.finditer(text):
        if match.start() > pos:
            segments.append((text[pos : match.start()], False))
        segments.append((match.group(1), True))
        pos = match.end()
    if pos < len(text):
        segments.append((text[pos:], False))

    result: list[Word] = []
    current: Word = []
    for chunk, highlighted in segments:
        for part in re.split(r"(\s+)", chunk.replace("[[", "").replace("]]", "")):
            if not part:
                continue
            if part.isspace():
                if current:
                    result.append(current)
                    current = []
            else:
                current.append((part, highlighted))
    if current:
        result.append(current)
    return result


def visible_len(word: Word) -> int:
    return sum(len(t) for t, _ in word)


def chunk(text: str, max_chars: int = 14) -> list[list[Word]]:
    """자막 한 화면에 들어갈 만큼 어절을 묶는다. 문장부호에서 먼저 끊는다."""
    chunks: list[list[Word]] = []
    current: list[Word] = []
    length = 0
    for word in words(text):
        size = visible_len(word)
        extra = size if not current else size + 1
        if current and length + extra > max_chars:
            chunks.append(current)
            current, length = [], 0
            extra = size
        current.append(word)
        length += extra
        if word[-1][0][-1:] in ".?!" and length >= max_chars // 2:
            chunks.append(current)
            current, length = [], 0
    if current:
        chunks.append(current)
    return [_trim_end(c) for c in chunks]


def _trim_end(chunk_words: list[Word]) -> list[Word]:
    """화면 자막 끝의 마침표·쉼표는 지운다 (물음표·느낌표는 둔다)."""
    last = chunk_words[-1]
    text, highlighted = last[-1]
    trimmed = text.rstrip(_TRAILING)
    if trimmed != text:
        new_last = last[:-1] + ([(trimmed, highlighted)] if trimmed else [])
        chunk_words = chunk_words[:-1] + ([new_last] if new_last else [])
    return chunk_words


def weight(chunk_words: list[Word]) -> float:
    """읽는 시간 가중치: 문장부호를 뺀 글자 수 + 어절 사이 쉼."""
    letters = sum(1 for w in chunk_words for t, _ in w for ch in t if ch.isalnum())
    return letters + 0.6 * len(chunk_words)


def plain(chunk_words: list[Word]) -> str:
    return " ".join("".join(t for t, _ in w) for w in chunk_words)


def spoken_length(text: str) -> int:
    """발화 길이 추정용 글자 수(공백·기호 제외)."""
    return sum(1 for ch in strip_marks(text) if ch.isalnum())
