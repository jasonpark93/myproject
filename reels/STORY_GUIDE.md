# 모션그래픽 릴스 만들기 (Claude용 작성 지침)

얼굴 없이 **목소리 + 움직이는 카드**로 만드는 설명형 릴스.
화면 구성은 검정 배경, 상단 고정 제목(형광 연두), 가운데 카드, 하단 자막이다.
사용자는 대본을 소리 내어 녹음만 하면 되고, 나머지(대본·장면 설계·렌더)는 Claude가 한다.

## 흐름
1. 사용자가 주제를 말하면, `reels/stories/이름.json`을 쓴다. 이름은 영문·숫자·하이픈으로 짓는다(예: `cheongyak-01`).
2. 컴퓨터 음성으로 미리보기를 만든다: `python reels/reel.py story 이름 --tts`
   - `reels/work/story-이름/snapshots/scenes.jpg`를 Read로 직접 보고 카드·자막을 점검한다.
3. 사용자에게 대본(각 장면의 say를 이어 붙인 글)을 보여주고 녹음을 부탁한다.
   - 휴대폰 음성 메모로 녹음해서 `reels/inbox/이름.m4a`로 넣으면 된다(다른 이름이면 `--voice` 경로).
   - 녹음 요령은 아래에 있다.
4. 녹음이 오면 `python reels/reel.py story 이름`을 실행한다.
   - 군말·NG·쉼을 자르고 대본 글자로 자막을 맞춘 뒤, 말에 맞춰 카드가 움직인다.
5. `report.md`의 자막 전문과 `scenes.jpg`를 확인하고 보고한다. 형식은 EDITING_RULES.md의 보고와 같다.

## 대본 구성 (참고 릴스에서 검증된 순서, 25~40초)

| 순서 | 역할 | 길이 | 잘 맞는 카드 |
| --- | --- | --- | --- |
| 1 | **훅**: 결과·이득을 먼저 말한다 ("~할 필요 없습니다", "~모르면 손해") | 2~3초 | bigtext (+ pills / stamp) |
| 2 | 증거·신뢰 ("지금 보고 계신 이 영상도…") | 3초 | stat, image |
| 3 | 쉬움 강조 ("방법도 진짜 간단해요") | 2초 | bigtext + pill |
| 4 | 방법 1~3단계 | 3~4초 | steps (+ stamp "끝!") |
| 5 | 기능·혜택 목록 | 4초 | checklist (말할 때마다 체크) |
| 6 | 고통 제거 (귀찮은 것, 손해 보는 것) | 4초 | waveform, compare |
| 7 | 결과 (시간·돈 절약 → 꾸준함) | 4초 | compare, calendar, stat |
| 8 | **CTA**: "댓글에 '키워드' 남기시면 ○○ 보내드릴게요" | 4~5초 | comment |

규칙
- 장면 하나 = 한두 문장 = 2~5초. 장면은 8~10개.
- 문장은 짧고 말하듯이 쓴다. 한 문장은 25자 이내가 좋다.
- 첫 문장은 2초 안에 핵심을 말한다. 인사나 자기소개로 시작하지 않는다.
- 숫자·돈·제도 이야기는 사실만 쓴다. 확실하지 않으면 사용자에게 확인한다. 과장·허위 약속은 금지.
- CTA로 자료를 준다고 했으면 실제로 줄 자료가 있어야 한다. 없으면 "팔로우하고 저장해 두세요"로 바꾼다.

## 파일 형식
```json
{
  "header": {"label": "요즘 해외에서 난리난", "title": "클로드 영상 자동편집"},
  "scenes": [
    {"say": "읽을 말. [[강조]]는 자막에서 연두색", "card": {"type": "...", ...}, "pills": [...], "stamp": {...}}
  ],
  "settings": {"sfx": {"max_per_10s": 4, "volume": 1.0}},
  "bgm": "음악 파일 경로 (생략하면 reels/bgm/의 첫 파일, false면 없음)"
}
```
- header.label: 작은 글씨, 맥락을 담는다(예: "모르면 손해", "1분 정리", "요즘 난리난"). 15자 이내.
- header.title: 큰 연두 글씨, 주제 키워드. 10자 안팎. 영상 내내 보인다.
- `at`: 카드 안에서 그 말이 나올 때 애니메이션(체크·등장)이 일어난다.
  - say에 있는 말을 그대로 쓴다. 띄어쓰기·문장부호는 무시된다.
  - 없으면 장면 안에 고르게 배치된다.

## 카드 종류 (card.type)

| type | 용도 | 필수 | 선택 |
| --- | --- | --- | --- |
| `bigtext` | 큰 한 방 문구 | `lines`: ["방법도", "[[진짜 간단]]"] — `[[ ]]`로 감싼 줄은 연두 대형 글씨 | `pill` 아래 작은 꼬리표, `at`(줄마다), `size`(기본 250), `underline` |
| `checklist` | 기능·혜택 목록, 말할 때 체크 | `items`: [{"text","icon","at"}] | `title`, `icon`(기본 spark) |
| `steps` | 1-2-3 단계 | `steps`: [{"text","at"}] | `title`, `icon` |
| `compare` | 전후 비교 막대 | `bars`: [{"label","value"(0~1),"color"(red/lime/#hex),"at"}] — 2개면 같은 자리에서 바뀜 | `title`, `icon`(clock), `result`(아래 큰 글씨), `result_at`, `mode`(replace/stack) |
| `calendar` | 꾸준함·일정 | — | `title`, `days`, `check`(체크할 날 수), `at`, `step` |
| `stat` | 큰 숫자 카운트업 | `value` | `label`, `unit`("만 원", "%"), `prefix`, `decimals`, `sub`, `at`, `count`(초) |
| `waveform` | 문제 구간 표시 → 잘라냄 | `marks`: [{"label","at"}] | `title`, `cut_at` |
| `comment` | 댓글 CTA + 자료 DM | `keyword` | `lead`("댓글에"), `type_at`, `dm`("○○님이 자료를 보냈어요"), `dm_at`, `items`: [{"text","at"}] |
| `image` | 캡처 화면·사진 | `path` (스토리 파일 기준 상대경로 가능) | `zoom`(1.06), `max_h` |
| `text` | 그 밖의 설명 | — | `title`, `lines` |

장면에 얹는 것
- `pills`: 카드 주변에 떠 있는 꼬리표.
  - 예: [{"text":"컷 편집","icon":"cut","pos":"tl","at":"..."}]
  - pos는 tl, tr, bl, br, t, b 중 하나.
- `stamp`: 쿵 찍히는 도장.
  - 예: {"text":"직접 편집\n필요없음","at":"...","size":300,"pos":[0.78,0.72]}

아이콘 이름 (직접 그린 단순 아이콘):
`spark zoom sound caption bolt cut clock calendar mic pin money chart home heart send mail star fire warning up down x play gift phone user check bank doc`

## 녹음 요령 (사용자에게 그대로 안내)
- 휴대폰 **음성 메모**로, 입에서 20~30cm 떨어져 조용한 방에서 녹음해요.
- 대본을 보며 **말하듯이** 읽어요. 또박또박보다 약간 빠르게 읽는 게 좋아요.
- 틀리면 **2초 멈추고 그 문장 처음부터** 다시 읽어요. 마지막 테이크만 남아요.
- 파일 이름을 스토리 이름으로 바꿔 `reels/inbox/`에 넣고 "녹음했어"라고 말해요.

## 수정 요청
- 문구·카드는 stories JSON을 고쳐서 다시 `story 이름`으로 렌더한다. 목소리 처리 결과는 캐시돼서 영상만 다시 만든다.
- 자막 오타는 say를 고친다. 녹음과 글자가 조금 달라도 대본 글자 기준으로 자막이 나간다.
