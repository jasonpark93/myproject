# 모션그래픽 릴스 만들기 (Claude용 작성 지침)

얼굴 없이 **목소리 + 움직이는 카드**로 만드는 설명형 릴스.
사용자는 대본을 소리 내어 녹음만 하면 되고, 나머지(대본·장면 설계·렌더)는 Claude가 한다.

## 디자인 (theme)
- **`note` (기본, 채널 고유 디자인 '돈한입 노트')**
  - 모눈 노트 배경, 위에 채널 이름표 + 회차 꼬리표 + 제목(형광펜이 쓱 칠해짐), 장면 진행 막대.
  - 카드는 종이(그림자·마스킹 테이프·살짝 기울임)이고, 오른쪽에서 밀려 들어와 왼쪽으로 빠진다.
  - 강조는 노란 형광펜, 틀린 것·중요한 것은 빨간 펜(동그라미·X·취소선·손글씨), 정보는 포스트잇.
  - 그림은 다꾸 스티커(3D 그림 + 흰 테두리)와 코드로 그린 그림(청약통장 등).
  - 자막은 먹색 라벨 위 흰 글씨, 강조 단어는 노랑.
- `neon`: 참고 릴스와 비슷한 검정 배경 + 형광 연두. 비교·예시용이다. 올릴 영상에는 쓰지 않는다(남의 디자인과 비슷해짐).
- 새 영상은 `note`로 만든다. 다른 사람 영상의 색·배치·카드 모양을 그대로 따라 하지 않는다.

## 흐름
0. **잘 되는 쇼츠 조사**: 주제가 정해지면 먼저 최근 30일 사이 조회수가 높은 쇼츠의 '제작 공식'을 뽑는다. 방법은 아래 '조사' 절에 있다.
1. 사용자가 주제를 말하면, `reels/stories/이름.json`을 쓴다.
   - 이름은 영문·숫자·하이픈으로 짓는다(예: `cheongyak-01`).
   - 조사에서 뽑은 공식(훅·구성·길이·CTA)을 따르되, 문장·예시·소재는 새로 쓴다.
   - `upload`(제목 3개, 설명, 해시태그 5개)도 함께 쓴다.
2. 컴퓨터 음성으로 미리보기를 만든다: `python reels/reel.py story 이름 --tts`
   - `reels/work/story-이름/snapshots/scenes.jpg`를 Read로 직접 보고 카드·자막을 점검한다.
3. 사용자에게 대본(각 장면의 say를 이어 붙인 글)을 보여주고 녹음을 부탁한다.
   - 휴대폰 음성 메모로 녹음해서 `reels/inbox/이름.m4a`로 넣으면 된다(다른 이름이면 `--voice` 경로).
   - 녹음 요령은 아래에 있다.
4. 녹음이 오면 `python reels/reel.py story 이름`을 실행한다.
   - 군말·NG·쉼을 자르고 대본 글자로 자막을 맞춘 뒤, 말에 맞춰 카드가 움직인다.
5. `report.md`의 자막 전문과 `scenes.jpg`를 확인하고 보고한다. 형식은 EDITING_RULES.md의 보고와 같다.
   - `upload.md`(추천 제목·설명·해시태그)도 함께 보여준다.
   - 업로드는 사용자가 확인한 뒤 직접 한다. 채널 연결 자동 업로드는 사용자가 명시적으로 원할 때만 한다.

## 조사: 잘 되는 쇼츠에서 공식 뽑기 (마윤 강의 방식을 무료로)
- vidIQ가 연결돼 있으면 쓴다. 호출마다 크레딧(조회 5)이 들어간다.
  - 먼저 `vidiq_balance`(무료)로 남은 크레딧을 확인한다.
  - 주제 하나당 4번 이내로 조사한다.
- 순서:
  1. `vidiq_trending_videos`
     - 설정: videoFormat=short, videoTitleLanguage=ko, titleQuery=주제, videoPublishedAfter=30일 전, sortBy=viewCount, limit=10
     - 조회수·참여율이 높은 1~3개를 고른다.
  2. 고른 영상의 대본을 `vidiq_video_transcript`로 받는다. 1~2개면 충분하다.
  3. 공식을 정리해 `reels/research/주제-날짜.md`에 저장한다. 이 폴더는 GitHub에 안 올라간다.
     - 첫 문장(훅)의 종류: 이득, 손해, 반전, 질문, 숫자
     - 구성 비트와 초 단위 길이
     - 전체 길이, 화면 글자, CTA, 제목 패턴
  4. 참고 영상 대본은 `reels/research/참고-제목.txt`로 저장한다. 다 쓴 대본은 이렇게 겹침을 검사한다:
     - `python reels/reel.py similar 이름 reels/research/참고-제목.txt`
     - 결과가 '괜찮습니다'가 될 때까지 문장을 바꿔 쓴다.
- vidIQ가 없으면 사용자에게 참고 쇼츠 링크나 화면 녹화를 1~2개 받는다.
  - 화면 녹화는 `python reels/reel.py ref 영상`으로 리듬·자막 위치를 재고, 대본은 화면 자막을 보고 옮긴다.
- 원칙:
  - 구조·연출 문법은 따라 해도 된다.
  - 문장·예시·영상·음원·캐릭터는 가져오지 않는다.
  - 같은 형식을 찍어내듯 대량으로 만들지 않는다. 매 영상에 새 정보와 내 목소리가 들어가야 유튜브 '재사용·대량생산 콘텐츠' 정책에 걸리지 않는다.

## 대본 구성 (정보형 쇼츠, 35~45초)

| 순서 | 역할 | 길이 | 잘 맞는 카드 |
| --- | --- | --- | --- |
| 1 | **훅**: 멈추게 하는 한마디 + 궁금증 ("잠깐, ○○하려고요? ○○하는 순간 △△이 사라져요") | 2~4초 | hook (그림이 깨지거나 쾅 찍힘) |
| 2 | 지금 이 얘기를 하는 이유 (최신 숫자·뉴스) | 3~4초 | chart, stat |
| 3 | 궁금증의 답 + 핵심 원리 | 5~7초 | stack, bigtext |
| 4 | 자세한 정보 (사라지는 것·조건·주의점) | 4~6초 | checklist(`mark: x`), notes |
| 5 | 실제 사례 (돈·시세) | 5~6초 | compare, image(사진) |
| 6 | **바로 따라 할 수 있는 방법** | 5~7초 | phone(앱 화면), steps |
| 7 | 챙길 혜택·조건 | 5~7초 | notes, checklist |
| 8 | **CTA**: 저장 + 의견 묻기 ("깨기 전에 다시 보게 저장해 두세요. 여러분은?") | 4~5초 | cta, comment |

규칙
- 훅은 첫 2초 안에 '멈출 이유'를 준다: 손해·궁금증·반전 중 하나. 첫 화면(0초)에도 제목과 그림이 보여야 한다.
- 정보는 '보는 사람이 바로 써먹을 수 있게' 쓴다: 조건(나이·소득), 숫자(최대 얼마), 방법(어디서 무엇을 누르나), 출처.
- 장면 하나 = 한두 문장 = 3~7초. 장면은 7~9개.
- 문장은 짧고 말하듯이 쓴다. 한 문장은 25자 이내가 좋다.
- 첫 문장은 2초 안에 핵심을 말한다. 인사나 자기소개로 시작하지 않는다.
- 숫자·돈·제도 이야기는 사실만 쓴다. 확실하지 않으면 사용자에게 확인한다. 과장·허위 약속은 금지.
- CTA로 자료를 준다고 했으면 실제로 줄 자료가 있어야 한다. 없으면 "팔로우하고 저장해 두세요"로 바꾼다.

## 파일 형식
```json
{
  "theme": "note",
  "brand": "돈한입",
  "header": {"tag": "청약 EP.1", "title": "청약통장 [[깨기 전에]]"},
  "scenes": [
    {"say": "읽을 말. [[강조]]는 자막에서 강조색", "card": {"type": "...", ...}, "stickers": [...], "stamp": {...}}
  ],
  "settings": {"sfx": {"max_per_10s": 4, "volume": 1.0}},
  "upload": {"titles": ["제목1", "제목2", "제목3"], "pick": 0, "description": "설명 2~3줄", "hashtags": ["#쇼츠", "#청약", "#내집마련", "#재테크", "#청약통장"]},
  "bgm": "음악 파일 경로 (생략하면 reels/bgm/의 첫 파일, false면 없음)"
}
```
- brand: 채널 이름표(노트 테마). 사용자가 정한 채널 이름을 쓴다. `brand_sticker`로 이름표 옆 그림을 바꿀 수 있다(기본 coin).
- header.tag: 회차·분류 꼬리표(예: "청약 EP.1"). neon 테마에서는 header.label이 작은 글씨 줄이다.
- header.title: 큰 제목, 주제 키워드. 10자 안팎. `[[ ]]` 부분에 형광펜이 칠해진다. 영상 내내 보인다.
- header.progress: 장면 진행 막대 (기본 true, 끄려면 false).
- `at`: 카드 안에서 그 말이 나올 때 애니메이션(체크·등장)이 일어난다.
  - say에 있는 말을 그대로 쓴다. 띄어쓰기·문장부호는 무시된다.
  - 없으면 장면 안에 고르게 배치된다.

## 업로드 문구 (upload)
- titles 3개, 각각 40자 이내. 핵심 키워드를 앞에 둔다.
  - 이득형: "○○ 이렇게 하면 3천만 원 아낍니다"
  - 경고형: "○○ 모르면 손해"
  - 질문형 중 하나를 고른다.
  - pick에 추천 번호를 쓴다(0부터 센다).
- description: 2~3줄 요약.
  - 돈·제도 이야기면 "개인 상황에 따라 다를 수 있으니 공고·기관 안내를 확인하세요"를 넣는다.
  - 댓글 CTA를 썼으면 그 안내도 넣는다.
- hashtags 5개: `#쇼츠` + 주제 태그 4개. 넓은 태그와 좁은 태그를 섞는다.
- 렌더하면 `reels/work/story-이름/upload.md`로 정리돼 나온다.

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
| `image` | 캡처 화면·사진 | `path` (스토리 파일 기준 상대경로 가능) | `zoom`(1.06), `max_h`, `caption`(폴라로이드 아래 손글씨), `credit`(출처, 사진을 가져왔으면 필수) |
| `text` | 그 밖의 설명 | — | `title`, `lines` |
| `hook` | 첫 2초 훅 | `lines`: ["돈으로도 못 사는 게", "[[사라져요]]"] | `top`(손글씨 "잠깐!"), `object`("passbook"=청약통장 그림, 또는 스티커 이름), `crack_at`(통장이 쩍 갈라짐+화면 흔들림), `since`·`count`(통장에 적힌 글), `at`(줄마다), `size` |
| `chart` | 꺾은선 그래프 (추세) | `points`: [{"x":"2022.6","v":2860,"label":"2,860만","note":"손글씨"}] | `title`, `sticker`, `pos`(점의 가로 위치 0~1), `range`([최소, 최대]), `at`(선 그리기 시작), `draw`(초), `badge`("1년 새\n-63만 명"), `badge_at`, `badge_pos`(tl/tr/bl/br), `source`(출처), `color` |
| `stack` | 합계를 나눈 막대 (가점 84 = 32+35+17) | `parts`: [{"label","value","focus","at"}] | `title`, `sticker`, `total`, `unit`("점"), `note`(빨간 손글씨)·`note_at`, `sub` |
| `phone` | 앱에서 바꾸는 방법 (일반 은행 앱 모양) | `from`, `to` (바뀌는 금액) | `screen`(화면 제목), `rows`: [{"k","v"}], `field`, `hint`, `change_at`, `button`, `tap_at`, `toast`, `note`(옆 포스트잇)·`note_at` |
| `notes` | 포스트잇 2~3장 (혜택·조건) | `notes`: [{"head","big","small","color"(yellow/mint/pink/blue),"sticker","at"}] | `title`, `note_h` |
| `cta` | 저장 + 투표 마무리 | `lines` | `at`, `save_at`(저장 버튼 톡), `question`(손글씨), `question_at`, `options`(["유지파","해지파"]), `options_at` |

카드 공통 선택 항목
- `sticker`: 카드 제목 옆 그림 (checklist·steps·compare·chart·stack, stat은 숫자 옆).
- checklist: `mark: "x"`면 빨간 펜 X + 취소선(사라지는 것·하면 안 되는 것). 항목마다 `sub`(작은 설명), `sticker`.
- compare: `source`(출처 작은 글씨).

장면에 얹는 것
- `stickers`: 카드 위에 '착' 붙는 다꾸 스티커.
  - 예: [{"name":"bulb","pos":[0.05,0.04],"size":140,"at":"부담되면","angle":-10}] (pos는 카드 기준 비율)
- `pills`: 카드 주변에 떠 있는 꼬리표(노트 테마는 작은 포스트잇).
  - 예: [{"text":"컷 편집","icon":"cut","pos":"tl","at":"..."}]
  - pos는 tl, tr, bl, br, t, b 중 하나.
- `stamp`: 쿵 찍히는 도장(노트 테마는 빨간 인주 도장).
  - 예: {"text":"직접 편집\n필요없음","at":"...","size":300,"pos":[0.78,0.72]}

스티커 이름 (Microsoft Fluent 이모지 3D, MIT 라이선스 — 상업 이용 가능, 설명란 출처 표기 권장)
- `reels/assets/stickers/`에 있는 것: `house houses apartment money coin cash flying_money down up hammer phone bank calendar hourglass clock bulb warning stop check x pin memo bookmark key gift think scream sparkles alert fire party receipt lock`
- 목록에 없는 것도 Fluent 이모지 영어 이름("Money with wings")으로 쓰면 처음 한 번 내려받는다(인터넷 필요).

사진 (보는 사람이 실제로 알아보게 하는 그림)
- 넣는 곳: `reels/stories/assets/스토리이름/` → card `{"type":"image","path":"assets/스토리이름/파일.jpg","caption":"과천 ○○ 단지","credit":"사진: 출처"}`
- 쓸 수 있는 것:
  - 사용자가 직접 찍은 사진, 앱·누리집 화면 캡처(이름·계좌·전화번호는 가리기)
  - 무료 사진 사이트(Unsplash·Pexels·Pixabay)는 각 사이트 라이선스 확인 후 사용
  - 공공누리 1유형 사진(출처 표시)
- 가져온 사진은 credit에 출처를 쓰고 upload 설명에도 적는다. 출처를 모르는 인터넷 사진·다른 사람 영상 캡처는 쓰지 않는다.
- 클라우드 작업 환경에서는 사진 사이트 접속이 막혀 있을 수 있다. 그때는 사용자에게 사진을 부탁하거나 스티커·그림 카드로 대신한다.

아이콘 이름 (직접 그린 단순 아이콘, 스티커가 없을 때):
`spark zoom sound caption bolt cut clock calendar mic pin money chart home heart send mail star fire warning up down x play gift phone user check bank doc`

## 녹음 요령 (사용자에게 그대로 안내)
- 휴대폰 **음성 메모**로, 입에서 20~30cm 떨어져 조용한 방에서 녹음해요.
- 대본을 보며 **말하듯이** 읽어요. 또박또박보다 약간 빠르게 읽는 게 좋아요.
- 틀리면 **2초 멈추고 그 문장 처음부터** 다시 읽어요. 마지막 테이크만 남아요.
- 파일 이름을 스토리 이름으로 바꿔 `reels/inbox/`에 넣고 "녹음했어"라고 말해요.

## 수정 요청
- 문구·카드는 stories JSON을 고쳐서 다시 `story 이름`으로 렌더한다. 목소리 처리 결과는 캐시돼서 영상만 다시 만든다.
- 자막 오타는 say를 고친다. 녹음과 글자가 조금 달라도 대본 글자 기준으로 자막이 나간다.
