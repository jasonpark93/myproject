# 릴스 편집 지침 (Claude용)

- 요청 종류에 따라 지침이 다르다.
  - "○○ 주제로 릴스 만들어줘", "녹음했어"처럼 얼굴 없는 모션그래픽 릴스 요청이면 **STORY_GUIDE.md**를 따른다.
  - 찍은 영상을 "편집해줘"라고 하면 아래를 따른다.
- 대화는 항상 한국어로, 짧고 쉽게.
명령은 저장소 최상위에서 `python reels/reel.py ...`로 실행한다 (Windows는 `python`, Mac은 `python3`).

## 0. 처음이거나 오류가 나면
- `python reels/reel.py doctor`로 점검하고, 안내된 설치 명령을 사용자 동의를 받아 실행한다.
  - ffmpeg: Mac은 `brew install ffmpeg`, Windows는 `winget install Gyan.FFmpeg`
  - 패키지: `pip install -r reels/requirements.txt`
- 음성 인식(faster-whisper)이 끝내 안 되면 Vrew·CapCut에서 SRT를 내보내 달라고 하고 `--srt`로 진행한다.

## 1. 분석
```
python reels/reel.py analyze [영상경로] [--script 대본.txt]
```
- 영상 경로를 안 주면 `reels/inbox/`의 가장 최근 영상을 쓴다. 영상과 같은 이름의 `.txt`가 있으면 대본으로 자동 사용한다.
- 출력으로 나온 작업 이름(예: `0107`)을 기억한다. 이후 명령의 `이름` 자리에 쓴다. `last`도 된다.

## 2. 검토표 읽고 판단하기 (`reels/work/이름/review.md`)
아래 다섯 가지를 직접 판단해서 고친다. 이 단계가 Claude의 몫이다.

1. **NG 판단 확인**
   - `✂ NG`로 지워진 문장이 정말 같은 말을 다시 한 앞 테이크인지 본다.
   - 아니면 `keep 이름 C번호`로 되살린다.
   - 반대로 중복인데 남아 있으면 `drop 이름 S번호`로 지운다.
2. **자막 오타**
   - 맞춤법·띄어쓰기·고유명사(브랜드, 앱 이름, 숫자 단위)를 고친다: `fix 이름 틀린말 바른말`
   - 마침표는 넣지 않는다. 말한 그대로 쓰되 오타만 고친다.
3. **문장 역할**: `role 이름 S3=number ...`
   - hook: 첫 문장
   - number: 숫자·통계
   - twist: 근데/사실/반전
   - conclusion: 결론·정리
   - cta: 팔로우·저장·댓글 유도
   - explain: 그 외 설명
   - 역할에 따라 줌이 정해진다. 펀치 줌은 hook·number·twist·conclusion·cta에 들어가고, 설명 문장은 슬로우 줌이다.
4. **강조 단어**
   - 숫자, 핵심 키워드, 결과 단어만 강조한다. 한 줄에 최대 1개.
   - 필요하면 plan.json의 captions[].hl을 고친다. 계속 쓰는 키워드는 plan.json `keywords`에 넣는다.
5. **훅 제목**
   - 첫 3초에 뜨는 제목을 제안해서 넣는다: `title 이름 "청약통장 [[이것]] 모르면\n3천만 원 손해"`
   - 2줄 이내, 한 줄 13자 이내. 핵심 단어는 [[ ]]로 감싸 강조한다.
   - 내용을 과장하거나 영상에 없는 약속은 하지 않는다.

plan.json을 직접 고쳐도 된다. 단어의 `cut` 값을 지우면 그 말이 되살아난다.

## 3. 렌더
```
python reels/reel.py render 이름
```
- 바뀐 단계만 다시 만든다. 예를 들어 효과음 볼륨만 바꾸면 몇 초 만에 끝난다.
- 끝나면 `reels/work/이름/snapshots/snapshots.jpg`를 **반드시 Read로 열어서 직접 본다.** 아래를 확인하고, 문제가 있으면 고쳐서 다시 렌더한다.
  - 자막이 얼굴을 가리지 않는지
  - 줌에서 얼굴이 잘리지 않는지
  - 글자가 잘 읽히는지

## 4. 보고 (사용자에게, 이 형식 그대로)
```
✅ 완성: reels/out/이름.mp4
- 길이: 1:03 → 0:41
- 컷: 무음 15 · 필러 5 · NG 3
- 줌: 0:00 펀치, 0:04 슬로우, ... (report.md의 줌 표)
- 효과음: 0:01 띵(3천만 원), 0:07 휙, ...
- 자막 전문: (report.md의 자막 표. 사용자가 오타를 확인할 수 있게 전부)
- 스냅샷: 처음·중간·끝 (snapshots.jpg를 보여주기)
고칠 게 있으면 "0:12 다시 살려줘", "자막 20% 크게"처럼 말해 주세요.
```
- 보고 뒤에 업로드 문구를 `reels/work/이름/upload.md`로 써서 함께 보여준다.
  - 제목 3개(40자 이내, ★로 추천 1개 표시), 설명 2~3줄, 해시태그 5개.
  - 형식은 STORY_GUIDE.md의 '업로드 문구'와 같다.

## 5. 수정 요청 → 명령
처음부터 다시 분석하지 말고 해당 부분만 고친 뒤 `render`한다.

| 요청 | 명령 |
| --- | --- |
| 문장 사이 0.2초 | `set 이름 gap=0.2` |
| 0:12 다시 살려줘 | `keep 이름 0:12` (완성본 시간) / 원본 시간이면 `keep 이름 src:0:12` |
| 0:12~0:14 잘라줘 | `drop 이름 0:12-0:14` |
| 자막 20% 크게 | `set 이름 caption.size=1.2` (현재 값 × 1.2) |
| 자막 위로/아래로 | `set 이름 caption.bottom=0.42` (아래에서의 비율, 0.30~0.45 권장) |
| '클라우드'→'클로드' | `fix 이름 클라우드 클로드` |
| 강조색 민트 | `set 이름 caption.highlight=민트` (노랑·민트·빨강·주황·초록·하늘·분홍·보라 또는 #RRGGBB) |
| 펀치 줌 3번만 | `set 이름 zoom.max_punch=3` |
| 줌 110% | `set 이름 zoom.punch=1.10` |
| 줌 빼줘 | `set 이름 zoom.enabled=false` |
| 이 문장은 줌 없이 | `zoom 이름 S4=none` |
| 효과음 볼륨 절반 | `set 이름 sfx.volume=0.5` (현재 값 × 0.5) |
| 효과음 너무 많아 | `set 이름 sfx.max_per_10s=2` |
| 이 문장에 띵 | `sfx 이름 S5=ding` |
| 제목 계속 보이게 | `set 이름 title.duration=0` |
| 앞으로도 이렇게 | 같은 `set` 명령에 `--save` → `reels/style.json`에 저장돼 다음 영상부터 기본값 |

## 6. 참고 영상 분석
- 사용자가 잘 된 릴스를 화면 녹화해서 주면 `python reels/reel.py ref 영상경로`를 실행한다.
- `reels/work/ref-*/reference.md`를 읽고, `reference_sheet.jpg`도 Read로 직접 본다.
- 우리 설정과 다른 점을 짚고, 맞출지 물어본 뒤 `set ... --save`로 반영한다.

## 7. 지킬 것
- 촬영본·완성본·작업 파일(`reels/inbox`, `reels/out`, `reels/work`)은 절대 git에 올리지 않는다. 이미 .gitignore에 있다.
- 사용자가 "올려줘"라고 해도 개인 영상은 GitHub에 올리지 말고, 직접 업로드하는 방법을 안내한다.
- 대본에 없는 정보·과장은 자막·제목에 넣지 않는다.
