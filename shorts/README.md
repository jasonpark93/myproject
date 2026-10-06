# 돈한입: AI 쇼츠 반자동 채널

생활경제·내 집 마련 정보를 1분 안팎 세로 영상으로 만드는 유튜브 쇼츠 채널과, 그 채널을 거의 자동으로 굴리는 파이프라인입니다.

```
[매주 일요일 10시] Claude가 웹 검색으로 사실 확인 → 7일치 대본 작성 → 검수용 PR
          ↓  (사람: 15~25분 읽고 고치고 Merge)
[자동] 음성(Google TTS) + 배경영상(Pexels) + 자막·그래픽 → 1080x1920 MP4
          ↓  GitHub Releases에 영상 + 제목·설명·고정댓글 보관
[사람: 영상 1편당 약 3분] YouTube 앱으로 업로드·예약
   (YouTube API 감사를 통과하면 이 단계도 자동 예약 업로드로 전환)
[매주 월요일] 조회수 리포트 이슈 → 잘 된 주제가 다음 주 대본에 반영
```

## 한눈에 보기

| 항목 | 내용 |
| --- | --- |
| 하루에 쓰는 시간 | 평균 10~15분 (몰아서 하면 일요일에 1~1.5시간) |
| 처음 드는 돈 | Claude API 크레딧 선불 5~10달러(약 7천~1만 4천 원). 나머지는 무료 |
| 매달 드는 돈 | 약 6~9달러(약 1만 원 안팎, Claude API). 음성·영상·호스팅은 무료 한도 안 |
| 첫 수익까지 | 몇 달에서 1년 이상. 대부분의 채널은 수익 조건을 못 넘깁니다 (아래 '현실적인 기대치') |
| 이미 준비된 것 | 첫 주 대본 7편, 주제 은행 36개, 채널 프로필·배너, 채널 설명 문구, 자동화 워크플로 |

## 왜 이렇게 만들었나

2026년 10월 기준으로 조사한 결과를 반영했습니다.

1. **완전 자동 대량 생산은 막혔습니다.** 유튜브는 2025년 7월 15일부터 틀만 같고 내용만 바꾼 대량 생산 영상을 '비진정성 콘텐츠'로 보고 수익 창출에서 뺍니다. 그래서 AI가 80~90%를 만들고, 사람이 매주 대본을 검수하는 반자동 구조로 만들었습니다.
2. **API 자동 업로드에는 심사가 필요합니다.** 심사(감사)를 통과하지 않은 구글 클라우드 프로젝트가 API로 올린 영상은 강제로 '비공개'가 됩니다. 그래서 처음에는 휴대폰으로 직접 올리고, 심사를 통과하면 자동 예약 업로드로 바꾸는 2단계로 설계했습니다.
3. **수익 조건이 높아지고 있습니다.** 지금은 구독자 1,000명 + 최근 90일 쇼츠 조회수 1,000만 회가 필요합니다. 2027년 2월 1일부터 새로 신청하는 채널은 쇼츠 조회수 2,000만 회(또는 긴 영상 시청시간 8,000시간)가 필요합니다.
4. **주제: 돈·내 집 마련.** vidIQ 기준 한국 유튜브 월 검색량은 '부동산' 약 34만, '재테크' 약 19만, '청약통장' 약 3만 회입니다. 특히 '청약통장'은 기준치보다 +144% 늘었습니다. 금융·부동산은 광고 단가가 높고, 청약 공공데이터로 매주 새롭고 정확한 영상을 만들 수 있습니다(월요일 '이번 주 청약' 편).
5. **사실 확인을 파이프라인에 넣었습니다.** 제도와 숫자가 자주 바뀌는 분야라, 대본을 쓰기 전에 Claude가 웹 검색으로 공식 출처를 확인하게 했습니다. 또 '업로드 전에 사람이 확인할 사실' 목록이 대본마다 붙습니다.

## 시작하기

### 0단계: 유튜브 채널 만들기 (20분)

1. 쇼츠 전용으로 쓸 구글 계정으로 [youtube.com](https://www.youtube.com)에 로그인 → 프로필 → **채널 만들기**
   - 개인 이름을 드러내고 싶지 않으면 **브랜드 계정**으로 만드세요(설정 → 채널 추가 또는 관리).
2. 이름·핸들·설명·키워드는 [CHANNEL_KIT.md](CHANNEL_KIT.md)의 문구를 그대로 쓰면 됩니다.
3. YouTube 스튜디오 → 맞춤설정 → 브랜딩: `brand/profile.png`(프로필), `brand/banner.png`(배너) 업로드
4. 구글 계정 **2단계 인증** 켜기 (수익 창출 신청에 필수)
5. YouTube 스튜디오 → 설정
   - 채널 → 고급 설정: 시청자층 '아동용 아님', 국가 '대한민국'
   - 기능 사용 자격요건: **전화번호 인증** (중급 기능)
   - 업로드 기본설정: 카테고리 '교육', 동영상 언어 '한국어', 설명에 CHANNEL_KIT의 기본 문구
6. '변경되거나 합성된 콘텐츠' 표시: 이 파이프라인처럼 일반 TTS 목소리, 그래픽, 실제 스톡 영상만 쓰면 표시 대상이 아닙니다. 실제 사람의 목소리를 복제하거나, 실제처럼 보이는 가짜 장면을 AI로 만들 때만 '예'를 고르세요(`channel.json`의 `contains_synthetic_media`).

### 1단계: Google Cloud에서 음성 키 받기 (15분, 무료)

1. [console.cloud.google.com](https://console.cloud.google.com) → 새 프로젝트(예: `shorts-studio`)
2. **결제 계정 연결**: 카드를 등록해야 API를 쓸 수 있습니다. 매달 100만 자(Chirp 3 HD 음성)까지 무료이고, 이 채널은 한 달에 약 1만 자를 씁니다.
   - 결제 → 예산 및 알림에서 5천 원 알림을 걸어 두면 안심입니다.
3. API 및 서비스 → 라이브러리 → **Cloud Text-to-Speech API** 사용 설정
4. 사용자 인증 정보 → API 키 만들기 → 키 제한에서 'Cloud Text-to-Speech API'만 허용 → 복사
5. (주간 리포트용, 선택) 같은 방법으로 **YouTube Data API v3**를 켜고 그 API로 제한한 키를 하나 더 만듭니다. 채널 ID(`UC`로 시작)는 [youtube.com/account_advanced](https://www.youtube.com/account_advanced)에서 확인합니다.

### 2단계: Claude API 키 받기 (10분, 대본 자동 작성용)

1. [console.anthropic.com](https://console.anthropic.com) 가입 → Billing에서 크레딧 5~10달러 충전
2. 월 사용 한도(Limits)를 10달러 정도로 정해 두면 그 이상 청구되지 않습니다.
3. API Keys → Create Key → 복사

API 키가 없어도 첫 주 대본 7편은 이미 들어 있어서 영상은 바로 만들 수 있습니다. 자동 대본은 그다음 주부터 필요합니다.

### 3단계: (선택) Pexels 키 — 배경 영상

[pexels.com/api](https://www.pexels.com/api/)에서 무료 키를 받으면 장면마다 어울리는 세로 영상이 배경으로 깔립니다. 없으면 브랜드 색상 그라디언트 배경을 씁니다.

### 4단계: GitHub 설정 (10분)

1. 저장소 **Settings → Secrets and variables → Actions → New repository secret**

   | 이름 | 값 | 필수 |
   | --- | --- | --- |
   | `GOOGLE_TTS_API_KEY` | 1단계 4번 키 | 필수 |
   | `ANTHROPIC_API_KEY` | 2단계 키 | 자동 대본에 필요 |
   | `PEXELS_API_KEY` | 3단계 키 | 선택 |
   | `YOUTUBE_API_KEY`, `YOUTUBE_CHANNEL_ID` | 1단계 5번 | 주간 리포트에 필요 |
   | `DATA_GO_KR_API_KEY` | 공공데이터포털 청약홈 키 (`cheongyak/README.md` 참고) | '이번 주 청약' 편에 필요 |

2. **Settings → Actions → General → Workflow permissions**: 'Read and write permissions' 선택, **'Allow GitHub Actions to create and approve pull requests'** 체크 (대본 PR을 열기 위해 필요)
3. 이 작업 브랜치를 `master`에 머지합니다.
4. **Actions → '쇼츠 영상 만들기' → Run workflow** → 몇 분 뒤 **Releases**에 첫 영상들이 올라옵니다.

## 매주 할 일

| 언제 | 할 일 | 시간 |
| --- | --- | --- |
| 일요일 오전 | 'pull requests'에 온 **🎬 쇼츠 대본 검수** PR 읽기 → '확인할 사실' 체크 → 고칠 곳은 Files changed에서 직접 수정 → **Merge** | 15~25분 |
| 머지 직후 (자동) | 영상 렌더 → Releases에 영상·제목·설명 정리 | 0분 |
| 매일 또는 일요일에 몰아서 | Releases에서 mp4 저장 → YouTube 앱으로 업로드, 제목·설명 붙여넣기, 예약 → 고정 댓글 | 1편당 약 3분 |
| 월요일 | Issues의 **📊 쇼츠 주간 리포트** 확인 | 5분 |

일요일에 7편을 한 번에 예약하면 주 1~1.5시간, 하루로 나누면 10~15분입니다.

## 비용

| 항목 | 비용 | 메모 |
| --- | --- | --- |
| Claude API (대본) | 1편당 약 0.2~0.3달러, 월 약 6~9달러 | 웹 검색 사실 확인 포함, 기본 모델 Claude Opus 5.5 ($4/$20 per MTok). `channel.json`의 `writer.model`을 `claude-sonnet-5-5`로 바꾸면 토큰 단가가 절반입니다 |
| Google TTS (음성) | 0원 | 월 100만 자 무료, 사용량 약 1만 자 |
| Pexels (배경 영상) | 0원 | 상업적 이용 가능 |
| GitHub Actions (렌더) | 0원 | 공개 저장소는 무료 |
| YouTube API | 0원 | |
| (선택) ElevenLabs 등 프리미엄 음성 | 유료 플랜 | `voice.provider`를 `elevenlabs`로 |

## 현실적인 기대치

- **쇼츠 광고 수익 단가**: 한국 기준 조회 1,000회당 약 50~200원. 월 300만 회 조회여도 월 15만~60만 원입니다.
- **같은 분야 실제 조회수** (vidIQ, 2026년 4~10월 업로드 쇼츠): '청약 꿀팁' 상위 영상은 2천~1만 회입니다. '경제 상식' 쇼츠는 1~2천 회가 흔하고, 리스트형 일부가 4만~7만 회입니다. 구독자 수십 명인 채널도 시기성 있는 주제(마감 임박 제도 등)로 1만 회를 넘긴 사례가 있습니다.
- **예상 시나리오 (추정, 보장 아님)**

  | 시점 | 흔한 경우 | 잘 될 때 |
  | --- | --- | --- |
  | 3개월 | 수익 0원, 구독자 수십~수백 | 구독자 1천 근처 |
  | 6개월 | 수익 0원 | 수익 조건 통과, 월 수만 원 |
  | 12개월 | 0~수만 원 | 월 20~60만 원 + 협찬 문의 |

- **광고 말고 먼저 오는 수익**: 구독자 1만 명 전후부터 부동산·금융 협찬(분양 홍보 등)이 들어올 수 있습니다. 채널 링크로 청약 정보 사이트(`cheongyak/`)나 텔레그램 채널에 사람을 모으는 것도 방법입니다. 링크는 `channel.json`의 `channel.links`에 넣으면 영상 설명에 자동으로 붙습니다.
- 운영비가 월 1만 원 안팎이라 **실험 비용이 싸다는 것**이 이 방식의 진짜 장점입니다. 주간 리포트를 보고 잘 되는 주제·첫 문장을 빠르게 늘리세요.

## 정책·법 체크리스트

- [x] 사람 검수(PR) 후 공개: 비진정성 콘텐츠 정책 대응
- [x] 영상마다 출처, 면책 문구, 해시태그 자동 삽입
- [x] 특정 상품 권유 금지, 과장 표현 금지 (대본 작성 규칙에 포함)
- [ ] 배경음악: [YouTube 오디오 보관함](https://studio.youtube.com) 음악을 `assets/bgm/`에 넣거나, 업로드할 때 앱의 '사운드'를 쓰세요. 출처 모를 음악은 쓰지 마세요.
- [ ] 협찬을 받으면 업로드 때 '유료 광고 포함'을 표시하고 설명란에 대가를 밝히세요.
- [ ] 회사에 다닌다면 겸업 규정을 확인하세요. 유튜브 수익은 종합소득세 신고 대상입니다.

## 바꿔 쓰기

| 파일 | 바꾸는 것 |
| --- | --- |
| `channel.json` | 채널 이름·소개·말투, 색상, 목소리(`voice`), 요일별 시리즈(`schedule.days`), 공개 시각, 검수 방식(`review`: pr/auto), 업로드 방식(`upload`: manual/api) |
| `topics.json` | 주제 은행. `used_on`이 비어 있는 주제부터 차례로 씁니다. 다 쓰면 Claude가 조회수 좋은 영상을 참고해 새 주제를 제안합니다 |
| `episodes/*.json` | 대본. `script` 안의 문장·제목만 고치면 됩니다. 내레이션에서 `[[강조]]`는 자막 노란색입니다 |
| `assets/bgm/` | 배경음악 파일(mp3 등). 여러 개면 영상마다 번갈아 씁니다 |

목소리를 바꾸려면 `GOOGLE_TTS_API_KEY=... python -m studio voices`로 한국어 목소리 목록을 보고, `channel.json`의 `voice.google_voice`를 바꾸세요. 말 빠르기는 `voice.speed`(기본 1.1)입니다.

## 완전 자동화로 넘어가기 (선택, 채널이 자리 잡은 뒤)

1. Google Cloud → API 및 서비스 → **OAuth 동의 화면**(외부)을 만들고 **프로덕션으로 게시**합니다. '테스트' 상태로 두면 토큰이 7일 뒤 만료됩니다.
2. 사용자 인증 정보 → **OAuth 클라이언트 ID**(유형: 데스크톱 앱) 만들기
3. 내 컴퓨터에서 한 번 실행합니다:
   ```bash
   cd shorts && pip install -r requirements.txt
   YOUTUBE_CLIENT_ID=... YOUTUBE_CLIENT_SECRET=... python -m studio auth
   ```
   나온 값을 Secrets에 `YOUTUBE_CLIENT_ID`, `YOUTUBE_CLIENT_SECRET`, `YOUTUBE_REFRESH_TOKEN`으로 저장합니다.
4. Google의 **YouTube API Services 감사(Audit and Quota Extension Form)**를 신청합니다. 승인 전에는 API로 올린 영상이 비공개로 잠기고, 승인까지 몇 주 걸릴 수 있습니다.
5. 승인되면 `channel.json`에서 `"upload": "api"`로 바꿉니다. 영상이 '비공개 + 예약 공개'로 올라가므로 공개 전에 YouTube 스튜디오에서 확인할 수 있습니다. 검수까지 생략하려면 `"review": "auto"`로 바꾸면 됩니다(권장하지 않음).

## 명령어 (shorts/ 폴더에서)

```bash
pip install -r requirements.txt            # ffmpeg와 한글 폰트(fonts-noto-cjk)도 필요
python -m studio check                     # 대본 검사
python -m studio preview episodes/X.json   # API 키 없이 무음 미리보기 → out/
python -m studio render episodes/X.json    # 실제 목소리로 렌더 (GOOGLE_TTS_API_KEY)
python -m studio plan --days 7             # 대본 7일치 생성 (ANTHROPIC_API_KEY)
python -m studio publish                   # 곧 공개될 영상 렌더 → GitHub Release
python -m studio report                    # 주간 리포트
python -m studio brand                     # 프로필·배너 이미지 다시 만들기
python -m studio voices                    # Google 한국어 목소리 목록
python -m studio auth                      # 자동 업로드용 토큰 발급
python -m pytest                           # 테스트
```

## 문제 해결

| 증상 | 확인할 것 |
| --- | --- |
| 영상 만들기 실패: `GOOGLE_TTS_API_KEY` | Secret 이름, Text-to-Speech API 사용 설정, 결제 계정 연결 |
| `voice ... not found` 같은 음성 오류 | `python -m studio voices`로 이름 확인 후 `voice.google_voice` 수정 |
| 대본 PR이 안 열림 | 4단계 2번 Actions 권한. 브랜치 `shorts/plan-...`는 만들어져 있으니 직접 PR을 열어도 됩니다 |
| 대본 생성 실패 | `ANTHROPIC_API_KEY`, 크레딧 잔액, 월 한도 |
| API로 올린 영상이 비공개로 잠김 | 감사 전입니다. `upload`를 `manual`로 되돌리세요 |
| 예약 실행이 멈춤 | 공개 저장소는 오래 활동이 없으면 GitHub가 예약 실행을 끕니다. Actions 탭에서 다시 켜세요 |

## 코드 구성

| 경로 | 역할 |
| --- | --- |
| `studio/writer.py` | Claude: 웹 검색 사실 확인 → 구조화된 JSON 대본, 주제 추천 |
| `studio/planner.py` | 요일별 시리즈·주제 선택, 청약 데이터 요약, 검수 PR 본문 |
| `studio/tts.py` | Google TTS / ElevenLabs / 무음(미리보기), 캐시 |
| `studio/visuals.py` | 카드·자막·리스트·배경·채널 아트 (Pillow) |
| `studio/render.py` | ffmpeg 렌더 (장면별 영상 + 샘플 단위로 맞춘 음성 + 배경음악 + 음량 정규화) |
| `studio/broll.py` | Pexels 세로 배경 영상 검색·캐시 |
| `studio/youtube.py` | 예약 업로드(OAuth), 채널 통계, 토큰 발급 |
| `studio/publish.py` | 렌더 → GitHub Release → (업로드), 중복 방지 상태 관리 |
| `studio/report.py` | 주간 리포트 |
| `../.github/workflows/shorts-*.yml` | 대본(일요일), 렌더(머지·매일), 리포트(월요일), PR 검사 |
