# 오늘의 청약: 자동으로 돌아가는 청약 정보 사이트

공공데이터포털의 **청약홈 분양정보 API**를 하루 두 번 자동으로 받아서, 전국 아파트·무순위(줍줍)·오피스텔 청약 공고마다
일정·분양가 페이지를 만들고 **GitHub Pages에 무료로 배포**합니다. 검색으로 들어오는 방문자에게 광고(구글 애드센스)를 보여주는 방식으로 수익을 냅니다.
한 번 설정해 두면 사람이 할 일은 거의 없습니다.

```
[매일 06:17 / 18:17]  GitHub Actions
   │  ① 공공데이터 API 수집 (새 공고·정정공고 반영)
   │  ② 공고별·지역별 페이지, 가점 계산기, 사이트맵, RSS, 캘린더(.ics) 생성
   │  ③ GitHub Pages 배포
   └  ④ (선택) 새 공고를 텔레그램 채널에 자동 게시
            ↓
   구글·네이버 검색 유입 → 애드센스 광고 수익
```

## 왜 이 아이템을 골랐나

| 방식 | 비용 | 자동화 | 걸리는 점 |
| --- | --- | --- | --- |
| **공공데이터 정보 사이트 + 광고 (이 프로젝트)** | 0원 (도메인 선택 시 연 1~2만 원) | 수집·게시·배포 전부 자동 | 검색 노출까지 몇 달 걸림 |
| AI 유튜브 쇼츠 자동 생성 | 도구 구독·크레딧 | 높음 | 유튜브가 2025년 7월 정책 개정으로 대량 생산·반복 콘텐츠를 수익 창출에서 명확히 제외 |
| 블로그 자동 포스팅 (제휴 링크) | 낮음 | 높음 | 검색엔진 스팸 정책에 걸리면 노출이 막힘 |
| 주식·코인 자동매매 | 투자금 | 높음 | 원금 손실 위험 |
| 위탁판매(스마트스토어) 자동화 | 중간 | 중간 | 주문·CS·반품은 결국 사람이 처리 |

청약 정보를 고른 이유는 다음과 같습니다.

- 데이터가 **공식·무료**이고, 공공데이터법상 영리 목적 이용도 허용됩니다 (출처 표시는 사이트 하단에 들어가 있음).
- 새 공고가 매주 나오므로 **페이지가 자동으로 계속 늘어납니다**. 사람이 블로그 글을 쓰는 경쟁자보다 빠르고 빠짐없이 올라갑니다.
- 단지 이름으로 검색하는 수요가 크고, 부동산은 광고 단가가 높은 분야입니다.
- 서버가 필요 없는 정적 사이트라 **운영비가 0원**이고, 서버가 멈출 걱정도 없습니다.

## 비용

| 항목 | 비용 |
| --- | --- |
| GitHub Actions (공개 저장소) | 무료 |
| GitHub Pages 호스팅 | 무료 |
| 공공데이터포털 API | 무료 |
| 텔레그램 봇·채널 | 무료 |
| 도메인 (선택, 애드센스 승인에 사실상 필요) | 연 1~2만 원 내외 |

## 시작하기 (약 30분)

### 1단계: 공공데이터 API 키 받기

1. [data.go.kr](https://www.data.go.kr) 회원가입 후 로그인
2. **한국부동산원_청약홈 분양정보 조회 서비스**를 검색해서 **활용신청** (개발계정은 보통 바로 자동 승인)
3. 마이페이지 → 데이터활용 → Open API → 개발계정에서 **일반 인증키(Decoding)** 복사
   (Encoding 키를 넣어도 됩니다. 승인 직후에는 키가 동작하기까지 1~2시간 걸릴 수 있습니다.)

### 2단계: GitHub에 키 등록

저장소 **Settings → Secrets and variables → Actions → New repository secret**

- Name: `DATA_GO_KR_API_KEY`
- Secret: 1단계에서 복사한 인증키

### 3단계: GitHub Pages 켜기

저장소 **Settings → Pages → Build and deployment → Source**를 **GitHub Actions**로 바꿉니다.

### 4단계: 실행

이 작업 브랜치를 `master`에 머지하면 자동으로 실행됩니다. 바로 돌려 보려면 **Actions 탭 → 오늘의 청약 사이트 → Run workflow**.

몇 분 뒤 **https://jasonpark93.github.io/myproject/** 에서 사이트를 볼 수 있습니다.
첫 실행은 최근 1년치 공고를 모읍니다. 주택형별 분양가는 하루 API 호출량을 넘지 않도록 며칠에 걸쳐 채워집니다.

## 수익이 나게 만들기

1. **검색엔진 등록 (가장 중요)**
   - [Google Search Console](https://search.google.com/search-console): 사이트 추가 → 'HTML 태그' 방식의 content 값을 `config.json`의 `google_site_verification`에 넣기 → `sitemap.xml` 제출
   - [네이버 서치어드바이저](https://searchadvisor.naver.com): 사이트 등록 → `naver_site_verification`에 값 넣기 → 사이트맵과 RSS(`feed.xml`) 제출. 국내 검색 점유율이 높아 꼭 해야 합니다.
2. **도메인 연결 (권장)**: 도메인을 산 뒤 **Settings → Pages → Custom domain**에 입력합니다.
   사이트 주소는 Pages 설정을 자동으로 따라가므로 코드는 고칠 필요가 없습니다. (애드센스는 `ads.txt`를 도메인 최상위에서 확인하기 때문에 `github.io/myproject` 같은 하위 경로로는 승인받기 어렵습니다.)
3. **구글 애드센스**: [adsense.google.com](https://adsense.google.com)에서 사이트 신청 → 승인되면 `config.json`의 `adsense_client`에 `ca-pub-...` 입력 후 커밋.
   자동 광고 코드와 `ads.txt`가 알아서 들어갑니다. 원하는 위치에 고정 광고를 넣으려면 `adsense_slots`에 광고 단위 ID를 넣으세요.
   개인정보처리방침·사이트 소개 페이지는 이미 들어 있고, `contact_email`을 채워 두면 승인에 도움이 됩니다.
4. **텔레그램 채널 (선택, 재방문 독자 만들기)**
   - 텔레그램 `@BotFather` → `/newbot` → 봇 토큰 받기
   - 공개 채널을 만들고 봇을 관리자로 추가
   - Secrets에 `TELEGRAM_BOT_TOKEN`, `TELEGRAM_CHAT_ID`(예: `@내채널아이디`) 등록
   - `config.json`의 `telegram_channel_url`에 `https://t.me/내채널아이디` 입력 → 사이트에 구독 버튼이 생깁니다.
5. **(선택) 방문 통계**: GA4 측정 ID를 `ga4_measurement_id`에 넣습니다.

## 현실적인 기대치

- 새 사이트가 검색 결과에 자리 잡는 데 보통 **몇 달**이 걸립니다. 처음 몇 주는 방문자가 거의 없는 게 정상입니다.
- 광고 수익은 방문자 수에 비례합니다. 대략적인 감으로는 하루 방문 1,000회 정도면 월 수만~십수만 원 수준이 흔하지만, 광고 단가에 따라 차이가 큽니다 (추정치이며 보장되지 않습니다).
- 공고가 쌓일수록 검색 입구가 늘어나는 구조라 **시간이 지날수록 유리**합니다. 지난 공고 페이지도 지우지 않고 계속 남깁니다.

## 운영하면서 알아둘 것

- **평소에 할 일은 없습니다.** Actions가 실패하면 GitHub가 메일을 보내니 그때 로그를 확인하세요.
  수집이 실패해도 사이트는 기존 데이터로 계속 배포되고(D-day는 매일 갱신), 마지막 작업만 빨간색으로 표시됩니다.
- 수집 데이터는 `master`가 아니라 **`cheongyak-data` 브랜치**에 저장됩니다. `master` 커밋 기록이 지저분해지지 않습니다.
- GitHub는 공개 저장소에서 오랫동안 활동이 없으면 예약 실행을 멈출 수 있습니다. 그런 메일을 받으면 Actions 탭에서 다시 켜 주세요.
- API 응답 형식이 바뀐 것 같으면(수집 요약에 '정상 0건' 경고) 로컬에서 `python -m app inspect`로 실제 필드 이름을 확인하세요.
- GitHub Pages는 정보 사이트에 광고를 붙이는 용도로 널리 쓰이지만 쇼핑몰처럼 거래 중심 사이트는 허용하지 않습니다. 사이트가 커지면 Cloudflare Pages(무료, 상업 이용 가능)로 옮기는 것도 방법입니다. `dist/` 폴더를 그대로 올리면 됩니다.

## 설정 파일 (`config.json`)

| 키 | 설명 |
| --- | --- |
| `site_name`, `tagline` | 사이트 이름과 한 줄 소개 |
| `base_url` | 로컬 빌드용 주소. GitHub Actions에서는 Pages 주소를 자동으로 씁니다 |
| `contact_email` | 소개·개인정보처리방침 페이지에 표시할 문의 메일 |
| `adsense_client`, `adsense_slots` | 애드센스 게시자 ID(`ca-pub-...`)와 광고 단위 ID |
| `ga4_measurement_id` | GA4 측정 ID (`G-...`) |
| `google_site_verification`, `naver_site_verification` | 검색엔진 소유 확인 값 |
| `telegram_channel_url` | 사이트에 보여줄 텔레그램 채널 주소 |
| `kinds` | 수집할 공고 종류: `apt`(APT 분양), `remndr`(무순위·잔여세대), `urbty`(오피스텔·도시형·민간임대) |
| `backfill_days`, `refresh_days` | 첫 실행 때 모을 기간, 이후 매번 다시 확인할 기간(일) |
| `max_model_calls_per_run` | 한 번 실행할 때 주택형 정보를 조회할 최대 공고 수 (API 일일 한도 보호) |

## 로컬에서 실행하기

```bash
cd cheongyak
pip install -r requirements.txt

# API 키 없이 샘플 데이터로 디자인 미리보기
python -m app demo
cd dist-demo && python -m http.server 8000   # http://localhost:8000

# 실제 데이터로 빌드
export DATA_GO_KR_API_KEY=발급받은키
python -m app update && python -m app build
```

테스트: `pip install -r requirements-dev.txt && python -m pytest && node --test tests/calc.test.js`

## 코드 구성

| 경로 | 역할 |
| --- | --- |
| `app/applyhome.py` | 공공데이터포털(api.odcloud.kr) 클라이언트: 페이지 넘김, 재시도, 인증키 처리 |
| `app/normalize.py` | API 응답 → 공고/주택형 데이터 (공고 종류별로 다른 필드 이름 흡수) |
| `app/update.py`, `app/store.py` | 수집·병합·저장, 새 공고 판별 |
| `app/view.py`, `app/site.py` | 상태·D-day·요약문 계산, 정적 페이지·사이트맵·RSS·캘린더 생성 |
| `app/notify.py` | 텔레그램 알림 |
| `templates/`, `static/` | 페이지 템플릿, CSS, 지역 필터·D-day·가점 계산기 스크립트 |
| `scripts/data-branch.sh` | 수집 데이터를 `cheongyak-data` 브랜치에 저장·복원 |
| `../.github/workflows/cheongyak-site.yml` | 매일 두 번 실행하는 자동화 |

## 주의

- 회사에 다니고 있다면 취업규칙의 **겸업 규정**을 먼저 확인하세요.
- 광고 수익은 **종합소득세 신고 대상**입니다 (매년 5월).
- 사이트의 청약 정보는 참고용이며, 모든 공고 페이지에 청약홈 원문 링크와 확인 안내가 들어가 있습니다.
