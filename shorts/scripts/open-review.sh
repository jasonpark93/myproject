#!/usr/bin/env bash
# 새로 만든 대본을 검수용 PR로 올리거나(review=pr), master에 바로 반영하고 렌더를 시작한다(review=auto).
# 필요 환경변수: GH_TOKEN, DEFAULT_BRANCH, REVIEW_FILE
set -euo pipefail
cd "$(dirname "$0")/.."

REVIEW=$(python3 -c 'import json; print(json.load(open("channel.json")).get("review", "pr"))')
git add episodes topics.json
if git diff --cached --quiet; then
  echo "새 대본이 없습니다."
  exit 0
fi
STAMP=$(TZ=Asia/Seoul date +%Y%m%d-%H%M)

if [ "$REVIEW" = "auto" ]; then
  git commit -q -m "쇼츠 대본 자동 생성 ($STAMP)"
  git push -q origin "HEAD:${DEFAULT_BRANCH}"
  # GITHUB_TOKEN으로 한 push는 다른 워크플로를 깨우지 않으므로 렌더를 직접 실행한다.
  gh workflow run shorts-render.yml --ref "${DEFAULT_BRANCH}"
  echo "자동 모드: ${DEFAULT_BRANCH}에 반영하고 영상 만들기를 시작했습니다."
  exit 0
fi

BRANCH="shorts/plan-$STAMP"
git switch -q -c "$BRANCH"
git commit -q -m "쇼츠 대본 검수 요청 ($STAMP)"
git push -q origin "$BRANCH"
if gh pr create --base "${DEFAULT_BRANCH}" --head "$BRANCH" --title "🎬 쇼츠 대본 검수 ($STAMP)" --body-file "${REVIEW_FILE}"; then
  echo "검수용 PR을 열었습니다."
else
  echo "::warning::PR을 만들지 못했습니다. Settings → Actions → General → Workflow permissions에서 'Allow GitHub Actions to create and approve pull requests'를 켜거나, 브랜치 ${BRANCH}로 직접 PR을 만드세요."
fi
