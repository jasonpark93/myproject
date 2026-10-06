#!/usr/bin/env bash
# 수집한 데이터(data/notices.json)를 master가 아닌 별도 브랜치(cheongyak-data)에 보관한다.
# master 커밋 기록을 깨끗하게 두면서, 한 번 수집한 지난 공고 페이지를 계속 살려 두기 위해서다.
#
#   restore  원격 데이터 브랜치의 notices.json을 data/로 받아 온다 (브랜치가 없으면 첫 실행)
#   save     data/notices.json이 바뀌었으면 데이터 브랜치에 커밋해서 push 한다
set -euo pipefail

BRANCH="${DATA_BRANCH:-cheongyak-data}"
FILE="data/notices.json"
cd "$(dirname "$0")/.."

case "${1:-}" in
  restore)
    set +e
    git ls-remote --exit-code --heads origin "$BRANCH" >/dev/null
    code=$?
    set -e
    if [ "$code" -eq 2 ]; then
      echo "데이터 브랜치($BRANCH)가 아직 없습니다. 첫 실행이므로 과거 공고부터 모읍니다."
      exit 0
    elif [ "$code" -ne 0 ]; then
      echo "원격 저장소를 확인하지 못했습니다 (git ls-remote 종료 코드 $code)." >&2
      exit 1
    fi
    git fetch --quiet --depth=1 origin "+refs/heads/$BRANCH:refs/remotes/origin/$BRANCH"
    mkdir -p data
    git show "refs/remotes/origin/$BRANCH:notices.json" > "$FILE"
    echo "데이터 브랜치에서 notices.json을 불러왔습니다 ($(wc -c < "$FILE") bytes)."
    ;;
  save)
    [ -f "$FILE" ] || { echo "$FILE 파일이 없습니다." >&2; exit 1; }
    blob=$(git hash-object -w "$FILE")
    tree=$(printf '100644 blob %s\tnotices.json\n' "$blob" | git mktree)
    parent=$(git rev-parse -q --verify "refs/remotes/origin/$BRANCH" || true)
    if [ -n "$parent" ] && [ "$(git rev-parse "$parent^{tree}")" = "$tree" ]; then
      echo "데이터 변경 없음"
      exit 0
    fi
    message="청약 데이터 업데이트 $(TZ=Asia/Seoul date '+%Y-%m-%d %H:%M')"
    commit=$(git commit-tree "$tree" ${parent:+-p "$parent"} -m "$message")
    git push --quiet origin "$commit:refs/heads/$BRANCH"
    git update-ref "refs/remotes/origin/$BRANCH" "$commit"
    echo "데이터 브랜치에 저장했습니다: $message"
    ;;
  *)
    echo "사용법: $0 restore|save" >&2
    exit 2
    ;;
esac
