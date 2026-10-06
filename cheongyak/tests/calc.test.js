// node --test tests/calc.test.js
const test = require("node:test");
const assert = require("node:assert/strict");
const G = require("../static/calc.js");

test("무주택기간 점수: 1년 미만 2점, 1년마다 2점, 15년 이상 32점", () => {
  assert.equal(G.homelessScore(-1), 0);
  assert.equal(G.homelessScore(0), 2);
  assert.equal(G.homelessScore(1), 4);
  assert.equal(G.homelessScore(14), 30);
  assert.equal(G.homelessScore(15), 32);
  assert.equal(G.homelessScore(30), 32);
});

test("부양가족 점수: 0명 5점, 6명 이상 35점", () => {
  assert.equal(G.dependentsScore(0), 5);
  assert.equal(G.dependentsScore(3), 20);
  assert.equal(G.dependentsScore(6), 35);
  assert.equal(G.dependentsScore(9), 35);
});

test("본인 통장 가입기간 점수", () => {
  assert.equal(G.accountScore(0), 1);
  assert.equal(G.accountScore(5), 1);
  assert.equal(G.accountScore(6), 2);
  assert.equal(G.accountScore(11), 2);
  assert.equal(G.accountScore(12), 3);
  assert.equal(G.accountScore(179), 16);
  assert.equal(G.accountScore(180), 17);
  assert.equal(G.accountScore(400), 17);
});

test("배우자 통장 점수(최대 3점)", () => {
  assert.equal(G.spouseScore(-1), 0);
  assert.equal(G.spouseScore(0), 1);
  assert.equal(G.spouseScore(11), 1);
  assert.equal(G.spouseScore(12), 2);
  assert.equal(G.spouseScore(23), 2);
  assert.equal(G.spouseScore(24), 3);
});

test("합계: 통장 점수는 배우자 포함 17점을 넘지 않는다", () => {
  const max = G.total({ homelessYears: 15, dependents: 6, accountMonths: 180, spouseMonths: 24 });
  assert.deepEqual([max.homeless, max.dependents, max.account, max.total], [32, 35, 17, 84]);
  const r = G.total({ homelessYears: 8, dependents: 3, accountMonths: 96, spouseMonths: 24 });
  assert.deepEqual([r.homeless, r.dependents, r.accountOwn, r.accountSpouse, r.account, r.total], [18, 20, 10, 3, 13, 51]);
});

test("무주택기간 계산: 만 30세, 30세 전 혼인, 주택 처분일 기준", () => {
  const d = G.parseDate;
  const ref = d("2026-10-06");
  assert.equal(G.homelessYears(d("1990-05-10"), null, null, ref), 6); // 2020-05-10부터
  assert.equal(G.homelessYears(d("1990-05-10"), d("2018-03-01"), null, ref), 8); // 30세 전 혼인
  assert.equal(G.homelessYears(d("1990-05-10"), d("2022-03-01"), null, ref), 6); // 30세 뒤 혼인은 영향 없음
  assert.equal(G.homelessYears(d("1990-05-10"), null, d("2024-01-01"), ref), 2); // 처분일부터 다시
  assert.equal(G.homelessYears(d("2000-01-01"), null, null, ref), -1); // 만 30세 미만 미혼
  assert.equal(G.homelessYears(d("1996-10-07"), null, null, ref), -1); // 30세 생일 하루 전
  assert.equal(G.homelessYears(d("1996-10-06"), null, null, ref), 0); // 30세 생일 당일
});
