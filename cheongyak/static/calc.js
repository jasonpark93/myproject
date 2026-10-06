/* 청약 가점 계산기 (민영주택 가점제, 주택공급에 관한 규칙 별표1 기준, 2024.3.25 배우자 통장 합산 반영) */
(function (root) {
  "use strict";

  /** 무주택기간 점수. years: 만 연수(0~), -1이면 유주택자 또는 만 30세 미만 미혼(0점). */
  function homelessScore(years) {
    if (years < 0) return 0;
    return 2 * (Math.min(Math.floor(years), 15) + 1);
  }

  /** 부양가족 수 점수: 0명 5점, 1명당 5점, 6명 이상 35점. */
  function dependentsScore(count) {
    return 5 + 5 * Math.min(Math.max(Math.floor(count), 0), 6);
  }

  /** 본인 청약통장 가입기간 점수. months: 가입 개월 수. */
  function accountScore(months) {
    if (months < 6) return 1;
    if (months < 12) return 2;
    return Math.min(Math.floor(months / 12), 15) + 2;
  }

  /** 배우자 통장 가입기간 점수(최대 3점). months: -1이면 배우자 없음/통장 없음. */
  function spouseScore(months) {
    if (months < 0) return 0;
    if (months < 12) return 1;
    if (months < 24) return 2;
    return 3;
  }

  function total(input) {
    var homeless = homelessScore(input.homelessYears);
    var dependents = dependentsScore(input.dependents);
    var own = accountScore(input.accountMonths);
    var spouse = spouseScore(input.spouseMonths);
    var account = Math.min(own + spouse, 17);
    return { homeless: homeless, dependents: dependents, accountOwn: own, accountSpouse: spouse, account: account, total: homeless + dependents + account };
  }

  function parseDate(text) {
    if (!text) return null;
    var p = String(text).split("-");
    if (p.length !== 3) return null;
    var d = new Date(Number(p[0]), Number(p[1]) - 1, Number(p[2]));
    return isNaN(d.getTime()) ? null : d;
  }

  function fullYears(from, to) {
    var years = to.getFullYear() - from.getFullYear();
    if (to.getMonth() < from.getMonth() || (to.getMonth() === from.getMonth() && to.getDate() < from.getDate())) years--;
    return years;
  }

  /**
   * 무주택기간(만 연수). 만 30세가 된 날부터 세되 30세 전에 혼인했으면 혼인신고일부터,
   * 그 뒤에 집을 처분했다면 처분일부터 센다. 아직 셀 수 없으면(만 30세 미만 미혼) -1.
   */
  function homelessYears(birth, marriage, disposal, reference) {
    var start = new Date(birth.getFullYear() + 30, birth.getMonth(), birth.getDate());
    if (marriage && marriage < start) start = marriage;
    if (disposal && disposal > start) start = disposal;
    if (reference < start) return -1;
    return fullYears(start, reference);
  }

  var api = {
    homelessScore: homelessScore,
    dependentsScore: dependentsScore,
    accountScore: accountScore,
    spouseScore: spouseScore,
    total: total,
    homelessYears: homelessYears,
    parseDate: parseDate
  };

  if (typeof module === "object" && module.exports) {
    module.exports = api;
    return;
  }
  root.Gajeom = api;

  var form = document.getElementById("calc");
  if (!form) return;
  var $ = function (id) { return document.getElementById(id); };
  var fields = { h: $("homeless"), d: $("dependents"), a: $("account"), s: $("spouse") };

  function render() {
    var r = total({
      homelessYears: Number(fields.h.value),
      dependents: Number(fields.d.value),
      accountMonths: Number(fields.a.value),
      spouseMonths: Number(fields.s.value)
    });
    $("total").textContent = r.total;
    $("meter").style.width = (r.total / 84 * 100).toFixed(1) + "%";
    $("r-homeless").textContent = r.homeless + " / 32점";
    $("r-dependents").textContent = r.dependents + " / 35점";
    $("r-account").textContent = r.account + " / 17점" + (r.accountSpouse ? " (배우자 +" + r.accountSpouse + ")" : "");
  }

  // 공유 링크(?h=10&d=3&a=180&s=24)로 들어오면 값을 채운다.
  try {
    var params = new URL(window.location.href).searchParams;
    Object.keys(fields).forEach(function (key) {
      var value = (params.get(key) || "").replace(/[^0-9-]/g, "");
      if (value && fields[key].querySelector('option[value="' + value + '"]')) fields[key].value = value;
    });
  } catch (err) { /* 무시 */ }

  form.addEventListener("change", render);
  render();

  var ref = $("refdate");
  if (ref && !ref.value) {
    var now = new Date();
    ref.value = now.getFullYear() + "-" + String(now.getMonth() + 1).padStart(2, "0") + "-" + String(now.getDate()).padStart(2, "0");
  }
  $("homeless-apply").addEventListener("click", function () {
    var birth = parseDate($("birth").value), reference = parseDate(ref.value);
    var out = $("homeless-result");
    if (!birth || !reference) { out.textContent = "생년월일과 기준일을 입력하세요."; return; }
    var disposal = parseDate($("disposal").value);
    if (disposal && disposal > reference) { out.textContent = "주택 처분일이 기준일보다 늦습니다. 날짜를 확인하세요."; return; }
    var years = homelessYears(birth, parseDate($("marriage").value), disposal, reference);
    fields.h.value = String(Math.min(years, 15));
    out.textContent = years < 0
      ? "아직 무주택기간을 셀 수 없습니다(만 30세 미만 미혼). 0점으로 반영했습니다."
      : "무주택기간 " + years + "년 → " + homelessScore(years) + "점으로 반영했습니다.";
    render();
  });

  $("share").addEventListener("click", function () {
    var url = new URL(window.location.href);
    Object.keys(fields).forEach(function (key) { url.searchParams.set(key, fields[key].value); });
    var msg = $("share-msg");
    var done = function () { msg.textContent = "결과 링크를 복사했습니다."; };
    if (navigator.clipboard && navigator.clipboard.writeText) {
      navigator.clipboard.writeText(url.toString()).then(done, function () { msg.textContent = url.toString(); });
    } else {
      msg.textContent = url.toString();
    }
    window.history.replaceState(null, "", url.toString());
  });
})(typeof window !== "undefined" ? window : this);
