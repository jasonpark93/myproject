/* 오늘의 청약: 지역 필터와 D-day 갱신. 사이트는 하루 두 번 빌드되므로 그 사이 날짜가 바뀌어도 D-day가 맞도록 브라우저에서 다시 계산한다. */
(function () {
  "use strict";

  var LABELS = { open: "접수 중", upcoming: "접수 예정", winner_wait: "발표 대기", contract: "계약 진행", closed: "마감" };
  var WEEK = "일월화수목금토";

  function kstToday() {
    return new Date(Date.now() + 9 * 3600 * 1000).toISOString().slice(0, 10);
  }

  function daysBetween(later, earlier) {
    return Math.round((Date.parse(later) - Date.parse(earlier)) / 86400000);
  }

  function md(iso) {
    var d = new Date(Date.parse(iso));
    return (d.getUTCMonth() + 1) + "/" + d.getUTCDate() + "(" + WEEK.charAt(d.getUTCDay()) + ")";
  }

  function compute(el, today) {
    var s = el.getAttribute("data-start"), e = el.getAttribute("data-end") || s;
    var w = el.getAttribute("data-winner"), ce = el.getAttribute("data-contract-end");
    var n;
    if (s && today < s) return ["upcoming", "D-" + daysBetween(s, today)];
    if (s && today <= e) { n = daysBetween(e, today); return ["open", n === 0 ? "오늘 마감" : "마감 D-" + n]; }
    if (w && today <= w) { n = daysBetween(w, today); return ["winner_wait", n === 0 ? "오늘 발표" : "발표 D-" + n]; }
    if (ce && today <= ce) return ["contract", "계약 ~" + md(ce)];
    return ["closed", ""];
  }

  function refreshDday() {
    var today = kstToday();
    var items = document.querySelectorAll("[data-dday]");
    for (var i = 0; i < items.length; i++) {
      var el = items[i], result = compute(el, today);
      el.textContent = result[1];
      el.hidden = !result[1];
      var badge = el.parentNode && el.parentNode.querySelector("[data-status-badge]");
      if (badge) {
        badge.className = "badge badge-" + result[0];
        badge.textContent = LABELS[result[0]];
      }
    }
  }

  function setupRegionFilter() {
    var bar = document.querySelector("[data-region-filter]");
    if (!bar) return;
    var buttons = bar.querySelectorAll("button[data-region]");

    function apply(region) {
      for (var i = 0; i < buttons.length; i++) {
        buttons[i].setAttribute("aria-pressed", String(buttons[i].getAttribute("data-region") === region));
      }
      var sections = document.querySelectorAll("[data-section]");
      for (var s = 0; s < sections.length; s++) {
        var cards = sections[s].querySelectorAll(".card[data-region]"), shown = 0;
        for (var c = 0; c < cards.length; c++) {
          var visible = !region || cards[c].getAttribute("data-region") === region;
          cards[c].hidden = !visible;
          if (visible) shown++;
        }
        var empty = sections[s].querySelector("[data-empty]");
        if (empty) empty.hidden = shown > 0 || cards.length === 0;
      }
      try {
        var url = new URL(window.location.href);
        if (region) url.searchParams.set("region", region); else url.searchParams.delete("region");
        window.history.replaceState(null, "", url.toString());
      } catch (err) { /* 오래된 브라우저는 주소 갱신을 건너뛴다 */ }
    }

    bar.addEventListener("click", function (event) {
      var button = event.target.closest("button[data-region]");
      if (button) apply(button.getAttribute("data-region"));
    });

    var initial = "";
    try { initial = new URL(window.location.href).searchParams.get("region") || ""; } catch (err) { initial = ""; }
    if (initial && bar.querySelector('button[data-region="' + initial.replace(/[^a-z]/g, "") + '"]')) apply(initial);
  }

  refreshDday();
  setupRegionFilter();
})();
