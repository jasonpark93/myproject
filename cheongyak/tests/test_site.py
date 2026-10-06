import json
import re
import xml.etree.ElementTree as ET

import pytest

from app import ROOT, site


def build(tmp_path, cfg, db, today, now, **kwargs):
    out = tmp_path / "dist"
    result = site.build(cfg, db, out, today=today, now=now, **kwargs)
    return out, result


def read(out, path):
    return (out / path).read_text(encoding="utf-8")


def test_writes_all_pages(tmp_path, cfg, db, today, now):
    out, result = build(tmp_path, cfg, db, today, now)
    for path in [
        "index.html",
        "apt/2099000001/index.html",
        "apt/2099000001/calendar.ics",
        "remndr/2099100001/index.html",
        "urbty/2099200001/index.html",
        "region/index.html",
        "region/seoul/index.html",
        "region/gyeonggi/index.html",
        "calculator/index.html",
        "guide/index.html",
        "about/index.html",
        "privacy/index.html",
        "404.html",
        "sitemap.xml",
        "robots.txt",
        "feed.xml",
        "calendar.ics",
        "static/style.css",
        "static/app.js",
        "static/calc.js",
    ]:
        assert (out / path).is_file(), path
    assert not (out / "ads.txt").exists()
    assert result["notices"] == 7
    assert result["counts"]["open"] == 2 and result["counts"]["upcoming"] == 2


def test_links_respect_base_path(tmp_path, cfg, db, today, now):
    out, _ = build(tmp_path, cfg, db, today, now)
    home = read(out, "index.html")
    assert 'href="/myproject/apt/2099000001/"' in home
    assert re.search(r'href="/myproject/static/style\.css\?v=[0-9a-f]{10}"', home)
    assert '<link rel="canonical" href="https://example.github.io/myproject/">' in home
    assert 'href="webcal://example.github.io/myproject/calendar.ics"' in home
    notice = read(out, "apt/2099000001/index.html")
    assert '<link rel="canonical" href="https://example.github.io/myproject/apt/2099000001/">' in notice
    assert 'href="/myproject/apt/2099000001/calendar.ics"' in notice


def test_home_sections(tmp_path, cfg, db, today, now):
    out, _ = build(tmp_path, cfg, db, today, now)
    home = read(out, "index.html")
    open_section = home.split('id="open"')[1].split("</section>")[0]
    assert "데모 오피스텔 스퀘어" in open_section and "샘플 예시아파트 1단지" in open_section
    # 마감이 빠른 공고가 먼저
    assert open_section.index("데모 오피스텔 스퀘어") < open_section.index("샘플 예시아파트 1단지")
    assert "모의 레이크시티" in home.split('id="winner"')[1].split("</section>")[0]
    assert "예제마을 센트럴" not in home  # 30일보다 오래전에 마감


def test_notice_page_content(tmp_path, cfg, db, today, now):
    out, _ = build(tmp_path, cfg, db, today, now)
    page = read(out, "apt/2099000001/index.html")
    assert "<title>샘플 예시아파트 1단지 청약 일정·분양가 (서울) | 오늘의 청약</title>" in page
    assert "1순위 해당지역" in page and "진행 중" in page
    assert "18억 2,000만원" in page and "3,969만원" in page
    assert "투기과열지구" in page and "분양 홈페이지" in page
    assert 'href="https://www.applyhome.co.kr/' in page
    jsonld = re.search(r'<script type="application/ld\+json">(.*?)</script>', page).group(1)
    crumbs = json.loads(jsonld)["itemListElement"]
    assert [c["name"] for c in crumbs] == ["홈", "서울", "샘플 예시아파트 1단지"]


def test_untrusted_text_is_escaped(tmp_path, cfg, db, today, now):
    db["notices"]["apt/2099000001"]["name"] = '<script>alert("x")</script>'
    out, _ = build(tmp_path, cfg, db, today, now)
    page = read(out, "apt/2099000001/index.html")
    assert '<script>alert("x")</script>' not in page
    assert "&lt;script&gt;" in page
    assert '<script>alert("x")' not in read(out, "index.html")


def test_feed_and_sitemap_are_valid_xml(tmp_path, cfg, db, today, now):
    out, _ = build(tmp_path, cfg, db, today, now)
    sitemap = ET.parse(out / "sitemap.xml").getroot()
    locs = [e.text for e in sitemap.iter("{http://www.sitemaps.org/schemas/sitemap/0.9}loc")]
    assert "https://example.github.io/myproject/apt/2099000001/" in locs
    assert "https://example.github.io/myproject/calculator/" in locs
    feed = ET.parse(out / "feed.xml").getroot()
    assert len(feed.findall("./channel/item")) == 7
    assert "Sitemap: https://example.github.io/myproject/sitemap.xml" in read(out, "robots.txt")


def test_calendar_follows_rfc5545(tmp_path, cfg, db, today, now):
    out, _ = build(tmp_path, cfg, db, today, now)
    raw = (out / "calendar.ics").read_bytes()
    assert raw.endswith(b"\r\n")
    lines = raw.split(b"\r\n")[:-1]
    assert all(len(line) <= 75 for line in lines)
    assert b"\n" not in raw.replace(b"\r\n", b"")
    text = raw.decode("utf-8").replace("\r\n ", "")  # 접힌 줄 펴기
    assert "SUMMARY:[서울] 샘플 예시아파트 1단지 1순위 해당지역 접수" in text
    assert "DTSTART;VALUE=DATE:20261026\r\nDTEND;VALUE=DATE:20261029" in text  # 계약 10/26~10/28
    # 지난 14일보다 오래된 일정은 빠진다
    assert "예제마을 센트럴 특별공급" not in text


def test_adsense_settings(tmp_path, make_cfg, db, today, now):
    cfg = make_cfg(adsense_client="ca-pub-1234567890123456", adsense_slots={"top": "1111111111", "in_article": "2222222222"})
    out, _ = build(tmp_path, cfg, db, today, now)
    assert read(out, "ads.txt") == "google.com, pub-1234567890123456, DIRECT, f08c47fec0942fa0\n"
    home = read(out, "index.html")
    assert "adsbygoogle.js?client=ca-pub-1234567890123456" in home
    assert 'data-ad-slot="1111111111"' in home
    assert 'data-ad-slot="2222222222"' in read(out, "apt/2099000001/index.html")


def test_demo_mode_is_not_indexable(tmp_path, make_cfg, db, today, now):
    cfg = make_cfg(adsense_client="ca-pub-1234567890123456")
    out, _ = build(tmp_path, cfg, db, today, now, demo=True)
    home = read(out, "index.html")
    assert '<meta name="robots" content="noindex, nofollow">' in home
    assert "샘플 데이터로 만든 미리보기" in home
    assert "adsbygoogle" not in home
    assert "Disallow: /" in read(out, "robots.txt")
    assert not (out / "sitemap.xml").exists()
    assert not (out / "ads.txt").exists()


def test_empty_database_still_builds(tmp_path, cfg, today, now):
    out, result = build(tmp_path, cfg, {"updated_at": None, "notices": {}}, today, now)
    assert result["notices"] == 0
    assert "오늘 접수 중인 청약이 없습니다." in read(out, "index.html")
    assert "아직 수집된 공고가 없습니다." in read(out, "region/index.html")


def test_refuses_to_wipe_project_folder(cfg, db, today, now):
    for target in (ROOT, ROOT.parent, ROOT / ".."):
        with pytest.raises(ValueError):
            site.build(cfg, db, target, today=today, now=now)
    assert (ROOT / "app" / "site.py").exists()
