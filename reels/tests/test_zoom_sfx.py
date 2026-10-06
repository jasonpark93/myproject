import numpy as np

from editor import FPS, SR, sfx, zoom

CFG = {"enabled": True, "punch": 1.15, "slow": 1.08, "max_punch": None, "mask_cuts": True, "mask_step": 1.06}


def sentences(roles, length=2.0):
    return [{"id": i + 1, "role": r, "zoom": "auto", "start": i * length + 0.1, "end": (i + 1) * length - 0.1} for i, r in enumerate(roles)]


def test_roles_map_to_alternating_zoom():
    s = sentences(["hook", "number", "explain", "explain", "twist", "cta"])
    assert zoom.resolve_types(s, CFG) == ["punch", "slow", "none", "slow", "punch", "slow"]


def test_max_punch_keeps_most_important():
    s = sentences(["hook", "explain", "number", "explain", "cta"])
    kinds = zoom.resolve_types(s, dict(CFG, max_punch=2))
    assert kinds.count("punch") == 2 and kinds[0] == "punch" and kinds[4] == "punch"


def test_explicit_zoom_wins():
    s = sentences(["hook", "hook"])
    s[1]["zoom"] = "punch"
    assert zoom.resolve_types(s, CFG) == ["punch", "punch"]
    assert zoom.resolve_types(s, dict(CFG, enabled=False)) == ["none", "none"]


def test_boundary_snaps_to_cut_and_curve_values():
    s = sentences(["hook", "explain"])
    bounds = zoom.boundaries(s, joins=[1.95], duration=4.0)
    assert bounds == [0.0, 1.95, 4.0]
    n = int(4.0 * FPS)
    z, events = zoom.curve(s, ["punch", "slow"], bounds, [1.95], n, CFG)
    assert z[0] == 1.15 and z[int(1.9 * FPS)] == 1.15
    assert z[int(2.0 * FPS)] < 1.01  # 문장이 바뀌면 원래 크기로
    assert 1.07 < z[n - 1] <= 1.08 + 1e-9  # 설명 문장 동안 천천히 108%까지
    assert [e["kind"] for e in events] == ["punch", "slow"]


def test_cut_inside_sentence_changes_framing():
    s = sentences(["explain"], length=4.0)
    bounds = [0.0, 4.0]
    z, _ = zoom.curve(s, ["none"], bounds, [2.0], int(4.0 * FPS), CFG)
    assert z[int(1.0 * FPS)] == 1.0 and abs(z[int(3.0 * FPS)] - 1.06) < 1e-9


def test_sfx_density_limit():
    events = [{"type": "ding", "t": t} for t in (1.0, 1.3, 2.0, 4.0, 6.0, 8.0)] + [{"type": "whoosh", "t": 2.1}]
    chosen = sfx.thin(events, max_per_10s=3)
    times = [e["t"] for e in chosen]
    assert len(chosen) == 3
    assert 2.1 in times  # 휙이 우선
    assert all(b - a >= 0.7 for a, b in zip(times, times[1:]))


def test_synth_sounds():
    for kind, fn in sfx.SYNTH.items():
        x = fn()
        assert x.dtype == np.float32 and 0.01 * SR < x.size < 1.0 * SR
        assert 0.85 <= np.abs(x).max() <= 0.91
        assert np.abs(x[-50:]).max() < 0.2  # 끝이 툭 끊기지 않는다
    gain = sfx.gain_db(sfx.synth_pop(), "pop", voice_rms_db=-12.0)
    assert 20 * np.log10(0.9) + gain <= sfx.PEAK_CAP_DB + 1e-6
