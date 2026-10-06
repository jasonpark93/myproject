"""에피소드 → 세로 MP4(1080x1920, 30fps).

장면마다 무음 영상(배경 + 카드 + 자막)을 만들어 이어 붙이고, 음성은 장면 길이에 맞춰 샘플 단위로
정확히 이어 붙인 뒤 마지막에 한 번만 합친다. (AAC 조각을 이어 붙일 때 생기는 '틱' 소리를 피하기 위해)
"""

from __future__ import annotations

import hashlib
import math
import subprocess
import wave
from dataclasses import dataclass, field
from pathlib import Path

from . import ROOT, broll as broll_mod, textutil, visuals

FPS = 30
SAMPLE_RATE = 48000
TAIL = 0.25  # 장면 끝 여백
MIN_SCENE = 1.2
LIST_PAGE = 5  # 리스트 한 화면 항목 수
BGM_VOLUME = {"narrated": 0.10, "list": 0.45}


class RenderError(RuntimeError):
    pass


@dataclass
class SceneAssets:
    duration: float
    card: Path
    background: tuple[str, Path]
    audio: Path | None = None
    subtitles: list[tuple[Path, float, float]] = field(default_factory=list)
    fade_in: bool = True


def list_durations(script: dict) -> list[float]:
    """리스트형: 제목 화면 + 5개씩 끊은 페이지. 항목이 많을수록 오래 보여준다."""
    items = script.get("items") or []
    pages = [items[i : i + LIST_PAGE] for i in range(0, len(items), LIST_PAGE)]
    return [1.8] + [2.0 + 1.0 * len(page) for page in pages]


def frames(seconds: float) -> int:
    return max(1, math.ceil(seconds * FPS - 1e-6))


class Renderer:
    def __init__(self, cfg: dict, *, tts, workdir: Path, pexels=None, bgm_dir: Path | None = None, ffmpeg: str = "ffmpeg"):
        self.cfg = cfg
        self.tts = tts
        self.pexels = pexels
        self.workdir = Path(workdir)
        self.bgm_dir = bgm_dir if bgm_dir is not None else ROOT / "assets" / "bgm"
        self.ffmpeg = ffmpeg
        self.notes: list[str] = []

    # ---------- 공개 API ----------
    def render(self, episode: dict, out_path: Path) -> dict:
        self.workdir.mkdir(parents=True, exist_ok=True)
        self.notes = []
        if episode["format"] == "list":
            scenes = self._list_scenes(episode)
        else:
            scenes = self._narrated_scenes(episode)

        clips = []
        for i, scene in enumerate(scenes):
            clip = self.workdir / f"scene_{i:02d}.mp4"
            self._render_scene(scene, clip)
            clips.append(clip)
        video = self.workdir / "video.mp4"
        self._concat(clips, video)
        voice = self.workdir / "voice.wav"
        self._voice_track(scenes, voice)
        out_path = Path(out_path)
        out_path.parent.mkdir(parents=True, exist_ok=True)
        self._mux(video, voice, self._pick_bgm(episode["id"]), episode["format"], out_path)
        cover = out_path.with_suffix(".jpg")
        self._run([self.ffmpeg, "-y", "-hide_banner", "-loglevel", "error", "-ss", "0.6", "-i", str(out_path), "-frames:v", "1", "-q:v", "3", str(cover)])
        total = sum(s.duration for s in scenes)
        return {"duration": round(total, 2), "scenes": len(scenes), "cover": cover, "notes": list(self.notes)}

    # ---------- 장면 구성 ----------
    def _narrated_scenes(self, episode: dict) -> list[SceneAssets]:
        cfg = self.cfg
        script = episode["script"]
        label = cfg["series"][episode["series"]]["label"]
        speed = cfg["voice"]["speed"]
        bg_image = self._background_image(episode["id"])

        prepared = []
        for i, scene in enumerate(script["scenes"]):
            text = textutil.strip_marks(scene["narration"]).strip()
            audio = self.workdir / f"voice_{i:02d}.wav"
            speech = self._speech(text, speed, audio)
            duration = frames(max(speech + TAIL, MIN_SCENE)) / FPS
            prepared.append((scene, audio, speech, duration))

        total = sum(p[3] for p in prepared)
        elapsed = 0.0
        used_clips: set[int] = set()
        scenes = []
        for i, (scene, audio, speech, duration) in enumerate(prepared):
            elapsed += duration
            card = self.workdir / f"card_{i:02d}.png"
            visuals.scene_card(
                brand=cfg["brand"],
                channel=cfg["channel"]["name"],
                series_label=label,
                headline=str(scene.get("headline", "")).strip(),
                visual=scene.get("visual") or {},
                progress=elapsed / total,
                dim=self.pexels is not None,
            ).save(card)
            subtitles = self._subtitles(scene["narration"], speech, duration, i)
            background = self._broll(scene, episode["id"], duration, used_clips) or ("image", bg_image)
            self._pad_audio(audio, duration)
            scenes.append(SceneAssets(duration, card, background, audio, subtitles, fade_in=i > 0))
        return scenes

    def _list_scenes(self, episode: dict) -> list[SceneAssets]:
        cfg = self.cfg
        script = episode["script"]
        label = cfg["series"][episode["series"]]["label"]
        bg = ("image", self._background_image(episode["id"]))
        durations = list_durations(script)
        scenes = []
        title_card = self.workdir / "card_00.png"
        visuals.list_title_card(
            brand=cfg["brand"],
            channel=cfg["channel"]["name"],
            series_label=label,
            list_title=script["list_title"],
            subtitle_text=str(script.get("subtitle", "")),
        ).save(title_card)
        scenes.append(SceneAssets(frames(durations[0]) / FPS, title_card, bg, fade_in=False))
        items = script["items"]
        for page_index, start in enumerate(range(0, len(items), LIST_PAGE), start=1):
            card = self.workdir / f"card_{page_index:02d}.png"
            visuals.list_page_card(
                brand=cfg["brand"],
                channel=cfg["channel"]["name"],
                list_title=script["list_title"],
                items=items[start : start + LIST_PAGE],
                start_number=start + 1,
                total=len(items),
            ).save(card)
            scenes.append(SceneAssets(frames(durations[page_index]) / FPS, card, bg))
        return scenes

    def _subtitles(self, narration: str, speech: float, duration: float, index: int) -> list[tuple[Path, float, float]]:
        chunks = textutil.chunk(narration)
        if not chunks:
            return []
        weights = [textutil.weight(c) for c in chunks]
        total = sum(weights) or 1.0
        result = []
        start = 0.0
        for k, (chunk_words, w) in enumerate(zip(chunks, weights)):
            end = duration if k == len(chunks) - 1 else start + speech * w / total
            path = self.workdir / f"sub_{index:02d}_{k:02d}.png"
            visuals.subtitle(chunk_words, self.cfg["brand"]).save(path)
            result.append((path, round(start, 3), round(end, 3)))
            start = end
        return result

    def _background_image(self, seed: str) -> Path:
        path = self.workdir / "background.png"
        if not path.exists():
            visuals.background(self.cfg["brand"], seed).save(path)
        return path

    def _broll(self, scene: dict, seed: str, duration: float, used: set[int]):
        if self.pexels is None:
            return None
        query = str(scene.get("broll") or "").strip() or broll_mod.default_query(seed + str(len(used)))
        try:
            found = self.pexels.find(query, seconds=duration, seed=seed, exclude=used)
        except broll_mod.BrollError as e:
            self.notes.append(f"배경 영상 검색 실패({query}): {e}")
            return None
        if not found:
            self.notes.append(f"배경 영상 없음: {query}")
            return None
        video_id, path = found
        used.add(video_id)
        return ("video", path)

    # ---------- 오디오 ----------
    def _speech(self, text: str, speed: float, out_wav: Path) -> float:
        """TTS → 48kHz 스테레오 WAV(속도 조절 포함). 발화 길이(초)를 돌려준다."""
        audio, ext = self.tts.synthesize(text)
        raw = out_wav.with_suffix(f".raw.{ext}")
        raw.write_bytes(audio)
        self._run(
            [self.ffmpeg, "-y", "-hide_banner", "-loglevel", "error", "-i", str(raw),
             "-af", f"atempo={speed:.3f}", "-ar", str(SAMPLE_RATE), "-ac", "2", "-sample_fmt", "s16", str(out_wav)]
        )
        with wave.open(str(out_wav), "rb") as w:
            return w.getnframes() / w.getframerate()

    def _pad_audio(self, path: Path, duration: float) -> None:
        """장면 길이와 샘플 수를 정확히 맞춘다 (모자라면 무음, 넘치면 자름)."""
        target = round(duration * SAMPLE_RATE)
        with wave.open(str(path), "rb") as w:
            params = w.getparams()
            data = w.readframes(w.getnframes())
        frame_size = params.sampwidth * params.nchannels
        data = data[: target * frame_size].ljust(target * frame_size, b"\x00")
        with wave.open(str(path), "wb") as w:
            w.setparams(params)
            w.writeframes(data)

    def _voice_track(self, scenes: list[SceneAssets], out: Path) -> None:
        with wave.open(str(out), "wb") as dst:
            dst.setnchannels(2)
            dst.setsampwidth(2)
            dst.setframerate(SAMPLE_RATE)
            for scene in scenes:
                samples = round(scene.duration * SAMPLE_RATE)
                if scene.audio:
                    with wave.open(str(scene.audio), "rb") as src:
                        dst.writeframes(src.readframes(samples))
                else:
                    dst.writeframes(b"\x00\x00\x00\x00" * samples)

    def _pick_bgm(self, seed: str) -> Path | None:
        if not self.bgm_dir or not self.bgm_dir.exists():
            return None
        tracks = sorted(p for p in self.bgm_dir.iterdir() if p.suffix.lower() in (".mp3", ".m4a", ".wav", ".ogg"))
        if not tracks:
            return None
        return tracks[int(hashlib.sha256(seed.encode("utf-8")).hexdigest(), 16) % len(tracks)]

    # ---------- ffmpeg ----------
    def _render_scene(self, scene: SceneAssets, out: Path) -> None:
        d = f"{scene.duration:.3f}"
        n = frames(scene.duration)
        cmd = [self.ffmpeg, "-y", "-hide_banner", "-loglevel", "error"]
        kind, bg_path = scene.background
        if kind == "video":
            cmd += ["-stream_loop", "-1", "-t", d, "-i", str(bg_path)]
            bg_filter = "[0:v]scale=1080:1920:force_original_aspect_ratio=increase,crop=1080:1920,setsar=1,fps=30,eq=saturation=0.9[bg]"
        else:
            cmd += ["-loop", "1", "-framerate", str(FPS), "-t", d, "-i", str(bg_path)]
            phase = int(hashlib.sha256(str(scene.card).encode()).hexdigest(), 16) % 628 / 100
            bg_filter = (
                f"[0:v]crop=1080:1920:x='(iw-1080)/2+(iw-1080)/2*sin(t*0.35+{phase:.2f})'"
                f":y='(ih-1920)/2+(ih-1920)/2*cos(t*0.27+{phase:.2f})',setsar=1,fps=30[bg]"
            )
        cmd += ["-loop", "1", "-framerate", str(FPS), "-t", d, "-i", str(scene.card)]
        for sub, _, _ in scene.subtitles:
            cmd += ["-loop", "1", "-framerate", str(FPS), "-t", d, "-i", str(sub)]

        card_filter = "[1:v]format=rgba" + (",fade=t=in:st=0:d=0.2:alpha=1" if scene.fade_in else "") + "[card]"
        graph = [bg_filter, card_filter, "[bg][card]overlay=0:0:format=auto[v0]"]
        last = "v0"
        for k, (_, start, end) in enumerate(scene.subtitles):
            graph.append(f"[{last}][{k + 2}:v]overlay=0:0:format=auto:enable='between(t,{start:.3f},{end - 0.001:.3f})'[v{k + 1}]")
            last = f"v{k + 1}"
        graph.append(f"[{last}]format=yuv420p[vout]")
        cmd += [
            "-filter_complex", ";".join(graph), "-map", "[vout]", "-an",
            "-c:v", "libx264", "-preset", "veryfast", "-crf", "20", "-pix_fmt", "yuv420p", "-r", str(FPS),
            "-frames:v", str(n), str(out),
        ]
        self._run(cmd)

    def _concat(self, clips: list[Path], out: Path) -> None:
        listing = self.workdir / "clips.txt"
        listing.write_text("".join(f"file '{c.resolve()}'\n" for c in clips), encoding="utf-8")
        self._run([self.ffmpeg, "-y", "-hide_banner", "-loglevel", "error", "-f", "concat", "-safe", "0", "-i", str(listing), "-c", "copy", str(out)])

    def _mux(self, video: Path, voice: Path, bgm: Path | None, fmt: str, out: Path) -> None:
        cmd = [self.ffmpeg, "-y", "-hide_banner", "-loglevel", "error", "-i", str(video), "-i", str(voice)]
        loudnorm = "loudnorm=I=-14:TP=-1.5:LRA=11"
        if bgm:
            cmd += ["-stream_loop", "-1", "-i", str(bgm)]
            graph = (
                f"[2:a]aformat=sample_rates={SAMPLE_RATE}:channel_layouts=stereo,volume={BGM_VOLUME[fmt]}[bgm];"
                f"[1:a][bgm]amix=inputs=2:duration=first:dropout_transition=0:normalize=0,{loudnorm}[aout]"
            )
        elif fmt == "narrated":
            graph = f"[1:a]{loudnorm}[aout]"
        else:
            graph = "[1:a]anull[aout]"
        cmd += [
            "-filter_complex", graph, "-map", "0:v", "-map", "[aout]",
            "-c:v", "copy", "-c:a", "aac", "-b:a", "160k", "-ar", str(SAMPLE_RATE), "-shortest",
            "-movflags", "+faststart", str(out),
        ]
        self._run(cmd)

    def _run(self, cmd: list[str]) -> None:
        result = subprocess.run(cmd, capture_output=True, text=True)
        if result.returncode != 0:
            tail = "\n".join(result.stderr.strip().splitlines()[-15:])
            raise RenderError(f"ffmpeg 실패 (종료 코드 {result.returncode}):\n{tail}")


def probe(path: Path, ffprobe: str = "ffprobe") -> dict:
    """테스트·검증용: 길이, 해상도, fps, 오디오 유무."""
    import json

    result = subprocess.run(
        [ffprobe, "-v", "error", "-show_entries", "format=duration:stream=codec_type,width,height,r_frame_rate", "-of", "json", str(path)],
        capture_output=True,
        text=True,
        check=True,
    )
    data = json.loads(result.stdout)
    video = next((s for s in data["streams"] if s["codec_type"] == "video"), {})
    num, _, den = video.get("r_frame_rate", "0/1").partition("/")
    return {
        "duration": float(data["format"]["duration"]),
        "width": video.get("width"),
        "height": video.get("height"),
        "fps": float(num) / float(den or 1),
        "audio": any(s["codec_type"] == "audio" for s in data["streams"]),
    }
