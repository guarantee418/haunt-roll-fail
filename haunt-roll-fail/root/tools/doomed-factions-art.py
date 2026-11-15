# Art for the Doomed Expansion's factions (Warriors Wake, Doomsayers Union, Whisperers Guild), from their print-and-play PDFs:
#   pdftoppm -r 300 -png <X>-TOKENSHEETfront.pdf t-<x>; pdftoppm -r 220 -png <X>-FACTIONBOARDfront.pdf b-<x> (and back, bb-<x>)
#   python3 doomed-factions-art.py <dir with those renders> <webp2/root/images/faction>
# - the board front, at the size of the other fan boards (1232 wide)
# - the title cut from the board's banner, the glyph and character art from the token sheet and the board
# - the warriors are the Turnlings' pieces (turnling-vulture, ...), with the empty and five-stack versions drawn from them
# - tokens and buildings cut round (or square) from the token sheet
import sys
import numpy as np
from PIL import Image, ImageDraw

src, out = sys.argv[1], sys.argv[2]

def save(im, path):
    im.save(path, lossless=True) if im.mode == "RGBA" else im.save(path, quality=88)

def board(x, name):
    im = Image.open(f"{src}/b-{x}-1.png").convert("RGB")
    save(im.resize((1232, round(im.size[1] * 1232 / im.size[0])), Image.LANCZOS), f"{out}/{name}")

def title(x, box, name, keep = False):
    im = Image.open(f"{src}/b-{x}-1.png").convert("RGB").crop(box)
    size = (920, round(920 * im.size[1] / im.size[0])) if keep else (920, 200)
    save(im.resize(size, Image.LANCZOS), f"{out}/{name}")

def hexagon(sheet, cx, cy, r, size, name):
    # a flat-topped hexagon of circumradius r
    import math
    h = round(r * math.sqrt(3) / 2)
    im = Image.open(f"{src}/{sheet}").convert("RGBA").crop((cx - r, cy - r, cx + r, cy + r))
    m = Image.new("L", im.size, 0)
    ImageDraw.Draw(m).polygon([(r + r * math.cos(math.pi / 3 * i), r + r * math.sin(math.pi / 3 * i)) for i in range(6)], fill=255)
    im.putalpha(m)
    im.resize((size, size), Image.LANCZOS).save(f"{out}/{name}", lossless=True)

def disc(sheet, cx, cy, r, size, name, square = False):
    im = Image.open(f"{src}/{sheet}").convert("RGBA").crop((cx - r, cy - r, cx + r, cy + r))
    m = Image.new("L", im.size, 0)
    if square:
        ImageDraw.Draw(m).rounded_rectangle((0, 0, 2 * r - 1, 2 * r - 1), radius = r // 8, fill=255)
    else:
        ImageDraw.Draw(m).ellipse((0, 0, 2 * r - 1, 2 * r - 1), fill=255)
    im.putalpha(m)
    im.resize((size, size), Image.LANCZOS).save(f"{out}/{name}", lossless=True)

def crop(path, box, w, h, name):
    im = Image.open(path).convert("RGB").crop(box)
    s = max(w / im.size[0], h / im.size[1])
    im = im.resize((round(im.size[0] * s), round(im.size[1] * s)), Image.LANCZOS)
    l, t = (im.size[0] - w) // 2, (im.size[1] - h) // 2
    save(im.crop((l, t, l + w, t + h)), f"{out}/{name}")

def warrior(piece, prefix, color = None):
    im = Image.open(piece).convert("RGBA")
    if color:
        # the Turnlings' pieces are black: black becomes the faction's color, white stays
        a = np.asarray(im).astype(float)
        lum = a[:, :, :3].mean(axis = 2, keepdims = True) / 255
        c = np.array(color, dtype = float).reshape(1, 1, 3)
        a[:, :, :3] = c + (255 - c) * lum
        im = Image.fromarray(a.astype(np.uint8), "RGBA")
    im.save(f"{out}/{prefix}.webp", lossless=True)
    a = np.asarray(im).astype(int)
    white = (a[:, :, 0] > 225) & (a[:, :, 1] > 225) & (a[:, :, 2] > 225)
    e = a.copy()
    e[:, :, :3] = np.where(white[:, :, None], a[:, :, :3], 34)
    empty = Image.fromarray(e.astype(np.uint8), "RGBA")
    empty.save(f"{out}/{prefix}-empty.webp", lossless=True)
    for p, suffix in ((im, "-x5"), (empty, "-x5-empty")):
        w, h = p.size
        stack = Image.new("RGBA", (w + 4 * (w // 4), h), (0, 0, 0, 0))
        for i in reversed(range(5)):
            stack.paste(p, (i * (w // 4), 0), p)
        stack.save(f"{out}/{prefix}{suffix}.webp", lossless=True)

hire = f"{out}/hireling"

which = sys.argv[3:] or ["ww", "du", "wg"]

if "ww" in which:
    o = f"{out}/wake"
    out, keep = o, out
    board("ww", "ww-board.webp")
    title("ww", (900, 140, 1636, 300), "ww-title.webp")
    disc("t-ww-1.png", 1190, 1120, 118, 90, "ww-despair.webp")
    disc("t-ww-1.png", 1507, 1722, 107, 86, "ww-glyph.webp", square = True)
    crop(f"{src}/bb-ww-1.png", (2160, 1430, 2530, 1880), 160, 194, "ww-char.webp")
    warrior(f"{hire}/turnling-vulture.webp", "ww-wakevulture")
    out = keep

if "du" in which:
    o = f"{out}/doomsayers"
    out, keep = o, out
    import os
    os.makedirs(out, exist_ok = True)
    board("du", "du-board.webp")
    title("du", (800, 158, 1880, 303), "du-title.webp", keep = True)
    disc("t-ds-1.png", 884, 696, 120, 90, "du-summoningcircle.webp")
    disc("t-ds-1.png", 942, 992, 106, 100, "du-church.webp", square = True)
    disc("t-ds-1.png", 1368, 1273, 104, 86, "du-glyph.webp", square = True)
    crop(f"{src}/b-du-1.png", (1650, 900, 2450, 1870), 160, 194, "du-char.webp")
    warrior(f"{hire}/turnling-doomsayer.webp", "du-unioncat")
    # the demand dice faces: black, orange and yellow columns on the token sheet
    for color, x0, y, faces in (("black", 455, 1595, ((0, "crusade"), (3, "consecrate"), (5, "craft"))),
                                ("orange", 1785, 1595, ((0, "strike"), (1, "crusade"), (3, "consecrate"), (5, "craft"))),
                                ("yellow", 1785, 2019, ((0, "strike"), (2, "crusade"), (3, "consecrate"), (4, "craft")))):
        for i, face in faces:
            disc("t-ds-1.png", x0 + 212 * i, y, 95, 64, f"du-die-{color}-{face}.webp", square = True)
    out = keep

if "wg" in which:
    o = f"{out}/whisperers"
    out, keep = o, out
    import os
    os.makedirs(out, exist_ok = True)
    board("wg", "wg-board.webp")
    title("wg", (840, 155, 1850, 297), "wg-title.webp", keep = True)
    x = lambda i: 260 + 408 * i
    y = (400, 780, 1156, 1532)
    for name, i, j in (("craft-mouse", 0, 0), ("craft-fox", 1, 0), ("craft-rabbit", 3, 0), ("carnage", 6, 0), ("build", 0, 2), ("martyr", 5, 2)):
        hexagon("t-wg-1.png", x(i), y[j], 160, 96, f"wg-insight-{name}.webp")
    hexagon("t-wg-1.png", x(0), y[3], 160, 96, "wg-insight.webp")
    # the face-up Build marker is the Guild's building
    hexagon("t-wg-1.png", x(0), y[2], 160, 100, "wg-wglair.webp")
    disc("t-wg-1.png", 1812, 470, 100, 86, "wg-glyph.webp", square = True)
    crop(f"{src}/bb-wg-1.png", (1860, 1440, 2200, 1980), 160, 194, "wg-char.webp")
    warrior(f"{hire}/turnling-whisperer.webp", "wg-guildwhisperer", (0x5a, 0x4f, 0xa6))
    out = keep
