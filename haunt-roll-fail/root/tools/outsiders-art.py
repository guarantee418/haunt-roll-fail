# Art for the Outsiders Expansion's factions (slugfacekillah), from its print-and-play files:
#   pdftoppm -r 300 -png <X>-tokens.pdf t-<x>; pdftoppm -r 300 -png <X>-tokens-back.pdf tb-<x>
#   pdftoppm -r 220 -png <X>-board-front.pdf b-<x> (and back, bb-<x>)
#   the meeples: "Meeples.svg" (Outsiders meeple files by The Irreverend), its "ORIGINAL" half rendered in Chromium with the
#   viewBox "-2143.55 -1000 700 600" at 5600x4800 with a transparent background, as meeples.png
#   python3 outsiders-art.py <dir with those renders> <webp2/root/images/faction> [bp ...]
# - the board front, at the size of the other fan boards (1232 wide)
# - the title cut from the board's banner, the glyph and character art from the token sheet and the board
# - the warriors are the meeples, with the empty and five-stack versions drawn from them
# - tokens and buildings cut round (or square) from the token sheet
import os
import sys
import numpy as np
from PIL import Image, ImageDraw

src, out = sys.argv[1], sys.argv[2]

def save(im, path):
    im.save(path, lossless=True) if im.mode == "RGBA" else im.save(path, quality=88)

def board(x, name):
    im = Image.open(f"{src}/b-{x}.png").convert("RGB")
    save(im.resize((1232, round(im.size[1] * 1232 / im.size[0])), Image.LANCZOS), f"{out}/{name}")

def title(x, box, name):
    im = Image.open(f"{src}/b-{x}.png").convert("RGB").crop(box)
    save(im.resize((920, round(920 * im.size[1] / im.size[0])), Image.LANCZOS), f"{out}/{name}")

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

# the meeples of the rendered SVG, left to right in rows
def meeples():
    im = Image.open(f"{src}/meeples.png").convert("RGBA")
    a = np.asarray(im)[:, :, 3] > 20
    def runs(v):
        r, s = [], None
        for i, x in enumerate(v):
            if x and s is None:
                s = i
            if not x and s is not None:
                r.append((s, i))
                s = None
        if s is not None:
            r.append((s, len(v)))
        return r
    result = []
    for y0, y1 in runs(a.any(axis = 1)):
        if y1 - y0 < 100:
            continue
        for x0, x1 in runs(a[y0:y1].any(axis = 0)):
            if x1 - x0 < 50:
                continue
            yy = np.where(a[y0:y1, x0:x1].any(axis = 1))[0]
            result.append(im.crop((x0, y0 + yy[0], x1, y0 + yy[-1] + 1)))
    return result

def warrior(im, height, prefix, stack = True):
    im = im.resize((round(im.size[0] * height / im.size[1]), height), Image.LANCZOS)
    im.save(f"{out}/{prefix}.webp", lossless=True)
    a = np.asarray(im).astype(int)
    e = a.copy()
    e[:, :, :3] = 34
    empty = Image.fromarray(e.astype(np.uint8), "RGBA")
    empty.save(f"{out}/{prefix}-empty.webp", lossless=True)
    if stack:
        for p, suffix in ((im, "-x5"), (empty, "-x5-empty")):
            w, h = p.size
            s = Image.new("RGBA", (w + 4 * (w // 4), h), (0, 0, 0, 0))
            for i in reversed(range(5)):
                s.paste(p, (i * (w // 4), 0), p)
            s.save(f"{out}/{prefix}{suffix}.webp", lossless=True)

which = sys.argv[3:] or ["bp"]

m = meeples()

if "bp" in which:
    o = f"{out}/bone"
    out, keep = o, out
    os.makedirs(out, exist_ok = True)
    board("bp", "bp-board.webp")
    title("bp", (880, 160, 1660, 290), "bp-title.webp")
    # the bones: mouse, rabbit, fox from the left of the top row, and the buried side (the back sheet's X)
    for suit, x in (("mouse", 888), ("rabbit", 1194), ("fox", 1500)):
        disc("t-bp.png", x, 839, 117, 90, f"bp-bone-{suit}.webp")
    disc("tb-bp.png", 869, 839, 117, 90, "bp-bone-buried.webp")
    disc("t-bp.png", 1371, 1137, 109, 100, "bp-stash.webp", square = True)
    disc("t-bp.png", 1653, 1137, 109, 86, "bp-glyph.webp", square = True)
    crop(f"{src}/b-bp.png", (1900, 1120, 2500, 1900), 160, 194, "bp-char.webp")
    warrior(m[17], 96, "bp-pug")
    warrior(m[14], 124, "bp-alpha", stack = False)
    out = keep

    # the action icon: the pug's head
    o = f"{out}/bone"
    p = m[17]
    w = p.size[0]
    head = p.crop((0, 0, w, w)).resize((100, 100), Image.LANCZOS)
    head.save(f"{o}/bp-action.webp", lossless=True)

if "wm" in which:
    # the Winged Menace: board b-wm.png (220 dpi), token sheet t-wm.png (300 dpi)
    o = f"{out}/menace"
    out, keep = o, out
    os.makedirs(out, exist_ok = True)
    board("wm", "wm-board.webp")
    title("wm", (790, 166, 1706, 258), "wm-title.webp")
    disc("t-wm.png", 1082, 1134, 104, 100, "wm-infestation.webp", square = True)
    disc("t-wm.png", 1364, 1134, 104, 100, "wm-cave.webp", square = True)
    disc("t-wm.png", 788, 1122, 116, 80, "wm-guano.webp")
    disc("t-wm.png", 1650, 1416, 104, 86, "wm-glyph.webp", square = True)
    crop(f"{src}/b-wm.png", (2080, 1500, 2480, 1930), 160, 194, "wm-char.webp")
    # the bat meeple is nearly black: a light rim keeps it visible on the dark parts of the maps
    from PIL import ImageFilter
    p = m[10]
    pad = Image.new("RGBA", (p.size[0] + 40, p.size[1] + 40), (0, 0, 0, 0))
    pad.paste(p, (20, 20), p)
    rim = pad.getchannel("A").point(lambda v: 255 if v > 20 else 0).filter(ImageFilter.MaxFilter(19))
    lit = Image.new("RGBA", pad.size, (196, 186, 220, 0))
    lit.putalpha(rim)
    lit.alpha_composite(pad)
    warrior(lit, 90, "wm-bat")
    w = p.size[0]
    p.crop((0, 0, w, w)).resize((100, 100), Image.LANCZOS).save(f"{out}/wm-action.webp", lossless=True)
    out = keep

if "of" in which:
    # the Order of the Forest: board b-of.png (220 dpi), token sheet t-of.png (300 dpi)
    o = f"{out}/order"
    out, keep = o, out
    os.makedirs(out, exist_ok = True)
    board("of", "of-board.webp")
    title("of", (826, 176, 1668, 290), "of-title.webp")
    disc("t-of.png", 1344, 840, 116, 80, "of-soup.webp")
    disc("t-of.png", 1344, 1710, 116, 80, "of-coins.webp")
    for suit, x, y in (("fox", 1366, 1134), ("bird", 1934, 1134), ("mouse", 1366, 1416), ("rabbit", 1934, 1416)):
        disc("t-of.png", x, y, 104, 100, f"of-abbey-{suit}.webp", square = True)
    disc("t-of.png", 1650, 1134, 104, 86, "of-glyph.webp", square = True)
    crop(f"{src}/t-of.png", (1560, 1310, 1740, 1522), 160, 194, "of-char.webp")
    # the meeples are dark green: a light rim keeps them visible on the forests of the maps
    from PIL import ImageFilter
    def rimmed(p, colour):
        pad = Image.new("RGBA", (p.size[0] + 40, p.size[1] + 40), (0, 0, 0, 0))
        pad.paste(p, (20, 20), p)
        rim = pad.getchannel("A").point(lambda v: 255 if v > 20 else 0).filter(ImageFilter.MaxFilter(15))
        lit = Image.new("RGBA", pad.size, colour + (0,))
        lit.putalpha(rim)
        lit.alpha_composite(pad)
        return lit
    warrior(rimmed(m[2], (236, 228, 196)), 90, "of-devotee")
    for suit, i in (("bird", 0), ("mouse", 4), ("rabbit", 8), ("fox", 12)):
        warrior(rimmed(m[i], (236, 228, 196)), 100, f"of-priest-{suit}", stack = False)
    p = m[2]
    w = p.size[0]
    p.crop((0, 0, w, w)).resize((100, 100), Image.LANCZOS).save(f"{out}/of-action.webp", lossless=True)
    out = keep
