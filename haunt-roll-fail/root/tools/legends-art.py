#!/usr/bin/env python3
# The Doomed Expansion's Legends Scenarios (fan-made by slugfacekillah; Law of Doom D5): the pieces for root/legends.scala,
# cut from the print-and-play sticker sheet "Doomed-Scenarios-Stickers.pdf" and the monster cards (63x88mm PNGs).
#
#   legends-art.py <Doomed-Scenarios-Stickers.pdf> <Doomed-Scenarios-CARDS(63x88mm) folder>
#
# Writes into webp2/root/images/: item/item-ring*.webp and the monsters' weapon items (mauler, spiritsword, chainsaw, slaymore,
# bushblazer) (the five variants every item has,
# on the backgrounds of the existing items), legends/space-suited.webp and space-unsuited.webp (the scenario space, both
# sides), legends/monster-<name>.webp (the pawns; the Demon Bear has no pawn sticker, so its marker's head is used),
# legends/marker-<name>.webp and marker-back.webp (the hex monster markers), legends/die-<n>.webp (the Legends die, 1 the
# ghost) and legends/monster-card-<name>.webp, -weapon.webp (the monster cards).
import os, subprocess, sys, tempfile
from collections import deque

import numpy as np
from PIL import Image, ImageChops, ImageFilter

ROOT = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..", "webp2", "root", "images")
ITEM = os.path.join(ROOT, "item")
OUT = os.path.join(ROOT, "legends")

VARIANTS = ["", "-exhausted", "-damaged", "-damaged-exhausted", "-empty"]


def sheet(pdf):
    d = tempfile.mkdtemp()
    subprocess.check_call(["pdftoppm", "-r", "300", "-png", pdf, os.path.join(d, "s")])
    return Image.open(os.path.join(d, "s-1.png")).convert("RGB")


# a round background: each pixel that differs from its circle's median (the "?" on item-any) set to it
def round_clean(a):
    h, w = a.shape[:2]
    yy, xx = np.mgrid[0:h, 0:w]
    r = np.round(np.hypot(xx - (w - 1) / 2, yy - (h - 1) / 2)).astype(int)
    out = a.copy()
    # only inside the circle the rounded square holds whole
    for k in range(72):
        on = r == k
        med = np.median(a[(r >= max(k, 14)) & (r <= max(k, 14) + 1)], 0)
        off = on & ((np.abs(a - med).sum(-1) > 12) | (k < 14))
        out[off] = med
    return out


def backgrounds():
    # the plain and damaged ones are item-any's without its "?"; the exhausted ones each pixel the value most of the existing items
    # agree on, and where too few agree (their drawings' middle) the nearest agreed pixel on the same stripe
    base = ["bag", "boots", "coins", "crossbow", "hammer", "sword", "teapot", "torch", "club", "bookofnames", "rabbitspaw", "cursedidol"]
    bgs = {}
    for v in VARIANTS:
        if v in ("", "-damaged"):
            a = np.array(Image.open(os.path.join(ITEM, "item-any" + v + ".webp")).convert("RGBA")).astype(float)
            bgs[v] = Image.fromarray(round_clean(a).astype("uint8"), "RGBA")
            continue
        st = np.stack([np.array(Image.open(os.path.join(ITEM, "item-" + b + v + ".webp")).convert("RGBA")).astype(float) for b in base])
        d = np.abs(st[:, None] - st[None, :]).sum(-1)
        votes = (d < 24).sum(1)
        pick = votes.argmax(0)
        bg = np.take_along_axis(st, pick[None, :, :, None].repeat(4, -1), 0)[0]
        if v == "-empty":
            bgs[v] = Image.fromarray(round_clean(bg).astype("uint8"), "RGBA")
            continue
        sure = votes.max(0) >= 8
        h, w = sure.shape
        out = bg.copy()
        for y, x in zip(*np.nonzero(~sure)):
            for k in range(1, w):
                hit = [(y - k, x - k), (y + k, x + k)]
                hit = [(yy, xx) for yy, xx in hit if 0 <= yy < h and 0 <= xx < w and sure[yy, xx]]
                if hit:
                    out[y, x] = bg[hit[0]]
                    break
        bgs[v] = Image.fromarray(out.astype("uint8"), "RGBA")
    return bgs


def largest(b, fill=True):
    h, w = b.shape
    lab = np.zeros(b.shape, bool)
    best = []
    for y0 in range(h):
        for x0 in range(w):
            if b[y0, x0] and not lab[y0, x0]:
                q = deque([(y0, x0)])
                lab[y0, x0] = True
                pts = []
                while q:
                    y, x = q.popleft()
                    pts.append((y, x))
                    for yy, xx in ((y + 1, x), (y - 1, x), (y, x + 1), (y, x - 1)):
                        if 0 <= yy < h and 0 <= xx < w and b[yy, xx] and not lab[yy, xx]:
                            lab[yy, xx] = True
                            q.append((yy, xx))
                if len(pts) > len(best):
                    best = pts
    keep = np.zeros(b.shape, bool)
    for y, x in best:
        keep[y, x] = True
    if not fill:
        return keep
    # fill the holes: everything not reached from the border
    out = np.zeros(b.shape, bool)
    q = deque([(0, 0)])
    out[0, 0] = True
    while q:
        y, x = q.popleft()
        for yy, xx in ((y + 1, x), (y - 1, x), (y, x + 1), (y, x - 1)):
            if 0 <= yy < h and 0 <= xx < w and not keep[yy, xx] and not out[yy, xx]:
                out[yy, xx] = True
                q.append((yy, xx))
    return ~out


# the icon of a sticker on its light gray background, its largest colored or dark shape kept
def cutout(im, size, fill=True, satmin=0.18, dark=0.55):
    im = im.resize((size, size), Image.LANCZOS)
    a = np.array(im).astype(float) / 255
    mx = a.max(2)
    mn = a.min(2)
    sat = (mx - mn) / (mx + 1e-6)
    m = ((sat > satmin) | (mx < dark)).astype("uint8") * 255
    M = Image.fromarray(m, "L").filter(ImageFilter.MaxFilter(3)).filter(ImageFilter.MinFilter(3))
    keep = largest(np.array(M) > 128, fill)
    M = Image.fromarray((keep * 255).astype("uint8"), "L").filter(ImageFilter.MinFilter(3)).filter(ImageFilter.GaussianBlur(0.8))
    icon = im.convert("RGBA")
    icon.putalpha(M)
    return icon, M


# a thin, pale icon (the swords): what differs from the sticker's background, as a wide median filter leaves it
def thin(im, size, med=31, th=0.04):
    im = im.resize((size, size), Image.LANCZOS)
    bg = im.filter(ImageFilter.MedianFilter(med))
    d = np.abs(np.array(im).astype(float) - np.array(bg).astype(float)).max(2) / 255
    m = Image.fromarray(((d > th) * 255).astype("uint8"), "L").filter(ImageFilter.MaxFilter(3)).filter(ImageFilter.MinFilter(3))
    keep = largest(np.array(m) > 128, True)
    M = Image.fromarray((keep * 255).astype("uint8"), "L").filter(ImageFilter.GaussianBlur(0.6))
    icon = im.convert("RGBA")
    icon.putalpha(M)
    return icon, M


def item(name, icon, M, bgs):
    for v in VARIANTS:
        t = bgs[v].copy()
        if v == "-empty":
            mm = M.point(lambda p: 255 if p > 128 else 0)
            outer = mm.filter(ImageFilter.MaxFilter(5))
            edge = ImageChops.subtract(outer, mm.filter(ImageFilter.MinFilter(3)))
            halo = outer.filter(ImageFilter.MaxFilter(5))
            white = Image.new("RGBA", (150, 150), (255, 255, 255, 0))
            white.putalpha(ImageChops.subtract(halo, outer).point(lambda p: p * 0.6))
            black = Image.new("RGBA", (150, 150), (20, 20, 20, 0))
            black.putalpha(edge)
            t.alpha_composite(white, (10, 10))
            t.alpha_composite(black, (10, 10))
        else:
            t.alpha_composite(icon, (10, 10))
        t.save(os.path.join(ITEM, "item-" + name + v + ".webp"), "WEBP", lossless=True)


# a round piece: the circle cut out, its edge softened
def disc(im, cx, cy, r, size):
    c = im.crop((cx - r, cy - r, cx + r, cy + r)).resize((size, size), Image.LANCZOS).convert("RGBA")
    m = Image.new("L", (size * 4, size * 4), 0)
    from PIL import ImageDraw
    ImageDraw.Draw(m).ellipse((4, 4, size * 4 - 4, size * 4 - 4), fill=255)
    c.putalpha(m.resize((size, size), Image.LANCZOS))
    return c


def main():
    s = sheet(sys.argv[1])
    bgs = backgrounds()
    os.makedirs(OUT, exist_ok=True)

    # the Ring: the "R" Ring sticker, without its letter
    ring = Image.new("RGB", (165, 165), (232, 232, 232))
    ring.paste(s.crop((110 + 30, 110 + 45, 110 + 195, 110 + 190)), (0, 6))
    icon, M = cutout(ring, 150, fill=False, satmin=0.3, dark=0.4)
    item("ring", icon, M, bgs)

    # the scenario space: the stone ring (no suit) and the dotted circle with the three suits
    disc(s, 1876, 2026, 372, 420).save(os.path.join(OUT, "space-unsuited.webp"), "WEBP", quality=90)
    disc(s, 2772, 2026, 372, 420).save(os.path.join(OUT, "space-suited.webp"), "WEBP", quality=90)

    # the weapons: the "L" stickers, without their letter and stars
    for k, name in enumerate(["chainsaw", "bushblazer", "mauler", "spiritsword"]):
        x = 110 + 284 * k
        w = Image.new("RGB", (165, 165), (232, 232, 232))
        w.paste(s.crop((x + 30, 1230 + 45, x + 195, 1230 + 190)), (0, 6))
        icon, M = (thin if name == "spiritsword" else cutout)(w, 150)
        item(name, icon, M, bgs)
    w = Image.new("RGB", (420, 420), (232, 232, 232))
    w.paste(s.crop((1250, 1255, 1670, 1440)), (0, 118))
    icon, M = thin(w, 150)
    item("slaymore", icon, M, bgs)

    # the Legends die: 1 is the ghost
    for n in range(1, 7):
        x = [120, 334, 548, 760, 974, 1188][n - 1]
        s.crop((x, 1810, x + 186, 1996)).resize((96, 96), Image.LANCZOS).save(os.path.join(OUT, "die-" + str(n) + ".webp"), "WEBP", quality=92)

    # the hex monster markers, as discs
    for name, cx, cy in [("white-wolf", 1800, 636), ("red-knight", 2206, 636), ("shadowmaw", 2614, 636), ("beaverbot", 3020, 636), ("demon-bear", 2614, 1066), ("back", 3020, 1066)]:
        disc(s, cx, cy, 165, 140).save(os.path.join(OUT, "marker-" + name + ".webp"), "WEBP", quality=92)

    # the pawns, cut from the white sheet; the Demon Bear has none, so its marker's head
    for name, box in [("shadowmaw", (70, 2040, 450, 2480)), ("red-knight", (425, 2030, 765, 2480)), ("white-wolf", (765, 2120, 1035, 2480)), ("beaverbot", (1045, 2120, 1295, 2480))]:
        c = s.crop(box)
        a = np.array(c).astype(int)
        keep = largest(np.array(Image.fromarray(((a.min(2) < 225) * 255).astype("uint8"), "L").resize((c.width // 2, c.height // 2))) > 128)
        M = Image.fromarray((keep * 255).astype("uint8"), "L").resize(c.size, Image.LANCZOS).filter(ImageFilter.GaussianBlur(0.8))
        p = c.convert("RGBA")
        p.putalpha(M)
        p = p.crop(p.getbbox())
        p.thumbnail((240, 240), Image.LANCZOS)
        p.save(os.path.join(OUT, "monster-" + name + ".webp"), "WEBP", quality=92)
    bear = s.crop((2614 - 150, 1066 - 150, 2614 + 150, 1066 + 150))
    a = np.array(bear).astype(float) / 255
    mx = a.max(2); mn = a.min(2)
    head = (((mx - mn) / (mx + 1e-6) > 0.45) | (mx < 0.25))
    keep = largest(np.array(Image.fromarray((head * 255).astype("uint8"), "L").resize((150, 150))) > 128)
    M = Image.fromarray((keep * 255).astype("uint8"), "L").resize(bear.size, Image.LANCZOS).filter(ImageFilter.GaussianBlur(1))
    p = bear.convert("RGBA")
    p.putalpha(M)
    p = p.crop(p.getbbox())
    p.thumbnail((240, 240), Image.LANCZOS)
    p.save(os.path.join(OUT, "monster-demon-bear.webp"), "WEBP", quality=92)

    # the monster cards, both sides
    for n, name in [(2, "shadowmaw"), (3, "white-wolf"), (4, "beaverbot"), (5, "red-knight"), (6, "demon-bear")]:
        for side, f in [("", "Monster%d.png"), ("-weapon", "Monster%dItem.png")]:
            c = Image.open(os.path.join(sys.argv[2], f % n)).convert("RGB")
            c = c.resize((500, round(500 * c.height / c.width)), Image.LANCZOS)
            c.save(os.path.join(OUT, "monster-card-" + name + side + ".webp"), "WEBP", quality=88)


if __name__ == "__main__":
    main()
