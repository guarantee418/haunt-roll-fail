#!/usr/bin/env python3
# The hecklers of the "Roast Me" role (2026-11-25, good-game/Heckler.scala). The Root heckler, the Court Crow, is the Corvid
# Conspiracy's character art (faction/mischief/cc-char.webp, laughing with its beak open) in a jester's cap drawn here.
#   webp2/lfg/webhooks/heckler-root.png          the Court Crow's avatar: the crow in the can on a gold-ringed badge (256)
#   webp2/lfg/server-emoji/root_crowjester.png   the same crow for a Discord server emoji (128, transparent), to upload by hand
#   webp2/lfg/server-emoji/root_jestercan.png    the capped crow peeking out of a trash can, lid up (128, transparent; asked for on
#                                                2026-11-25), to upload by hand
#   webp2/lfg/webhooks/heckler-nort.png          Ratatoskr's avatar: the Squirrel Clan's emblem (nort/images/clan/squirrel.webp) as a PNG
# Run from the repository root: python3 haunt-roll-fail/tools/heckler-art.py
import math
import os
from PIL import Image, ImageDraw, ImageFilter

BASE = "haunt-roll-fail/webp2"
INK = (28, 20, 30, 255)
PURPLE = (134, 66, 176, 255)
PURPLE_DARK = (96, 44, 130, 255)
GREEN = (126, 184, 98, 255)
GREEN_DARK = (86, 136, 66, 255)
GOLD = (236, 190, 62, 255)
GOLD_DARK = (170, 122, 30, 255)

PAD = 420          # room above and around the crow for the cap's horns
OX, OY = PAD, PAD  # where the art's (0, 0) lands


LIFT = 130         # the horns and bells are drawn this much higher than their numbers say, clear of the eye


def P(x, y):
    return (x + OX, y + OY)


def H(pts):
    return [(x, y - LIFT) for x, y in pts]


def hbezier(p0, p1, p2, p3, n=60):
    return bezier(*H([p0, p1, p2, p3]), n)


def bezier(p0, p1, p2, p3, n=60):
    out = []
    for i in range(n + 1):
        t = i / n
        a, b, c, e = (1 - t) ** 3, 3 * (1 - t) ** 2 * t, 3 * (1 - t) * t ** 2, t ** 3
        out.append(P(a * p0[0] + b * p1[0] + c * p2[0] + e * p3[0], a * p0[1] + b * p1[1] + c * p2[1] + e * p3[1]))
    return out


def shape(d, pts, fill, w=16):
    d.polygon(pts, fill=fill)
    d.line(pts + [pts[0]], fill=INK, width=w, joint="curve")


def bell(d, x, y, r):
    x, y = P(x, y)
    d.ellipse((x - r - 9, y - r - 9, x + r + 9, y + r + 9), fill=INK)
    d.ellipse((x - r, y - r, x + r, y + r), fill=GOLD)
    d.ellipse((x - r * 0.6, y - r * 0.7, x - r * 0.05, y - r * 0.15), fill=(255, 240, 170, 255))
    d.line((x - r * 0.6, y + r * 0.35, x + r * 0.6, y + r * 0.35), fill=GOLD_DARK, width=9)
    d.ellipse((x - 7, y + r * 0.35 - 7, x + 7, y + r * 0.35 + 7), fill=INK)


def cap(d):
    # in the art's coordinates: the crown of the head is near (820, 60), the back of the head near (1150, 330), the beak points left
    # three horns from the band: the front one flops forward over the beak, the middle one stands up, the back one hangs behind
    back = hbezier((930, 175), (1170, -60), (1450, 40), (1470, 260)) + hbezier((1430, 275), (1360, 120), (1210, 120), (1150, 300))[1:]
    front = hbezier((620, 220), (470, 10), (270, -20), (200, 150)) + hbezier((235, 170), (320, 80), (460, 110), (720, 180))[1:]
    middle = hbezier((660, 185), (630, -90), (880, -230), (1010, -150)) + hbezier((1005, -110), (900, -150), (840, 20), (960, 185))[1:]
    shape(d, back, GREEN)
    shape(d, front, GREEN)
    shape(d, middle, PURPLE)
    d.line(hbezier((1040, 200), (1210, 40), (1380, 100), (1440, 240), 30), fill=GREEN_DARK, width=18)
    d.line(hbezier((640, 205), (500, 60), (330, 40), (240, 150), 30), fill=GREEN_DARK, width=18)
    d.line(hbezier((790, 180), (760, -20), (880, -150), (990, -130), 30), fill=PURPLE_DARK, width=18)
    # the band round the head, purple with gold diamonds
    top = bezier((575, 20), (760, -45), (1010, 30), (1185, 185), 40)
    bottom = bezier((1150, 265), (990, 115), (770, 45), (600, 100), 40)
    shape(d, top + bottom, PURPLE_DARK)
    for i in range(1, 6):
        t = i / 6
        (x0, y0), (x1, y1) = top[int(t * 40)], bottom[40 - int(t * 40)]
        x, y, r = (x0 + x1) / 2, (y0 + y1) / 2, 24
        d.polygon([(x, y - r), (x + r * 0.75, y), (x, y + r), (x - r * 0.75, y)], fill=GOLD, outline=INK, width=6)
    bell(d, 205, 160 - LIFT, 56)
    bell(d, 1010, -140 - LIFT, 56)
    bell(d, 1455, 275 - LIFT, 56)


def collar(d):
    # a jester's pointed collar round the neck, which is where the emoji cuts the art
    n, x0, x1 = 6, 360, 1300
    xs = [x0 + (x1 - x0) * i / n for i in range(n + 1)]
    def top(x):
        t = (x - x0) / (x1 - x0)
        return 470 + 70 * t - 40 * math.sin(math.pi * t)
    d.polygon([P(x0, top(x0) - 40)] + [P(x, top(x) - 40) for x in xs] + [P(x1, top(x1) + 30), P(x0, top(x0) + 30)], fill=PURPLE_DARK)
    for i in range(n):
        a, b = xs[i], xs[i + 1]
        m = (a + b) / 2
        tip = (m, top(m) + 190)
        pts = [P(a, top(a)), P(b, top(b)), P(*tip)]
        shape(d, pts, PURPLE if i % 2 == 0 else GREEN, 14)
    d.line([P(x, top(x)) for x in [x0 + (x1 - x0) * i / 40 for i in range(41)]], fill=INK, width=16)
    for i in range(n):
        m = (xs[i] + xs[i + 1]) / 2
        bell(d, m, top(m) + 205, 34)


def crow(with_collar=True):
    src = Image.open(os.path.join(BASE, "root/images/faction/mischief/cc-char.webp")).convert("RGBA")
    im = Image.new("RGBA", (src.width + 2 * PAD, src.height + 2 * PAD), (0, 0, 0, 0))
    im.alpha_composite(src, (OX, OY))
    # the body under the collar (the bomb, the sword) is left out: the emoji is the bust
    cut = Image.new("L", im.size, 255)
    dc = ImageDraw.Draw(cut)
    dc.polygon([P(-200, 560), P(1700, 640), P(1700, 2000), P(-200, 2000)], fill=0)
    dc.rectangle(P(-200, 420) + P(350, 2000), fill=0)      # the bomb's fuse
    dc.rectangle(P(1280, 420) + P(2000, 2000), fill=0)     # the sword's hilt
    if not with_collar:
        dc.polygon([P(-200, 470), P(1700, 470), P(1700, 2000), P(-200, 2000)], fill=0)   # the tunic, down in the can
    im.putalpha(Image.composite(im.getchannel("A"), Image.new("L", im.size, 0), cut))
    d = ImageDraw.Draw(im)
    cap(d)
    if with_collar:
        collar(d)
    return im


def square(im, size, margin):
    # the head, cap and collar, cut under the collar (an emoji reads at 22 px: the laughing beak and the cap are what matter)
    x0, y0, x1, y1 = P(-40, -440) + P(1560, 760)
    im = im.crop((x0, y0, x1, y1))
    im = im.crop(im.getbbox())
    s = max(im.size)
    sq = Image.new("RGBA", (s, s), (0, 0, 0, 0))
    sq.alpha_composite(im, ((s - im.width) // 2, (s - im.height) // 2))
    inner = size - 2 * margin
    out = Image.new("RGBA", (size, size), (0, 0, 0, 0))
    out.alpha_composite(sq.resize((inner, inner), Image.LANCZOS), (margin, margin))
    return out


def badge(content, size=1024):
    im = Image.new("RGBA", (size, size), (0, 0, 0, 0))
    d = ImageDraw.Draw(im)
    d.ellipse((8, 8, size - 8, size - 8), fill=(140, 104, 34, 255))
    d.ellipse((28, 28, size - 28, size - 28), fill=(214, 172, 74, 255))
    d.ellipse((64, 64, size - 64, size - 64), fill=(52, 30, 70, 255))
    d.ellipse((84, 84, size - 84, size - 84), fill=(78, 46, 104, 255))
    c = content.resize((size - 220, size - 220), Image.LANCZOS)
    mask = Image.new("L", (size, size), 0)
    ImageDraw.Draw(mask).ellipse((84, 84, size - 84, size - 84), fill=255)
    layer = Image.new("RGBA", (size, size), (0, 0, 0, 0))
    layer.alpha_composite(c, (110, 110))
    clipped = Image.new("RGBA", (size, size), (0, 0, 0, 0))
    clipped.paste(layer, (0, 0), Image.composite(layer.getchannel("A"), Image.new("L", (size, size), 0), mask))
    im.alpha_composite(clipped)
    return im


STEEL = (150, 158, 166, 255)
STEEL_LIGHT = (196, 204, 210, 255)
STEEL_DARK = (98, 106, 116, 255)


def can(c, size=1024):
    """The capped crow's head peeking out of a galvanized trash can, its lid propped up behind."""
    im = Image.new("RGBA", (size, size), (0, 0, 0, 0))
    d = ImageDraw.Draw(im)
    cx, rim_y, rx, ry = 560, 600, 330, 70     # the can's opening
    bottom_y, brx = 1000, 280
    # the lid, tipped up behind the head on the right
    lid = Image.new("RGBA", (size, size), (0, 0, 0, 0))
    ld = ImageDraw.Draw(lid)
    lx, ly = 760, 380
    ld.ellipse((lx - 300, ly - 80, lx + 300, ly + 80), fill=INK)
    ld.ellipse((lx - 284, ly - 66, lx + 284, ly + 66), fill=STEEL)
    ld.ellipse((lx - 200, ly - 44, lx + 200, ly + 44), outline=STEEL_DARK, width=10)
    ld.ellipse((lx - 110, ly - 24, lx + 110, ly + 24), outline=STEEL_LIGHT, width=8)
    lid = lid.rotate(-58, center=(lx, ly), resample=Image.BICUBIC)
    im.alpha_composite(lid)
    # the opening, dark inside
    d.ellipse((cx - rx, rim_y - ry, cx + rx, rim_y + ry), fill=INK)
    d.ellipse((cx - rx + 22, rim_y - ry + 16, cx + rx - 22, rim_y + ry - 12), fill=(20, 18, 24, 255))
    # the crow's head and cap, its neck down in the can
    x0, y0, x1, y1 = P(-40, -440) + P(1560, 700)
    head = c.crop((x0, y0, x1, y1))
    k = 0.56
    head = head.resize((int(head.width * k), int(head.height * k)), Image.LANCZOS)
    im.alpha_composite(head, (int(cx - (40 + 800) * k), int(rim_y + 75 - (440 + 560) * k)))
    # the can's body over the neck: from the opening's middle line down, ribbed
    body = [(cx - rx, rim_y), (cx + rx, rim_y), (cx + brx, bottom_y), (cx - brx, bottom_y)]
    d.polygon(body, fill=STEEL)
    for i in range(1, 8):
        t = i / 8
        xt = cx - rx + 2 * rx * t
        xb = cx - brx + 2 * brx * t
        d.line((xt, rim_y + 40, xb, bottom_y - 30), fill=STEEL_DARK if i % 2 else STEEL_LIGHT, width=14)
    d.line((cx - rx + 30, rim_y + 30, cx - brx + 20, bottom_y - 20), fill=STEEL_LIGHT, width=22)   # the shine
    d.polygon(body, outline=INK, width=16)
    # the front of the rim, over the neck
    d.arc((cx - rx, rim_y - ry, cx + rx, rim_y + ry), 0, 180, fill=INK, width=38)
    d.arc((cx - rx + 8, rim_y - ry + 8, cx + rx - 8, rim_y + ry - 8), 10, 170, fill=STEEL_LIGHT, width=12)
    # a band near the bottom and the handles
    d.arc((cx - brx - 10, bottom_y - 150, cx + brx + 10, bottom_y - 70), 0, 180, fill=INK, width=14)
    for side in (-1, 1):
        hx = cx + side * (rx - 6)
        d.arc((hx - 60, rim_y + 60, hx + 60, rim_y + 170), 90 if side > 0 else 270, 270 if side > 0 else 90, fill=INK, width=18)
    d.ellipse((cx - brx, bottom_y - 22, cx + brx, bottom_y + 22), fill=INK)
    return im


def edged(im, grow=13):
    # a light sticker edge, so the dark crow shows on Discord's dark theme
    edge = im.getchannel("A").filter(ImageFilter.MaxFilter(grow))
    out = Image.new("RGBA", im.size, (240, 232, 222, 0))
    out.putalpha(edge)
    out.alpha_composite(im)
    return out


def fit(im, size, margin):
    im = im.crop(im.getbbox())
    s = max(im.size)
    sq = Image.new("RGBA", (s, s), (0, 0, 0, 0))
    sq.alpha_composite(im, ((s - im.width) // 2, (s - im.height) // 2))
    out = Image.new("RGBA", (size, size), (0, 0, 0, 0))
    out.alpha_composite(sq.resize((size - 2 * margin, size - 2 * margin), Image.LANCZOS), (margin, margin))
    return out


def main():
    c = crow()
    emoji = square(c, 512, 12)
    # a light sticker edge, so the dark crow shows on Discord's dark theme
    edge = emoji.getchannel("A").filter(ImageFilter.MaxFilter(13))
    sticker = Image.new("RGBA", emoji.size, (240, 232, 222, 0))
    sticker.putalpha(edge)
    sticker.alpha_composite(emoji)
    emoji = sticker
    os.makedirs(os.path.join(BASE, "lfg/server-emoji"), exist_ok=True)
    emoji.resize((128, 128), Image.LANCZOS).save(os.path.join(BASE, "lfg/server-emoji/root_crowjester.png"), optimize=True)
    os.makedirs(os.path.join(BASE, "lfg/webhooks"), exist_ok=True)
    sq = Image.open(os.path.join(BASE, "nort/images/clan/squirrel.webp")).convert("RGBA")
    sq.resize((256, 256), Image.LANCZOS).save(os.path.join(BASE, "lfg/webhooks/heckler-nort.png"), optimize=True)
    bin_ = edged(fit(can(crow(with_collar=False)), 512, 12))
    bin_.resize((128, 128), Image.LANCZOS).save(os.path.join(BASE, "lfg/server-emoji/root_jestercan.png"), optimize=True)
    # the crow in the can is the Court Crow's picture too (2026-11-25, liked best)
    badge(fit(bin_, 512, 0)).resize((256, 256), Image.LANCZOS).save(os.path.join(BASE, "lfg/webhooks/heckler-root.png"), optimize=True)


if __name__ == "__main__":
    main()
