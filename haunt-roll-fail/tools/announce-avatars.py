#!/usr/bin/env python3
# The avatars of the General Announcements webhook (2026-11-26; good-game/Announce.scala): the webhook's own picture, a scroll, and one
# for each of the four names its posts take in turn, from Samson's riddle (Judges 14:14, 18): original drawings in the round
# gold-ringed badge style of the other webhooks (webhook-avatars.py).
#   general-announcements.png   the webhook's own: a parchment scroll with a red seal
#   eater-to-eat.png            "Eater to Eat": a lion's head
#   sweeter-than-honey.png      "Sweeter than Honey": a honeycomb dripping honey
#   strength-became-sweet.png   "Strength became Sweet": a honeycomb and bees inside a lion's head
#   stronger-than-a-lion-sun.png "Stronger than a Lion": the sun in its might (Judges 5:31)
#   vedrfolnir.png              Northgard Announcements' Vedrfolnir: the Clan of the Eagle's emblem (only with the header's path:
#                               python3 haunt-roll-fail/tools/announce-avatars.py <header.jpg>)
# Run from the repository root: python3 haunt-roll-fail/tools/announce-avatars.py
import math
import os
import sys
from PIL import Image, ImageDraw

S = 1024          # drawn at 4x, saved at 256
OUT = "haunt-roll-fail/webp2/lfg/webhooks"
GOLD = (214, 172, 74)
GOLD_DARK = (140, 104, 34)
HONEY = (238, 170, 34)
HONEY_DARK = (176, 104, 16)
MANE = (150, 82, 28)
MANE_DARK = (104, 52, 18)
FUR = (222, 164, 82)
FUR_DARK = (170, 112, 48)
INK = (40, 26, 14)


def badge(bg, bg_dark):
    im = Image.new("RGBA", (S, S), (0, 0, 0, 0))
    d = ImageDraw.Draw(im)
    d.ellipse((8, 8, S - 8, S - 8), fill=GOLD_DARK)
    d.ellipse((28, 28, S - 28, S - 28), fill=GOLD)
    d.ellipse((64, 64, S - 64, S - 64), fill=bg_dark)
    d.ellipse((84, 84, S - 84, S - 84), fill=bg)
    return im, d


def save(im, name):
    os.makedirs(OUT, exist_ok=True)
    im.resize((256, 256), Image.LANCZOS).save(os.path.join(OUT, name), optimize=True)


def hexagon(cx, cy, r):
    return [(cx + r * math.cos(math.radians(60 * k)), cy + r * math.sin(math.radians(60 * k))) for k in range(6)]


def comb(d, cells, r, fill, line, width):
    for cx, cy in cells:
        d.polygon(hexagon(cx, cy, r), fill=fill, outline=line)
        d.line(hexagon(cx, cy, r) + [hexagon(cx, cy, r)[0]], fill=line, width=width)


def cells(cx, cy, r, ring):
    # hexagons (flat top) around a centre, out to the given ring
    out = []
    w, h = 1.5 * r, math.sqrt(3) * r
    for q in range(-ring, ring + 1):
        for s in range(-ring, ring + 1):
            if abs(q + s) <= ring:
                out.append((cx + w * q, cy + h * (s + q / 2)))
    return out


def bee(d, x, y, k=1.0, angle=0):
    a = math.radians(angle)
    def p(dx, dy):
        return (x + (dx * math.cos(a) - dy * math.sin(a)) * k, y + (dx * math.sin(a) + dy * math.cos(a)) * k)
    # wings
    for side in (-1, 1):
        cx, cy = p(-4, side * 26)
        d.ellipse((cx - 26 * k, cy - 18 * k, cx + 26 * k, cy + 18 * k), fill=(236, 244, 250), outline=(120, 140, 160), width=max(1, int(4 * k)))
    # body, striped
    bx, by = p(0, 0)
    d.ellipse((bx - 38 * k, by - 24 * k, bx + 38 * k, by + 24 * k), fill=(250, 200, 40), outline=INK, width=max(1, int(5 * k)))
    for dx in (-14, 8):
        x0, y0 = p(dx, -22)
        x1, y1 = p(dx, 22)
        d.line((x0, y0, x1, y1), fill=INK, width=int(11 * k))
    hx, hy = p(40, 0)
    d.ellipse((hx - 16 * k, hy - 16 * k, hx + 16 * k, hy + 16 * k), fill=INK)


def lion_head(d, cx, cy, k=1.0, mane=MANE, mane_dark=MANE_DARK, fur=FUR, fur_dark=FUR_DARK):
    # mane: a ring of flame-shaped tufts
    n = 18
    for i in range(n):
        a = math.radians(i * 360 / n)
        a1, a2 = a - math.radians(13), a + math.radians(13)
        r0, r1 = 190 * k, 300 * k
        d.polygon([(cx + r0 * math.cos(a1), cy + r0 * math.sin(a1)), (cx + r1 * math.cos(a), cy + r1 * math.sin(a)),
                   (cx + r0 * math.cos(a2), cy + r0 * math.sin(a2))], fill=mane_dark if i % 2 else mane)
    d.ellipse((cx - 240 * k, cy - 240 * k, cx + 240 * k, cy + 240 * k), fill=mane)
    d.ellipse((cx - 205 * k, cy - 205 * k, cx + 205 * k, cy + 205 * k), fill=mane_dark)
    # ears
    for side in (-1, 1):
        ex, ey = cx + side * 130 * k, cy - 140 * k
        d.ellipse((ex - 48 * k, ey - 48 * k, ex + 48 * k, ey + 48 * k), fill=fur_dark)
        d.ellipse((ex - 26 * k, ey - 26 * k, ex + 26 * k, ey + 26 * k), fill=(120, 70, 40))
    # face
    d.ellipse((cx - 170 * k, cy - 165 * k, cx + 170 * k, cy + 175 * k), fill=fur)
    # brow and eyes
    for side in (-1, 1):
        ex, ey = cx + side * 70 * k, cy - 40 * k
        d.ellipse((ex - 28 * k, ey - 18 * k, ex + 28 * k, ey + 18 * k), fill=(250, 226, 120))
        d.ellipse((ex - 11 * k, ey - 16 * k, ex + 11 * k, ey + 16 * k), fill=INK)
        d.line((ex - side * 44 * k, ey - 40 * k, ex + side * 30 * k, ey - 30 * k), fill=fur_dark, width=int(14 * k))
    # muzzle, nose and mouth
    d.ellipse((cx - 95 * k, cy + 30 * k, cx + 5 * k, cy + 130 * k), fill=(246, 222, 176))
    d.ellipse((cx - 5 * k, cy + 30 * k, cx + 95 * k, cy + 130 * k), fill=(246, 222, 176))
    d.polygon([(cx - 42 * k, cy + 22 * k), (cx + 42 * k, cy + 22 * k), (cx, cy + 66 * k)], fill=(96, 50, 36))
    d.line((cx, cy + 66 * k, cx, cy + 100 * k), fill=INK, width=int(8 * k))
    d.arc((cx - 60 * k, cy + 70 * k, cx, cy + 130 * k), 20, 160, fill=INK, width=int(8 * k))
    d.arc((cx, cy + 70 * k, cx + 60 * k, cy + 130 * k), 20, 160, fill=INK, width=int(8 * k))
    d.ellipse((cx - 50 * k, cy + 128 * k, cx + 50 * k, cy + 168 * k), fill=(246, 222, 176))


def scroll():
    im, d = badge((126, 30, 34), (84, 18, 22))
    paper, paper_dark = (240, 222, 176), (196, 170, 116)
    # the sheet
    d.rectangle((300, 300, 724, 720), fill=paper)
    for y in range(380, 680, 56):
        d.line((350, y, 674, y), fill=(160, 132, 90), width=12)
    # rolls at the top and bottom
    for y in (300, 720):
        d.rounded_rectangle((250, y - 50, 774, y + 50), radius=50, fill=paper_dark)
        d.rounded_rectangle((262, y - 38, 762, y + 38), radius=38, fill=paper)
        for x in (250, 774):
            d.ellipse((x - 36, y - 50, x + 36, y + 50), fill=paper_dark)
            d.ellipse((x - 18, y - 24, x + 18, y + 24), fill=(150, 120, 70))
    # a red wax seal with ribbons
    d.polygon([(560, 700), (600, 850), (630, 800), (660, 840), (640, 700)], fill=(168, 32, 36))
    d.ellipse((540, 620, 680, 760), fill=(176, 30, 34))
    d.ellipse((566, 646, 654, 734), outline=(120, 16, 20), width=10)
    save(im, "general-announcements.png")


def eater():
    im, d = badge((92, 64, 28), (60, 40, 16))
    lion_head(d, 512, 500, 1.15)
    save(im, "eater-to-eat.png")


def honey():
    im, d = badge((110, 66, 14), (74, 42, 8))
    comb(d, cells(512, 470, 82, 2), 82, HONEY, HONEY_DARK, 14)
    # honey running off the bottom cells
    for x, top, length in ((430, 640, 150), (540, 650, 210), (620, 620, 110)):
        d.rounded_rectangle((x - 22, top, x + 22, top + length), radius=22, fill=HONEY)
        d.ellipse((x - 34, top + length - 30, x + 34, top + length + 38), fill=HONEY)
        d.ellipse((x - 14, top + length - 6, x + 2, top + length + 12), fill=(255, 236, 170))
    bee(d, 330, 290, 1.4, -20)
    save(im, "sweeter-than-honey.png")


def strength_sweet():
    im, d = badge((60, 44, 20), (38, 26, 10))
    # the lion's head in dark outline, a honeycomb where its face was
    lion_head(d, 512, 520, 1.1, mane=(112, 70, 26), mane_dark=(80, 46, 16), fur=(132, 90, 40), fur_dark=(100, 64, 26))
    mask = Image.new("L", (S, S), 0)
    ImageDraw.Draw(mask).ellipse((512 - 175, 520 - 170, 512 + 175, 520 + 180), fill=255)
    hive = Image.new("RGBA", (S, S), (0, 0, 0, 0))
    comb(ImageDraw.Draw(hive), cells(512, 520, 58, 3), 58, HONEY, HONEY_DARK, 10)
    im.paste(hive, (0, 0), Image.composite(hive.split()[3], Image.new("L", (S, S), 0), mask))
    d = ImageDraw.Draw(im)
    bee(d, 330, 250, 1.1, 20)
    bee(d, 690, 230, 1.1, -30)
    bee(d, 760, 420, 0.95, 10)
    save(im, "strength-became-sweet.png")


def sun():
    # Judges 5:31, "let them that love him be as the sun when he goeth forth in his might"
    im, d = badge((40, 70, 140), (24, 44, 96))
    cx, cy = 512, 512
    # rays: long straight ones and short flame-shaped ones between them
    for i in range(24):
        a = math.radians(i * 15)
        long_ray = i % 2 == 0
        r0, r1, w = 200, (360 if long_ray else 300), (0.10 if long_ray else 0.13)
        d.polygon([(cx + r0 * math.cos(a - w), cy + r0 * math.sin(a - w)), (cx + r1 * math.cos(a), cy + r1 * math.sin(a)),
                   (cx + r0 * math.cos(a + w), cy + r0 * math.sin(a + w))], fill=(250, 196, 40) if long_ray else (244, 150, 30))
    # the disc, with a warm glow ring
    d.ellipse((cx - 215, cy - 215, cx + 215, cy + 215), fill=(240, 140, 26))
    d.ellipse((cx - 195, cy - 195, cx + 195, cy + 195), fill=(252, 200, 50))
    d.ellipse((cx - 150, cy - 160, cx + 110, cy + 90), fill=(255, 224, 110))
    save(im, "stronger-than-a-lion-sun.png")


def vedrfolnir(header):
    # Northgard Announcements (2026-11-26): Vedrfolnir, shown with the video game's official eagle, the emblem of Hraesvelg, Clan of
    # the Eagle (Shiro Games' DLC, Steam app 2138500), cut from its Steam store header and set in the gold-ringed badge. The header
    # isn't in the repository: give its path, downloaded from
    # https://shared.fastly.steamstatic.com/store_item_assets/steam/apps/2138500/header.jpg (460x215; the emblem's disc is 158-302, 30-174)
    art = Image.open(header).convert("RGB").crop((158, 30, 302, 174)).resize((S - 168, S - 168), Image.LANCZOS)
    im, d = badge((40, 24, 20), (40, 24, 20))
    disc = Image.new("L", art.size, 0)
    ImageDraw.Draw(disc).ellipse((0, 0, art.width - 1, art.height - 1), fill=255)
    im.paste(art, (84, 84), disc)
    save(im, "vedrfolnir.png")


scroll()
eater()
honey()
strength_sweet()
sun()
if len(sys.argv) > 1:
    vedrfolnir(sys.argv[1])
