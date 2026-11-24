#!/usr/bin/env python3
# The avatars of the Discord webhooks the bot makes for the turn pings of Pincer and Company of Heroes (2026-11-24; the make-webhook
# change, good-game/DiscordServer.scala): original drawings in the round gold-ringed badge style of the server's own emojis, nothing
# taken from the published games.
#   drum-and-fife.png   Pincer's "Drum and Fife": an 18th-century side drum with crossed sticks, on regimental blue
#   field-wireless.png  Company of Heroes' "Field Wireless": a 1940s field radio set with its handset and whip antenna, on olive
# Run from the repository root: python3 haunt-roll-fail/tools/webhook-avatars.py
import math
import os
from PIL import Image, ImageDraw

S = 1024          # drawn at 4x, saved at 256
OUT = "webp2/lfg/webhooks"
GOLD = (214, 172, 74)
GOLD_DARK = (140, 104, 34)


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


def stick(d, x0, y0, x1, y1, w, color, tip):
    d.line((x0, y0, x1, y1), fill=color, width=w)
    r = w * 0.9
    d.ellipse((x1 - r, y1 - r, x1 + r, y1 + r), fill=tip)


def drum():
    im, d = badge((32, 52, 104), (20, 34, 70))
    cx = S // 2
    top, bottom = 470, 800
    left, right = 250, 774
    ry = 70
    # shell
    d.rectangle((left, top, right, bottom), fill=(232, 220, 190))
    d.ellipse((left, bottom - ry, right, bottom + ry), fill=(232, 220, 190))
    # cords: a zigzag between the hoops
    n = 7
    pts = []
    for i in range(n * 2 + 1):
        x = left + (right - left) * i / (n * 2)
        y = top + 40 if i % 2 == 0 else bottom - 30
        pts.append((x, y))
    d.line(pts, fill=(120, 84, 40), width=14)
    # hoops, red
    d.rectangle((left, bottom - 40, right, bottom + 10), fill=(168, 32, 36))
    d.ellipse((left, bottom - ry + 10, right, bottom + ry + 10), outline=(120, 20, 24), width=10)
    d.rectangle((left, top - 10, right, top + 40), fill=(168, 32, 36))
    # head
    d.ellipse((left, top - ry, right, top + ry), fill=(245, 240, 226), outline=(168, 32, 36), width=22)
    # crossed sticks above the head
    wood = (110, 70, 34)
    tip = (80, 48, 22)
    stick(d, 300, 220, 640, 440, 30, wood, tip)
    stick(d, 724, 220, 384, 440, 30, wood, tip)
    save(im, "drum-and-fife.png")


def radio():
    im, d = badge((78, 86, 52), (52, 58, 34))
    olive = (96, 104, 60)
    olive_dark = (58, 64, 36)
    # antenna
    d.line((690, 470, 760, 150), fill=(40, 40, 36), width=16)
    d.ellipse((748, 136, 776, 164), fill=(40, 40, 36))
    # case
    d.rounded_rectangle((230, 430, 794, 800), radius=36, fill=olive_dark)
    d.rounded_rectangle((250, 450, 774, 780), radius=28, fill=olive)
    # front panel
    d.rounded_rectangle((290, 490, 734, 740), radius=18, fill=(34, 36, 30))
    # tuning dial with a needle
    d.ellipse((320, 520, 500, 700), fill=(226, 212, 168), outline=(150, 136, 96), width=8)
    for k in range(9):
        a = math.radians(200 + k * 17.5)
        x0, y0 = 410 + 70 * math.cos(a), 610 + 70 * math.sin(a)
        x1, y1 = 410 + 84 * math.cos(a), 610 + 84 * math.sin(a)
        d.line((x0, y0, x1, y1), fill=(60, 50, 30), width=6)
    a = math.radians(300)
    d.line((410, 610, 410 + 72 * math.cos(a), 610 + 72 * math.sin(a)), fill=(168, 32, 36), width=8)
    d.ellipse((398, 598, 422, 622), fill=(60, 50, 30))
    # knobs and a lamp
    for x, y in ((570, 560), (670, 560)):
        d.ellipse((x - 34, y - 34, x + 34, y + 34), fill=(20, 20, 18), outline=(110, 110, 100), width=6)
        d.line((x, y - 30, x, y - 8), fill=(200, 200, 190), width=6)
    d.ellipse((600, 640, 640, 680), fill=(230, 160, 40))
    d.rectangle((560, 690, 704, 712), fill=(90, 90, 80))
    # handset on a cord, left of the case
    d.arc((150, 520, 330, 760), 90, 270, fill=(30, 30, 26), width=10)
    d.rounded_rectangle((150, 330, 220, 560), radius=30, fill=(30, 30, 26))
    d.ellipse((128, 300, 242, 380), fill=(30, 30, 26))
    d.ellipse((128, 510, 242, 590), fill=(30, 30, 26))
    save(im, "field-wireless.png")


drum()
radio()
