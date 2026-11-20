#!/usr/bin/env python3
# The Capitalists and Anarchists deck's card pictures (`CapitalistsDeck`, root/deck-capitalists.scala): each is the Might and Right
# card with its cost plank swapped for one with the new cost (taken from a card that has it) and its text box repainted with the
# new text, gray for lasting effects, the parchment tan of the Favors for effects used once when crafted.
#
#   python3 -I root/tools/capitalists-art.py <luminari.ttf> <webp2/root/images/card/deck> <webp2/root/images/card/capitalists>
#
# luminari.ttf is fonts/luminari.woff unpacked to a TrueType file (the woff tables are zlib streams).

import sys
from PIL import Image, ImageDraw, ImageFont

font, deck, out = sys.argv[1:4]

# card, plank donor (a card with the new cost), title (None: no title, as printed), text, tan box
CARDS = [
    ("command-warren", "a-visit-to-friends", "Command Warren", "At start of Daylight, may initiate a battle. Spend a card matching its clearing, or discard this.", False),
    ("cobbler", "a-visit-to-friends", "Cobbler", "At start of Evening, may take a move. Spend a card matching the clearing moved from, or discard this.", False),
    ("better-burrow-bank", "a-visit-to-friends", None, "At start of Birdsong, you may return this card to your hand to draw one card. If you do, choose an enemy to draw one card.", False),
    ("brutal-tactics", "armorers", "Brutal Tactics", "In battle as attacker, may deal an extra hit, but defender scores one point. Spend a card matching the clearing, or discard this.", False),
    ("stand-and-deliver", "codebreakers", "Stand and Deliver!", "Take a random card from an enemy. They must draw one card. Then discard this card.", True),
    ("tax-collector", "saboteurs", "Tax Collector", "Remove a warrior of your faction from a clearing on the map to draw two cards, then discard.", True),
    ("scouting-party", "codebreakers", "Scouting Party", "As attacker in battle, may discard this to cancel an ambush card played against you.", False),
    ("royal-claim", None, "Royal Claim", "In Birdsong, must discard this to score one point per clearing you rule.", False),
    ("codebreakers", None, "Codebreakers", "Look at another player's hand, then discard.", True),
]

INK = (35, 31, 32)
TAN = (251, 233, 196)


def wood(p):
    r, g, b = p
    return r > 150 and 95 < g < 190 and b < 130 and r - b > 70


def plank_box(im):
    # every Might and Right card has its cost plank here
    return 18, 561, 100, 679


def light(p):
    r, g, b = p
    return min(r, g, b) > 170 and max(r, g, b) - min(r, g, b) < 28


def text_box(im):
    # every Might and Right effect card has its text box here
    return 119, 540, 483, 678


def fill(im, box, tan):
    l, t, r, b = box
    px = im.load()
    w, h = im.size
    for y in range(max(0, t - 6), min(h, b + 7)):
        for x in range(max(0, l - 6), min(w, r + 7)):
            p = px[x, y]
            if tan and light(p):
                k = sum(p) / 3 / 225
                px[x, y] = tuple(min(255, int(c * k)) for c in TAN)
    base = TAN if tan else median(im, box)
    d = ImageDraw.Draw(im)
    d.rectangle((l + 6, t + 5, r - 6, b - 5), fill=base)


def median(im, box):
    l, t, r, b = box
    s = sorted(im.getpixel((x, y)) for y in range(t, b, 3) for x in range(l, r, 3) if light(im.getpixel((x, y))))
    return s[len(s) // 2]


def wrap(text, f, width):
    lines, cur = [], ""
    for word in text.split():
        t = (cur + " " + word).strip()
        if f.getlength(t) <= width:
            cur = t
        else:
            lines.append(cur)
            cur = word
    return lines + [cur]


def write(im, box, title, text):
    l, t, r, b = box
    width, height = r - l - 18, b - t - 10
    d = ImageDraw.Draw(im)
    for size in range(27, 15, -1):
        bf = ImageFont.truetype(font, size)
        tf = ImageFont.truetype(font, size + 7)
        lines = wrap(text, bf, width)
        lh = int(size * 1.12)
        th = (size + 10) if title else 0
        if th + lh * len(lines) <= height:
            break
    y = t + 5 + (height - th - lh * len(lines)) / 2
    cx = (l + r) / 2
    if title:
        d.text((cx, y), title, font=tf, fill=INK, anchor="mt")
        y += th
    for line in lines:
        d.text((cx, y), line, font=bf, fill=INK, anchor="mt")
        y += lh


for name, donor, title, text, tan in CARDS:
    im = Image.open(f"{deck}/{name}.webp").convert("RGB")
    if donor:
        dm = Image.open(f"{deck}/{donor}.webp").convert("RGB")
        db = plank_box(dm)
        tb = plank_box(im)
        im.paste(dm.crop(db).resize((tb[2] - tb[0], tb[3] - tb[1]), Image.LANCZOS), tb[:2])
    box = text_box(im)
    fill(im, box, tan)
    write(im, box, title, text)
    im.save(f"{out}/ca-{name}.webp", quality=90)
    print(name, box)
