#!/usr/bin/env python3
# The creature line's order on the map: a small square badge with the creature's place in the line
# (1 moves first), drawn at a creature token's corner (ui/order-<n>.webp; asked for on Discord on 2026-11-23).
# Square and gold on near-black, so it isn't mistaken for the round unit counts (ui/count-<n>)
# usage: python3 nort/tools/order-badges.py   (from haunt-roll-fail/)

from PIL import Image, ImageDraw, ImageFont

S = 4
W = 72
FONT = "/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf"

def badge(n):
    w = W * S
    im = Image.new("RGBA", (w, w), (0, 0, 0, 0))
    d = ImageDraw.Draw(im)
    gold = (240, 200, 80, 255)
    d.rounded_rectangle([3 * S, 3 * S, w - 3 * S, w - 3 * S], 10 * S, fill=(26, 22, 18, 255), outline=gold, width=5 * S)
    t = str(n)
    size = (44 if len(t) == 1 else 34) * S
    font = ImageFont.truetype(FONT, size)
    x0, y0, x1, y1 = d.textbbox((0, 0), t, font=font)
    d.text(((w - (x1 - x0)) / 2 - x0, (w - (y1 - y0)) / 2 - y0), t, font=font, fill=gold)
    return im.resize((W, W), Image.LANCZOS)

for n in range(1, 31):
    badge(n).save("webp2/nort/images/ui/order-%d.webp" % n, lossless=True)
