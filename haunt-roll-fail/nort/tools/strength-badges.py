#!/usr/bin/env python3
# A creature's strength on the map: a badge with the battle die's axe and the creature's combat value, drawn at a creature
# token's bottom left corner (ui/strength-<n>.webp; Discord northgard-feedback, 2026-11-27, "Creature movements vs force":
# the strength in place of the creature line's order, which went onto the Creature Moves arrows, ui/order-<n>.webp,
# nort/tools/order-badges.py). Dark with a red rim, so it isn't taken for the gold order badges or the round unit counts.
# usage: python3 nort/tools/strength-badges.py   (from haunt-roll-fail/)

from PIL import Image, ImageDraw, ImageFont

S = 4
W, H = 112, 72
FONT = "/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf"
axe = Image.open("webp2/nort/images/ui/axe.webp").convert("RGBA")

def badge(n):
    w, h = W * S, H * S
    im = Image.new("RGBA", (w, h), (0, 0, 0, 0))
    d = ImageDraw.Draw(im)
    rim = (225, 60, 45, 255)
    d.rounded_rectangle([3 * S, 3 * S, w - 3 * S, h - 3 * S], 14 * S, fill=(26, 18, 16, 255), outline=rim, width=5 * S)
    a = axe.resize((46 * S, 46 * S), Image.LANCZOS)
    im.alpha_composite(a, (9 * S, (h - a.size[1]) // 2))
    t = str(n)
    font = ImageFont.truetype(FONT, (44 if len(t) == 1 else 36) * S)
    x0, y0, x1, y1 = d.textbbox((0, 0), t, font=font)
    cx = (56 + (W - 6)) / 2 * S
    d.text((cx - (x1 - x0) / 2 - x0, (h - (y1 - y0)) / 2 - y0), t, font=font, fill=(255, 255, 255, 255))
    return im.resize((W, H), Image.LANCZOS)

for n in range(1, 16):
    badge(n).save("webp2/nort/images/ui/strength-%d.webp" % n, lossless=True)
