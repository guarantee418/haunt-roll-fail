#!/usr/bin/env python3
# Goat Clan's sheep (Clan Fame upgrades, Shepherd): a small drawn sheep, token/sheep.webp
# usage: python3 nort/tools/sheep-token.py   (from haunt-roll-fail/)

from PIL import Image, ImageDraw, ImageFilter

S = 4
W = 200 * S

def draw():
    im = Image.new("RGBA", (W, W), (0, 0, 0, 0))
    d = ImageDraw.Draw(im)
    ink = (40, 34, 30, 255)
    wool = (246, 242, 232, 255)
    shade = (214, 206, 190, 255)
    face = (58, 50, 46, 255)
    o = 7 * S

    # legs
    for x in (62, 84, 118, 140):
        d.rounded_rectangle([x * S - 7 * S, 118 * S, x * S + 7 * S, 172 * S], 5 * S, fill=face, outline=ink, width=3 * S)

    # the woolly body: overlapping puffs, outlined first, then filled
    puffs = [(100, 98, 52), (62, 92, 30), (138, 92, 30), (78, 70, 28), (112, 66, 30), (140, 116, 24), (60, 118, 24), (100, 126, 30)]
    for (x, y, r) in puffs:
        d.ellipse([(x - r) * S - o, (y - r) * S - o, (x + r) * S + o, (y + r) * S + o], fill=ink)
    for (x, y, r) in puffs:
        d.ellipse([(x - r) * S, (y - r) * S, (x + r) * S, (y + r) * S], fill=shade)
    for (x, y, r) in puffs:
        d.ellipse([(x - r + 4) * S, (y - r + 2) * S, (x + r - 6) * S, (y + r - 8) * S], fill=wool)

    # head, ears and eye
    d.ellipse([22 * S, 58 * S, 66 * S, 112 * S], fill=face, outline=ink, width=4 * S)
    d.ellipse([10 * S, 62 * S, 34 * S, 76 * S], fill=face, outline=ink, width=3 * S)
    d.ellipse([52 * S, 54 * S, 76 * S, 68 * S], fill=face, outline=ink, width=3 * S)
    d.ellipse([32 * S, 74 * S, 42 * S, 84 * S], fill=(235, 230, 220, 255))
    # a tuft of wool on the head
    for (x, y, r) in [(42, 58, 11), (54, 60, 9)]:
        d.ellipse([(x - r) * S, (y - r) * S, (x + r) * S, (y + r) * S], fill=wool, outline=ink, width=3 * S)

    im = im.resize((200, 200), Image.LANCZOS)

    # a soft shadow under it, so it reads on any tile
    shadow = Image.new("RGBA", im.size, (0, 0, 0, 0))
    ImageDraw.Draw(shadow).ellipse([40, 160, 170, 186], fill=(0, 0, 0, 110))
    shadow = shadow.filter(ImageFilter.GaussianBlur(5))
    out = Image.alpha_composite(shadow, im)
    return out

if __name__ == "__main__":
    draw().save("webp2/nort/images/token/sheep.webp", "WEBP", lossless=True)
