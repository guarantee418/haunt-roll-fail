#!/usr/bin/env python3
# Face-down relics as drawn on the Root map (Keepers in Iron, ki-*, and the old
# Keepers, ok-*): the board tokens are half-transparent grey discs that vanish on
# the green forest art, so the map draws <name>-map instead: the same picture on
# an opaque disc, a little more contrast, a gold rim and a dark shadow around it.
# Run from haunt-roll-fail/: python3 root/tools/relic-hidden-map.py
from PIL import Image, ImageDraw, ImageEnhance, ImageFilter

BASE = "webp2/root/images/faction/"
SIZE = 120          # output canvas; the 90 px disc sits in the middle
RIM = (255, 214, 64, 255)

def make(path):
    src = Image.open(BASE + path + ".webp").convert("RGBA")
    w, h = src.size
    # opaque disc behind the token's own half-transparent one
    back = Image.new("RGBA", src.size, (0, 0, 0, 0))
    ImageDraw.Draw(back).ellipse((1, 1, w - 2, h - 2), fill=(70, 74, 84, 255))
    tok = Image.alpha_composite(back, src)
    tok = ImageEnhance.Contrast(tok).enhance(1.35)
    tok = ImageEnhance.Brightness(tok).enhance(1.1)

    out = Image.new("RGBA", (SIZE, SIZE), (0, 0, 0, 0))
    o = (SIZE - w) // 2
    # dark shadow ring, blurred
    sh = Image.new("RGBA", (SIZE, SIZE), (0, 0, 0, 0))
    ImageDraw.Draw(sh).ellipse((o - 9, o - 9, o + w + 8, o + h + 8), fill=(0, 0, 0, 230))
    out = Image.alpha_composite(out, sh.filter(ImageFilter.GaussianBlur(4)))
    # gold rim
    ImageDraw.Draw(out).ellipse((o - 5, o - 5, o + w + 4, o + h + 4), fill=RIM)
    ImageDraw.Draw(out).ellipse((o - 1, o - 1, o + w, o + h), fill=(20, 20, 20, 255))
    out.alpha_composite(tok, (o, o))
    out.save(BASE + path + "-map.webp", lossless=True)

for style, folder in (("ki", "expedition"), ("ok", "old-expedition")):
    for rt in ("tablet", "jewel", "idol"):
        make(folder + "/" + style + "-" + rt + "-hidden")
