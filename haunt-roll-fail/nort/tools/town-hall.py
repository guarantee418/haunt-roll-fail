#!/usr/bin/env python3
# PC version (fan content): the Town Hall tokens, token/building/town-hall.webp and town-hall-upgraded.webp, cut to the octagon
# of the other large buildings (the Forge's outline).
# The Town Hall is the thatched hall of the Development card Town Hall (card/dev/town-hall.webp). The upgraded one is the
# Northgard video game's upgraded hall (slate roof), from the game wiki (https://northgard.fandom.com/wiki/File:Town_Hall_icon.png,
# the render on grass, kept here as town-hall-source.webp); Gman, 2026-11-11.
# Run from haunt-roll-fail/: python3 nort/tools/town-hall.py
from PIL import Image, ImageFilter

out = 'webp2/nort/images/token/building/'
octagon = Image.open(out + 'forge.webp').convert('RGBA').split()[-1]
w, h = octagon.size

def token(src, box, name):
    art = src.crop(box).resize((w, h), Image.LANCZOS).filter(ImageFilter.UnsharpMask(radius=2, percent=40, threshold=2)).convert('RGBA')
    art.putalpha(octagon)
    art.save(out + name, quality=90)

# The card's hall, a square around it (the card is 412x617)
card = Image.open('webp2/nort/images/card/dev/town-hall.webp').convert('RGB')
token(card, (22, 160, 392, 530), 'town-hall.webp')

# The wiki render: scale so the shorter side covers the octagon, centred (the hall sits a little high in the source)
src = Image.open('nort/tools/town-hall-source.webp').convert('RGB')
sw, sh = src.size
k = max(w / sw, h / sh) * 1.04
cw, ch = w / k, h / k
x, y = (sw - cw) / 2, (sh - ch) / 2 + 4
token(src, (round(x), round(y), round(x + cw), round(y + ch)), 'town-hall-upgraded.webp')
