#!/usr/bin/env python3
# PC version (fan content): the Town Hall tokens, token/building/town-hall.webp and town-hall-upgraded.webp.
# The hall is the Northgard video game's, from the game wiki (https://northgard.fandom.com/wiki/File:Town_Hall_icon.png, the
# render on grass, kept here as town-hall-source.webp), cut to the octagon of the other large buildings (the Forge's outline).
# The wiki has no picture of the upgraded hall, so until there is one the upgraded token is the same hall in a gold frame with
# a gold upgrade chevron in its top left corner.
# Run from haunt-roll-fail/: python3 nort/tools/town-hall.py
from PIL import Image, ImageDraw, ImageFilter, ImageChops

out = 'webp2/nort/images/token/building/'
octagon = Image.open(out + 'forge.webp').convert('RGBA').split()[-1]
w, h = octagon.size

src = Image.open('nort/tools/town-hall-source.webp').convert('RGB')
sw, sh = src.size
# Fill the octagon: scale so the shorter side covers it, then centre (the hall sits a little high in the source)
k = max(w / sw, h / sh) * 1.04
big = src.resize((round(sw * k), round(sh * k)), Image.LANCZOS).filter(ImageFilter.UnsharpMask(radius=2, percent=50, threshold=2))
x = (big.width - w) // 2
y = (big.height - h) // 2 + round(4 * k)
art = big.crop((x, y, x + w, y + h)).convert('RGBA')

hall = art.copy()
hall.putalpha(octagon)
hall.save(out + 'town-hall.webp', quality=90)

# Upgraded: a gold frame along the octagon's edge and a chevron badge
up = art.copy()
gold = Image.new('RGBA', (w, h), (232, 184, 60, 255))
# (padded first, so the straight edges along the image's border erode too)
padded = Image.new('L', (w + 40, h + 40), 0)
padded.paste(octagon, (20, 20))
ring = ImageChops.subtract(padded, padded.filter(ImageFilter.MinFilter(25))).crop((20, 20, 20 + w, 20 + h))
up.paste(gold, (0, 0), ring)

d = ImageDraw.Draw(up)
cx, cy, r = round(w * 0.24), round(h * 0.24), round(w * 0.15)
d.ellipse((cx - r, cy - r, cx + r, cy + r), fill=(40, 30, 20, 255), outline=(232, 184, 60, 255), width=8)
for dy in (-r * 0.28, r * 0.22):
    d.line([(cx - r * 0.5, cy + dy + r * 0.25), (cx, cy + dy - r * 0.25), (cx + r * 0.5, cy + dy + r * 0.25)], fill=(250, 210, 80, 255), width=round(r * 0.22), joint='curve')

up.putalpha(octagon)
up.save(out + 'town-hall-upgraded.webp', quality=90)
