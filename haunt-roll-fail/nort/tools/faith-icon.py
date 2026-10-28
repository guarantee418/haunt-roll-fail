#!/usr/bin/env python3
# The Lion Kingdom's Faith (LionFaith, fan content): the icon (ui/faith.webp) is the Northgard video game's Faith icon, a white rune.
# The only copy found is the game wiki's 24x27 one (https://northgard.fandom.com/wiki/File:Faith.png, kept here as faith-source.png);
# it is scaled up and set on a dark blue disc with a gold rim, like the round lore token, so it shows on the light panels.
# Run from haunt-roll-fail/: python3 nort/tools/faith-icon.py
from PIL import Image, ImageDraw, ImageFilter

S = 4
N = 64 * S
src = Image.open('nort/tools/faith-source.png').convert('RGBA')
src = src.crop(src.getbbox())

disc = Image.new('RGBA', (N, N), (0, 0, 0, 0))
d = ImageDraw.Draw(disc)
d.ellipse((2 * S, 2 * S, N - 2 * S, N - 2 * S), fill=(196, 160, 78, 255))
d.ellipse((6 * S, 6 * S, N - 6 * S, N - 6 * S), fill=(30, 44, 86, 255))
# a soft light from the top, like the other round tokens
shade = Image.new('L', (N, N), 0)
ImageDraw.Draw(shade).ellipse((10 * S, 4 * S, N - 10 * S, N // 2), fill=60)
disc.paste(Image.new('RGBA', (N, N), (120, 150, 210, 255)), (0, 0), Image.composite(shade.filter(ImageFilter.GaussianBlur(10 * S)), Image.new('L', (N, N), 0), disc.split()[3].point(lambda v: 255 if v > 0 else 0)).point(lambda v: v // 2))

h = 40 * S
rune = src.resize((round(src.width * h / src.height), h), Image.BICUBIC).filter(ImageFilter.UnsharpMask(radius=2 * S, percent=60, threshold=2))
# brighten it to the near white the video game draws it in
r, g, b, a = rune.split()
rune = Image.merge('RGBA', (r.point(lambda v: min(255, v + 20)), g.point(lambda v: min(255, v + 20)), b.point(lambda v: min(255, v + 30)), a))
disc.alpha_composite(rune, ((N - rune.width) // 2, (N - rune.height) // 2))
disc.resize((64, 64), Image.LANCZOS).save('webp2/nort/images/ui/faith.webp', lossless=True)
