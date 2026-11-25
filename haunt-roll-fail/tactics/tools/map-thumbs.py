#!/usr/bin/env python3
# The setup screen's map thumbnails (tactics/meta.scala, Meta.optionTile): map-thumbs.py <mapdump.tsv> <images dir>, the dump printed by
# `sbt "runMain tactics.MapDump"` (each Map choice's picture, size, Spawn Areas and Control Points). Each choice gets
# <images>/thumb/<choice>.webp: its map picture made to fit 360 x 240, the Spawn Areas tinted in each side's color (as the game draws
# them: blue, red, then green, yellow and purple for the free-for-alls) and each Control Point a dot, gold where it gives VP, white
# else, so missions on the same panels look different.
import sys, os, math
from PIL import Image, ImageDraw
dump, images = sys.argv[1], sys.argv[2]
os.makedirs(os.path.join(images, 'thumb'), exist_ok=True)
BOX = (360, 240)
SIDE = 100.2
COLORS = [(40, 110, 230), (220, 40, 40), (40, 170, 60), (230, 200, 30), (150, 60, 200)]
for line in open(dump):
    if not line.startswith('MAP\t'): continue
    _, choice, image, w, h, spawns, points = line.rstrip('\n').split('\t')
    w, h = int(w), int(h)
    src = Image.open(os.path.join(images, image + '.webp')).convert('RGB')
    k = min(BOX[0] / w, BOX[1] / h)
    tw, th = round(w * k), round(h * k)
    base = src.resize((tw, th), Image.LANCZOS).convert('RGBA')
    over = Image.new('RGBA', base.size, (0, 0, 0, 0))
    d = ImageDraw.Draw(over)
    for i, team in enumerate(spawns.split('|')):
        c = COLORS[i % len(COLORS)]
        for p in team.split(';'):
            if not p: continue
            x, y = (float(v) * k for v in p.split(','))
            s = SIDE * k
            hexagon = [(x + s * math.sin(math.radians(a)), y - s * math.cos(math.radians(a))) for a in range(0, 360, 60)]
            d.polygon(hexagon, fill=c + (150,))
    for p in points.split(';'):
        if not p: continue
        x, y, res = p.split(',')
        x, y = float(x) * k, float(y) * k
        r = max(4.5, 34 * k)
        fill = (255, 205, 40) if 'vp' in res else (245, 245, 245)
        d.ellipse((x - r, y - r, x + r, y + r), fill=fill + (255,), outline=(0, 0, 0, 255), width=2)
    out = Image.alpha_composite(base, over).convert('RGB')
    out.save(os.path.join(images, 'thumb', choice + '.webp'), 'WEBP', quality=80, method=6)
    print(choice, out.size)
