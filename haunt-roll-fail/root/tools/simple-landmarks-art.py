#!/usr/bin/env python3
# Cuts the Landmarks Pack pieces (Legendary Forge, Black Market, Elder Treetop) out of their Leder Card Library cards for the
# Simple Landmarks (root/simple-landmarks.scala): feature/simple-forge, feature/simple-market and feature/royal-tree, outlined
# like feature/tower. The Birdsong Tower uses feature/tower itself.
#
#   python3 simple-landmarks-art.py <dir with landmark-legendaryforge.webp, landmark-blackmarket.webp, landmark-treetop.webp>
#
# from https://ledercards.netlify.app/cards/root/en-US/<name>.webp (the cards are 680x926).
import sys, os
from PIL import Image, ImageFilter, ImageDraw
import numpy as np

src = sys.argv[1]
out = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', '..', 'webp2', 'root', 'images', 'feature')

pieces = {
    'simple-forge': ('landmark-legendaryforge.webp', (300, 630, 660, 900)),
    'simple-market': ('landmark-blackmarket.webp', (300, 610, 660, 900)),
    'royal-tree': ('landmark-treetop.webp', (240, 510, 640, 910)),
}

HEIGHT = 130   # the drawn height, the Tower being 153 with its spire

for name, (card, box) in pieces.items():
    im = Image.open(os.path.join(src, card)).convert('RGB').crop(box)
    a = np.asarray(im).astype(int)
    mx = a.max(axis=2)
    mn = a.min(axis=2)
    sat = (mx - mn) / np.maximum(mx, 1)
    piece = (mx < 150) | ((sat > 0.30) & (mx > 60))
    mask = Image.fromarray((piece * 255).astype('uint8'))
    # close small gaps, then fill the holes from outside
    mask = mask.filter(ImageFilter.MaxFilter(5)).filter(ImageFilter.MinFilter(5))
    w, h = mask.size
    framed = Image.new('L', (w + 2, h + 2), 0)
    framed.paste(mask, (1, 1))
    ImageDraw.floodfill(framed, (0, 0), 128)
    filled = np.asarray(framed)[1:-1, 1:-1] != 128
    # keep the largest blob only (the card's text and sketches are elsewhere)
    m = filled.copy()
    seen = np.zeros_like(m)
    best = None
    for y in range(h):
        for x in range(w):
            if m[y, x] and not seen[y, x]:
                stack = [(y, x)]
                seen[y, x] = True
                pts = []
                while stack:
                    cy, cx = stack.pop()
                    pts.append((cy, cx))
                    for ny, nx in ((cy + 1, cx), (cy - 1, cx), (cy, cx + 1), (cy, cx - 1)):
                        if 0 <= ny < h and 0 <= nx < w and m[ny, nx] and not seen[ny, nx]:
                            seen[ny, nx] = True
                            stack.append((ny, nx))
                if best is None or len(pts) > len(best):
                    best = pts
    keep = np.zeros_like(m)
    for (y, x) in best:
        keep[y, x] = True
    ys, xs = np.nonzero(keep)
    pad = 8
    x0, y0, x1, y1 = xs.min() - pad, ys.min() - pad, xs.max() + pad + 1, ys.max() + pad + 1
    rgba = Image.new('RGBA', (x1 - x0, y1 - y0), (0, 0, 0, 0))
    piece_alpha = Image.fromarray((keep * 255).astype('uint8')).crop((x0, y0, x1, y1))
    # the outline: a translucent white ring around the piece, as on feature/tower
    ring = piece_alpha.filter(ImageFilter.MaxFilter(9)).point(lambda v: 130 if v else 0)
    rgba.paste(Image.new('RGBA', rgba.size, (255, 255, 255, 255)), (0, 0), ring)
    rgba.paste(im.crop((x0, y0, x1, y1)).convert('RGBA'), (0, 0), piece_alpha)
    k = HEIGHT / rgba.size[1]
    rgba = rgba.resize((round(rgba.size[0] * k), HEIGHT), Image.LANCZOS)
    rgba.save(os.path.join(out, name + '.webp'), lossless=True)
    print(name, rgba.size)
