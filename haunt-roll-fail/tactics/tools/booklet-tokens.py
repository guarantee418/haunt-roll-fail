#!/usr/bin/env python3
# Finds the resource tokens (Control Points) on a Mission Booklet picture, for the missions the Tabletop Simulator mod has no bag for:
# booklet-tokens.py <page.png> <x0> <y0> <x1> <y1> <out.json> [overlay.png], the map's box in the page picture's pixels. Each token is a
# round counter of one colour: Victory Points navy, Manpower yellow, Munitions red, Fuel green. Pixels of each colour are opened (so the
# thin cover lines go) and cut into blobs; a big round token is a point of one resource, and a blob much longer than it is wide two
# small tokens of that kind side by side. Tokens whose centres
# are close are one Control Point (the booklet draws a point's two tokens overlapping); the json lists each point's centre as fractions
# of the map's width and height, with its resources in V, M, U, F order ("points"), and every blob ("blobs", the box as fractions),
# for missions-grid.py to group by hex, as two points in neighbouring hexes may touch.
import sys, json
import numpy as np
from PIL import Image, ImageDraw
from scipy import ndimage
src, X0, Y0, X1, Y1, out = sys.argv[1], *map(float, sys.argv[2:6]), sys.argv[6]
img = Image.open(src).convert('RGB').crop((int(X0), int(Y0), int(X1), int(Y1)))
a = np.asarray(img).astype(np.int32)
r, g, b = a[..., 0], a[..., 1], a[..., 2]
W, H = img.size
# the token's size in this picture: about a third of a hex (a hex is 923/6000 of a panel's width)
panels = round(W / (H / 1.5)) if W > H * 1.2 else 2
hexw = W / (6000 * panels) * 923 if W > H * 1.2 else W / 12000 * 923
d = hexw * 0.45  # a small token (two of them make a Control Point of two resources; a big one is a point of one)
masks = {
    'V': (r > 6) & (r < 34) & (g > 38) & (g < 66) & (b > 68) & (b < 102) & (b > g + 22),  # the token's navy is flat; snow shadows aren't
    'M': (r > 130) & (g > 120) & (b < 25) & (abs(r - g) < 14),  # the cover lines' yellow is bluer, and redder or greener
    'U': (r > 190) & (g < 110) & (g > 30) & (b < 70),
    'F': (g > 155) & (g > r + 75) & (g > b + 100) & (r > 50) & (r < 105),  # the token's green is brighter than the cover lines'
}
blobs = []
for k, m0 in masks.items():
    m = ndimage.binary_closing(m0, iterations=3)
    m = ndimage.binary_fill_holes(m)
    m = ndimage.binary_opening(m, iterations=max(2, int(d * 0.07)))
    lab, n = ndimage.label(m)
    for i, sl in enumerate(ndimage.find_objects(lab)):
        h = sl[0].stop - sl[0].start; w = sl[1].stop - sl[1].start
        area = (lab[sl] == i + 1).sum()
        if max(w, h) < d * 0.35 or area < (d * 0.3) ** 2: continue
        # a token is mostly its colour; a closed cover outline (the Monastery's yellow hexes) fills in, but is mostly not
        px = (m0[sl] & (lab[sl] == i + 1)).sum()
        if px < area * 0.4: continue
        blobs.append((k, sl[1].start, sl[0].start, sl[1].stop, sl[0].stop, px))
# a Control Point: blobs (of any colour) that touch (its two tokens overlap); a colour's blobs in it, taken together, are one token, or two
# when they are longer than one small token and than they are wide (two small tokens of a kind side by side; a big token is round)
def near(p, q):
    return max(p[1], q[1]) - min(p[3], q[3]) < d * 0.3 and max(p[2], q[2]) - min(p[4], q[4]) < d * 0.3
groups = []
for p in blobs:
    hit = [gr for gr in groups if any(near(p, q) for q in gr)]
    for gr in hit: groups.remove(gr)
    groups.append(sum(hit, []) + [p])
res = []
for gr in groups:
    x0 = min(q[1] for q in gr); y0 = min(q[2] for q in gr); x1 = max(q[3] for q in gr); y1 = max(q[4] for q in gr)
    rs = ''
    for k in 'VMUF':
        ks = [q for q in gr if q[0] == k]
        if not ks: continue
        w = max(q[3] for q in ks) - min(q[1] for q in ks); h = max(q[4] for q in ks) - min(q[2] for q in ks)
        rs += k * (2 if max(w, h) > d * 1.4 and max(w, h) > min(w, h) * 1.15 else 1)
        if '-v' in sys.argv: print(k, round(w / d, 2), round(h / d, 2), round((x0 + x1) / 2), round((y0 + y1) / 2))
    res.append({'x': (x0 + x1) / 2 / W, 'y': (y0 + y1) / 2 / H, 'r': rs})
json.dump({'points': res, 'd': d / W, 'blobs': [{'k': q[0], 'box': [q[1] / W, q[2] / H, q[3] / W, q[4] / H], 'px': q[5] / (W * H)} for q in blobs]}, open(out, 'w'), indent=0)
print(len(res), 'points', sum(len(x['r']) for x in res), 'tokens', sorted(x['r'] for x in res))
if len(sys.argv) > 7 and sys.argv[7] != '-v':
    dr = ImageDraw.Draw(img)
    for x in res:
        dr.ellipse((x['x'] * W - 6, x['y'] * H - 6, x['x'] * W + 6, x['y'] * H + 6), fill=(255, 0, 255))
        dr.text((x['x'] * W + 8, x['y'] * H - 8), x['r'], fill=(255, 255, 255))
    img.save(sys.argv[7])
