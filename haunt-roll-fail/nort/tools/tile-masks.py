#!/usr/bin/env python3
# Makes the territory masks (webp2/nort/images/tile/mask/<tile>-<area>.webp) and the free-space
# grid (nort/grid.scala) from the tile art and the area data in nort/tiles.scala.
#
# The borders on the tiles are roads with white (regular) or yellow (rough) dashes. Each area is
# seeded on the tile sides it owns, its territory number and its building spaces, and grown by a
# watershed on the blurred dash density, so neighbouring areas meet along the roads.
#
# The resource icons are cut out of the masks, so the tint doesn't cover them.
#
# The grid is 24 x 24 cells per tile. Each cell holds its area and how cluttered it is (0-9): busy
# art (icons, trees, rocks), building spaces, the territory number, the unit marker and the
# nearness of a border. The map uses it to put creatures, warchiefs and Kaija on free ground.
#
# Needs Pillow, numpy, scipy and scikit-image. Run from anywhere:
#   python3 haunt-roll-fail/nort/tools/tile-masks.py [--check DIR] [tile ...]
# --check writes an overlay per tile to DIR to look at.

import os, re, sys
import numpy as np
from PIL import Image
from scipy import ndimage as ndi
from skimage.segmentation import watershed

HERE = os.path.dirname(os.path.abspath(__file__))
NORT = os.path.dirname(HERE)
ROOT = os.path.dirname(NORT)
TILES = os.path.join(ROOT, 'webp2', 'nort', 'images', 'tile')
MASKS = os.path.join(TILES, 'mask')

N = 474          # working size
MASK = 237       # mask image size
GRID = 24        # grid cells per tile side
SIGMA = 5        # blur of the dash density
# Tiles cut from a photo have washed-out colours
PHOTO = ('start-5', 'tile-31', 'tile-32', 'tile-33')


def parse():
    tiles = {}
    cur = None
    for line in open(os.path.join(NORT, 'tiles.scala')):
        t = re.search(r'tile\("([^"]+)"\)\(', line)
        if t:
            cur = t.group(1)
            tiles[cur] = []
        a = re.search(r'area\("(\w+)", "(\w+)", ([\d.]+), ([\d.]+)(?:, ([\d.]+), ([\d.]+))?\)\((.*)\),', line)
        if a and cur:
            id, edges, x, y, ux, uy, feat = a.groups()
            x, y = float(x), float(y)
            ux = float(ux) if ux else x + 0.16
            uy = float(uy) if uy else y
            spaces = [(k, float(sx), float(sy)) for k, sx, sy in re.findall(r'(small|carved|large)\(([\d.]+), ([\d.]+)\)', feat)]
            tiles[cur].append(dict(id=id, edges=edges, x=x, y=y, ux=ux, uy=uy, spaces=spaces))
    return tiles


def dashes(a, photo):
    r, g, b = a[..., 0], a[..., 1], a[..., 2]
    spread = a.max(2) - a.min(2)
    white = (r > 180) & (g > 200) & (b > 195) & (spread < 70)
    yellow = (r > 190) & (g > 150) & (b < 120) & (r - b > 110) & (g - b > 80)
    pale = (r > 195) & (g > 205) & (b < 175) & (g - b > 55) & (r - b > 40)
    d = white | yellow | (pale & photo)
    # Dashes are small blobs; drop big light patches (snow, building spaces)
    lab, n = ndi.label(d)
    sizes = ndi.sum(d, lab, range(1, n + 1))
    ok = np.zeros(n + 1, bool)
    ok[1:] = (sizes >= 4) & (sizes <= 220)
    return ok[lab]


def segment(tid, areas, a):
    elev = ndi.gaussian_filter(dashes(a, tid in PHOTO).astype(float), SIGMA)
    seeds = np.zeros((N, N), int)
    m = int(N * 0.1)
    for i, ar in enumerate(areas):
        k = i + 1
        for e in ar['edges']:
            if e == 'N': seeds[0:2, m:N - m] = k
            if e == 'S': seeds[N - 2:N, m:N - m] = k
            if e == 'W': seeds[m:N - m, 0:2] = k
            if e == 'E': seeds[m:N - m, N - 2:N] = k
        # Not the unit marker: some sit over a border
        for px, py in [(ar['x'], ar['y'])] + [(sx, sy) for _, sx, sy in ar['spaces']]:
            cx, cy = int(px * N), int(py * N)
            seeds[max(0, cy - 3):cy + 4, max(0, cx - 3):cx + 4] = k
    lab = watershed(elev, seeds)
    # Smooth the outlines
    for _ in range(2):
        votes = np.stack([ndi.uniform_filter((lab == k + 1).astype(float), 9) for k in range(len(areas))])
        lab = votes.argmax(0) + 1
    return lab


# Resource icons the colour test misses on the photo tiles: centre and radius, in tile units
ICONS = {
    'start-5': [(0.207, 0.68, 0.05)],
    'tile-31': [(0.613, 0.85, 0.058)],
    'tile-32': [(0.82, 0.477, 0.067)],
}


def icons(tid, a):
    # Resource icons and the lore stones have a white rim; the food icon is a red apple
    rim = a.min(2) > 238
    red = (a[..., 0] > 110) & (a[..., 1] < 75) & (a[..., 2] < 85) & (a[..., 0] - a[..., 1] > 60)
    icon = ndi.binary_fill_holes(ndi.binary_closing(rim | red, iterations=5))
    lab, n = ndi.label(icon)
    sizes = ndi.sum(icon, lab, range(1, n + 1))
    keep = np.zeros(n + 1, bool)
    keep[1:] = sizes > 150
    icon = keep[lab]
    yy, xx = np.mgrid[0:N, 0:N] / N
    for x, y, r in ICONS.get(tid, []):
        icon |= (xx - x) ** 2 + (yy - y) ** 2 < r * r
    return icon


def clutter(tid, areas, a, lab):
    # Busy art: edges, with the insides of closed outlines (bushes, rocks) filled in
    gray = a.mean(2)
    grad = np.hypot(ndi.sobel(gray, 0), ndi.sobel(gray, 1))
    edges = ndi.maximum_filter(ndi.gaussian_filter(grad, 1), 5)
    busy = ndi.gaussian_filter(ndi.grey_closing(edges, size=(21, 21)), 3)
    busy = np.clip(busy / 300.0, 0, 1) * 6
    busy[ndi.binary_dilation(icons(tid, a), iterations=6)] = 9

    yy, xx = np.mgrid[0:N, 0:N] / N
    fixed = np.zeros((N, N))

    def disc(x, y, r, v):
        sel = (xx - x) ** 2 + (yy - y) ** 2 < r * r
        fixed[sel] = np.maximum(fixed[sel], v)

    def box(x, y, h, v):
        sel = (abs(xx - x) < h) & (abs(yy - y) < h)
        fixed[sel] = np.maximum(fixed[sel], v)

    for ar in areas:
        disc(ar['x'], ar['y'], 0.08, 9)
        disc(ar['ux'], ar['uy'], 0.15, 9)
        for k, sx, sy in ar['spaces']:
            box(sx, sy, (0.12 if k == 'large' else 0.1), 9)

    # Near a border: distance to another area, in tile units
    near = np.zeros((N, N))
    for k in range(len(areas)):
        inside = lab == k + 1
        d = ndi.distance_transform_edt(inside) / N
        near[inside] = np.clip((0.07 - d[inside]) / 0.07, 0, 1) * 8
    # A little off the tile edges, which may be open
    edge = np.minimum.reduce([xx, yy, 1 - xx, 1 - yy])
    near = np.maximum(near, np.clip((0.04 - edge) / 0.04, 0, 1) * 3)

    c = np.maximum(fixed, busy + near)
    return np.clip(c, 0, 9)


SYMBOLS = ['0123456789', 'abcdefghij', 'klmnopqrst', 'ABCDEFGHIJ']


def grid(lab, c):
    s = N / GRID
    rows = []
    for gy in range(GRID):
        row = ''
        for gx in range(GRID):
            y0, y1, x0, x1 = int(gy * s), int((gy + 1) * s), int(gx * s), int((gx + 1) * s)
            cell = lab[y0:y1, x0:x1]
            k = np.bincount(cell.ravel()).argmax() - 1
            v = int(round(c[y0:y1, x0:x1].mean()))
            row += SYMBOLS[k][v]
        rows.append(row)
    return rows


def main():
    args = sys.argv[1:]
    check = None
    if args[:1] == ['--check']:
        check = args[1]
        args = args[2:]
        os.makedirs(check, exist_ok=True)
    tiles = parse()
    ids = args or list(tiles)
    os.makedirs(MASKS, exist_ok=True)
    out = {}
    for tid in ids:
        areas = tiles[tid]
        im = Image.open(os.path.join(TILES, tid + '.webp')).convert('RGB').resize((N, N), Image.LANCZOS)
        a = np.asarray(im).astype(int)
        lab = segment(tid, areas, a)
        # The resource icons stay clear of the tint
        holes = ndi.binary_dilation(icons(tid, a), iterations=2)
        for k, ar in enumerate(areas):
            alpha = ndi.gaussian_filter(((lab == k + 1) & ~holes).astype(float), 1.0)
            m = Image.fromarray((alpha * 255).astype(np.uint8)).resize((MASK, MASK), Image.LANCZOS)
            rgba = Image.new('RGBA', (MASK, MASK), (255, 255, 255, 0))
            rgba.putalpha(m)
            rgba.save(os.path.join(MASKS, tid + '-' + ar['id'] + '.webp'), lossless=True)
        c = clutter(tid, areas, a, lab)
        out[tid] = grid(lab, c)
        if check:
            pal = [(255, 0, 0), (0, 90, 255), (255, 220, 0), (200, 0, 255)]
            col = np.zeros_like(a, dtype=float)
            for k in range(len(areas)):
                col[lab == k + 1] = pal[k]
            o = a * 0.55 + col * 0.45
            o[ndi.maximum_filter(lab, 3) != ndi.minimum_filter(lab, 3)] = (0, 0, 0)
            o = o * (1 - c[..., None] / 18)
            Image.fromarray(o.astype(np.uint8)).save(os.path.join(check, tid + '.png'))
        print(tid)

    if args:
        return

    with open(os.path.join(NORT, 'grid.scala'), 'w') as f:
        f.write('package nort\n//\n//\n//\n//\nimport hrf.colmat._\n//\n//\n//\n//\n\n')
        f.write('// Generated by nort/tools/tile-masks.py from the tile art; do not edit by hand.\n')
        f.write('// Each tile is 24 x 24 cells, rows from the top, unturned. A cell is its area (the index into the\n')
        f.write('// tile\'s areas: 0-9 for the first, a-j, k-t, A-J for the others) and its clutter, the digit or the\n')
        f.write('// letter\'s place in its group: 0 is open ground, 9 is a building space, a number or the unit marker.\n')
        f.write('object TileGrid {\n    val size = %d\n\n' % GRID)
        f.write('    val groups : $[String] = $(%s)\n\n' % ', '.join('"%s"' % g for g in SYMBOLS))
        f.write('    val cells : Map[String, $[String]] = Map(\n')
        for tid in ids:
            f.write('        "%s" -> $(\n' % tid)
            for row in out[tid]:
                f.write('            "%s",\n' % row)
            f.write('        ),\n')
        f.write('    )\n}\n')


if __name__ == '__main__':
    main()
