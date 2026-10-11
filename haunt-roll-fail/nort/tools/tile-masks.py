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
# --check writes an overlay per tile to DIR to look at. With tiles named (and no --check), only their entries in
# grid.scala are replaced.

import json, os, re, sys
import numpy as np
from PIL import Image, ImageDraw
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
# Wilderness tiles with impassable borders: solid orange lines
ORANGE = ('wild-poison', 'wild-peaks-1', 'wild-peaks-2', 'start-relic', 'start-lake', 'start-volcano', 'start-5-wall',
          'waste-kobold', 'waste-jotnar', 'waste-nastrond', 'horizon-bridge')
# Lines drawn into the borders where the art has none, in tile units: the Bridge's cliffs end at the bridge, which
# is left to the valley under it
BARRIERS = {'horizon-bridge': [((0.31, 0.39), (0.61, 0.35)), ((0.33, 0.65), (0.64, 0.655))]}
# Impassable middles inside an orange ring (Wastelands): no territory, left untinted
RINGED = ('start-relic', 'start-lake', 'start-volcano', 'waste-kobold', 'waste-jotnar', 'waste-nastrond')
# Wilderness, Wastelands and Uncharted Horizons tiles: figures keep off the rock bands along their borders
def expansion(tid):
    return tid.startswith(('wild-', 'waste-', 'horizon-')) or (tid.startswith('start-') and not tid.startswith('start-5'))
# Beach tiles (Sea module): the land, split along the dashes on the wings; the sea and the transparent parts are left out
BEACH = ('beach-port', 'beach-wing-w', 'beach-wing-e')


def land(rgba):
    r, g, b = rgba[..., 0], rgba[..., 1], rgba[..., 2]
    water = ndi.binary_opening(((b > g + 5) & (b > r + 15)) | (rgba[..., 3] < 128), iterations=2)
    lab, n = ndi.label(~water)
    sizes = ndi.sum(~water, lab, range(1, n + 1))
    keep = np.isin(lab, [i + 1 for i, s in enumerate(sizes) if s > 400])
    return ndi.binary_closing(keep, iterations=3)


# The sea of a Beach tile: the largest stretch of water (grey rocks pass for water too, so not every patch)
def sea(rgba):
    lab, n = ndi.label(~land(rgba))
    if n == 0:
        return np.zeros(lab.shape, bool)
    return lab == np.argmax(ndi.sum(np.ones(lab.shape), lab, range(1, n + 1))) + 1


def parse():
    tiles = {}
    cur = None
    for line in open(os.path.join(NORT, 'tiles.scala')):
        t = re.search(r'tile\("([^"]+)"\)\(', line)
        if t:
            cur = t.group(1)
            tiles[cur] = []
        a = re.search(r'area\("(\w+)", "(\w*)", ([\d.]+), ([\d.]+)(?:, ([\d.]+), ([\d.]+))?\)\((.*)\),', line)
        if a and cur:
            id, edges, x, y, ux, uy, feat = a.groups()
            x, y = float(x), float(y)
            ux = float(ux) if ux else x + 0.16
            uy = float(uy) if uy else y
            spaces = [(k, float(sx), float(sy)) for k, sx, sy in re.findall(r'(small|carved|large)\(([\d.]+), ([\d.]+)\)', feat)]
            tiles[cur].append(dict(id=id, edges=edges, x=x, y=y, ux=ux, uy=uy, spaces=spaces))
    return tiles


def orange(a):
    r, g, b = a[..., 0], a[..., 1], a[..., 2]
    return (r > 200) & (g > 90) & (g < 175) & (b < 90) & (r - g > 60)


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


# Tiles whose roads the colour tests miss (pale dashes on ice, ash or bare ground): borders traced by hand, as lines
# between their two areas, in tools/traced-borders.json
TRACED = json.load(open(os.path.join(HERE, 'traced-borders.json')))


def traced(tid):
    im = Image.new('L', (N, N), 0)
    draw = ImageDraw.Draw(im)
    for pts in TRACED[tid].values():
        draw.line([(x * N, y * N) for x, y in pts], fill=255, width=5)
    return np.asarray(im) > 0


def segment(tid, areas, a, sea=None):
    d = traced(tid) if tid in TRACED else dashes(a, tid in PHOTO)
    if tid in ORANGE:
        d = d | orange(a)
    for (x0, y0), (x1, y1) in BARRIERS.get(tid, []):
        for t in np.linspace(0, 1, 200):
            d[int((y0 + (y1 - y0) * t) * N), int((x0 + (x1 - x0) * t) * N)] = True
    elev = ndi.gaussian_filter(d.astype(float), SIGMA)
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
    # Beach tiles: the sea grows from the water up to the shore's dashes, taking the sand, against the grass; it is one
    # more region (dropped by the caller)
    n = len(areas)
    if sea is not None:
        r, g, b = a[..., 0], a[..., 1], a[..., 2]
        grass = ndi.binary_erosion((g > r + 15) & (g > b + 15) & ~sea, iterations=2)
        two = np.zeros((N, N), int)
        two[ndi.binary_erosion(sea, iterations=4)] = 1
        two[grass] = 2
        shore = watershed(elev, two) == 1
        n += 1
        lab[shore] = n
    # Smooth the outlines
    for _ in range(2):
        votes = np.stack([ndi.uniform_filter((lab == k + 1).astype(float), 9) for k in range(n)])
        lab = votes.argmax(0) + 1
    return lab


# Resource icons the colour test misses on the photo tiles: centre and radius, in tile units
ICONS = {
    'start-5': [(0.207, 0.68, 0.05)],
    'tile-31': [(0.613, 0.85, 0.058)],
    'tile-32': [(0.82, 0.477, 0.067)],
    # The Uncharted Horizons scans: lore stones with a dark rim, and dull white rims
    'horizon-2': [(0.3, 0.43, 0.065), (0.665, 0.875, 0.06)],
    'horizon-4': [(0.44, 0.87, 0.06)],
    'horizon-bridge': [(0.075, 0.555, 0.065)],
}


# Wastelands tiles with no resource icons, where lava, ice or stone would pass for one
NO_ICONS = ('start-magma', 'start-yggdrasil', 'start-relic', 'waste-thor', 'waste-urdarbrunn')


def icons(tid, a):
    if tid in NO_ICONS:
        return np.zeros(a.shape[:2], bool)
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


# The orange lines of impassable borders and rings, not the orange bushes: long thin pieces
def orange_lines(a):
    o = ndi.binary_dilation(orange(a), iterations=2)
    lab, n = ndi.label(o)
    keep = np.zeros(n + 1, bool)
    for k, sl in enumerate(ndi.find_objects(lab)):
        piece = lab[sl] == k + 1
        keep[k + 1] = piece.sum() > 150 and piece.sum() < 0.3 * piece.size
    return keep[lab]


# The yellow dashes of rough borders: dashes in a row, not a lone yellow flower or icon
def rough_dashes(a):
    r, g, b = a[..., 0], a[..., 1], a[..., 2]
    y = dashes(a, False) & (r > 190) & (g > 150) & (b < 120) & (r - b > 110) & (g - b > 80)
    lab, n = ndi.label(ndi.binary_dilation(y, iterations=12))
    count = ndi.sum(y, lab, range(1, n + 1))
    keep = np.zeros(n + 1, bool)
    keep[1:] = count > 150
    return y & keep[lab]


# Grey and slate-blue rocks (the walls along rough and impassable borders, the rock bands of some roads): big dull
# patches no greener than they are blue
def rocks(a):
    r, g, b = a[..., 0], a[..., 1], a[..., 2]
    v = a.mean(2)
    rock = ndi.binary_opening((b >= g - 8) & (b >= r) & (a.max(2) - a.min(2) < 70) & (v > 45) & (v < 185), iterations=2)
    lab, n = ndi.label(rock)
    keep = np.zeros(n + 1, bool)
    keep[1:] = ndi.sum(rock, lab, range(1, n + 1)) > 120
    return keep[lab]


def clutter(tid, areas, a, lab):
    # Busy art: edges, with the insides of closed outlines (bushes, rocks) filled in
    gray = a.mean(2)
    grad = np.hypot(ndi.sobel(gray, 0), ndi.sobel(gray, 1))
    edges = ndi.maximum_filter(ndi.gaussian_filter(grad, 1), 5)
    busy = ndi.gaussian_filter(ndi.grey_closing(edges, size=(21, 21)), 3)
    busy = np.clip(busy / 300.0, 0, 1) * 6
    if expansion(tid):
        rock = ndi.binary_dilation(rocks(a), iterations=3)
        busy[rock] = np.maximum(busy[rock], 7)
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
    # Wilderness and Wastelands: the cliffs, rock walls and ridges along an impassable or rough border, wider than
    # the road of a regular one
    if expansion(tid):
        for line, w in ((orange_lines(a), 0.1), (rough_dashes(a), 0.075)):
            if line.any():
                d = ndi.distance_transform_edt(~line) / N
                near = np.maximum(near, np.clip((w - d) / w * 2, 0, 1) * 8)

    c = np.maximum(fixed, busy + near)
    return np.clip(c, 0, 9)


SYMBOLS = ['0123456789', 'abcdefghij', 'klmnopqrst', 'ABCDEFGHIJ', 'KLMNOPQRST']
# A cell of no territory (the impassable middle inside an orange ring, the Great Lake's water)
NONE = '.'


def grid(lab, c, none=None):
    s = N / GRID
    rows = []
    for gy in range(GRID):
        row = ''
        for gx in range(GRID):
            y0, y1, x0, x1 = int(gy * s), int((gy + 1) * s), int(gx * s), int((gx + 1) * s)
            # Mostly inside an impassable ring or the Great Lake: no territory
            if none is not None and none[y0:y1, x0:x1].mean() > 0.5:
                row += NONE
                continue
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
    # The sea of a Beach has no area
    ids = args or [t for t in tiles if tiles[t]]
    os.makedirs(MASKS, exist_ok=True)
    out = {}
    for tid in ids:
        areas = tiles[tid]
        im = Image.open(os.path.join(TILES, tid + '.webp')).convert('RGBA' if tid in BEACH else 'RGB').resize((N, N), Image.LANCZOS)
        px = np.asarray(im).astype(int)
        a = px[..., :3]
        water = sea(px) if tid in BEACH else None
        lab = segment(tid, areas, a, water) if tid in BEACH else np.ones((N, N), int) if len(areas) == 1 else segment(tid, areas, a)
        # The resource icons stay clear of the tint
        holes = ndi.binary_dilation(icons(tid, a), iterations=2)
        if tid in BEACH:
            holes |= water | (px[..., 3] < 128) | (lab > len(areas))
        ringed = None
        # The Great Lake's water belongs to no territory
        if tid == 'wild-lake':
            r, g, b = a[..., 0], a[..., 1], a[..., 2]
            water = ndi.binary_opening((b > r + 25) & (b > g - 10) & (r < 110), iterations=3)
            lab_w, n = ndi.label(water)
            if n:
                big = np.argmax(ndi.sum(water, lab_w, range(1, n + 1))) + 1
                ringed = ndi.binary_fill_holes(lab_w == big)
                holes |= ringed
        if tid in RINGED:
            # The ring's outline can be broken where a border meets it, so its convex hull
            from skimage.morphology import convex_hull_image
            o = orange(a)
            lab_o, n = ndi.label(ndi.binary_dilation(o, iterations=2))
            if n:
                ring = lab_o == np.argmax(ndi.sum(o, lab_o, range(1, n + 1))) + 1
                # Pieces of the ring cut off by the art (the Jötnar Camp's tusks): the other long thin orange lines,
                # not the orange bushes
                for k, sl in enumerate(ndi.find_objects(lab_o)):
                    piece = lab_o[sl] == k + 1
                    if piece.sum() > 300 and piece.sum() < 0.3 * piece.size:
                        ring |= lab_o == k + 1
                # Inside the ring itself, line included; its convex hull where the outline is too broken to fill
                filled = ndi.binary_fill_holes(ndi.binary_closing(np.pad(ring, 30), iterations=6))[30:-30, 30:-30]
                if filled.sum() < 3 * ring.sum():
                    filled = ndi.binary_erosion(convex_hull_image(ring), iterations=6)
                holes |= filled
                ringed = filled
        for k, ar in enumerate(areas):
            alpha = ndi.gaussian_filter(((lab == k + 1) & ~holes).astype(float), 1.0)
            m = Image.fromarray((alpha * 255).astype(np.uint8)).resize((MASK, MASK), Image.LANCZOS)
            rgba = Image.new('RGBA', (MASK, MASK), (255, 255, 255, 0))
            rgba.putalpha(m)
            rgba.save(os.path.join(MASKS, tid + '-' + ar['id'] + '.webp'), lossless=True)
        # Beach tiles: the sea and the sand beyond the shore's dashes, for BorderLines.java (the shore's borders)
        if tid in BEACH:
            alpha = ndi.gaussian_filter(((lab > len(areas)) | water | (px[..., 3] < 128)).astype(float), 1.0)
            m = Image.fromarray((alpha * 255).astype(np.uint8)).resize((MASK, MASK), Image.LANCZOS)
            rgba = Image.new('RGBA', (MASK, MASK), (255, 255, 255, 0))
            rgba.putalpha(m)
            rgba.save(os.path.join(MASKS, tid + '-sea.webp'), lossless=True)
        c = clutter(tid, areas, a, lab)
        if tid in BEACH:
            c[ndi.binary_dilation(water | (px[..., 3] < 128), iterations=8) | (lab > len(areas))] = 9
        out[tid] = grid(np.where(lab > len(areas), 1, lab), c, ringed)
        if check:
            pal = [(255, 0, 0), (0, 90, 255), (255, 220, 0), (200, 0, 255), (0, 220, 120)]
            col = np.zeros_like(a, dtype=float)
            for k in range(len(areas)):
                col[lab == k + 1] = pal[k]
            o = a * 0.55 + col * 0.45
            o[ndi.maximum_filter(lab, 3) != ndi.minimum_filter(lab, 3)] = (0, 0, 0)
            o = o * (1 - c[..., None] / 18)
            Image.fromarray(o.astype(np.uint8)).save(os.path.join(check, tid + '.png'))
        print(tid)

    # Some tiles only: their entries replaced in the grid, the others kept (the other library versions make slightly
    # different grids)
    if check and args:
        return
    if args:
        path = os.path.join(NORT, 'grid.scala')
        text = open(path).read()
        for tid in ids:
            rows = ''.join('            "%s",\n' % row for row in out[tid])
            text, n = re.subn(r'(        "%s" -> \$\(\n)(?:            "[^"]*",\n)*' % re.escape(tid), lambda m: m.group(1) + rows, text)
            if n != 1:
                print('not in grid.scala:', tid)
        open(path, 'w').write(text)
        return

    with open(os.path.join(NORT, 'grid.scala'), 'w') as f:
        f.write('package nort\n//\n//\n//\n//\nimport hrf.colmat._\n//\n//\n//\n//\n\n')
        f.write('// Generated by nort/tools/tile-masks.py from the tile art; do not edit by hand.\n')
        f.write('// Each tile is 24 x 24 cells, rows from the top, unturned. A cell is its area (the index into the\n')
        f.write('// tile\'s areas: 0-9 for the first, a-j, k-t, A-J, K-T for the others) and its clutter, the digit or the\n')
        f.write('// letter\'s place in its group: 0 is open ground, 9 is a building space, a number or the unit marker.\n')
        f.write('// A "%s" is no territory (the impassable middle inside an orange ring, the Great Lake\'s water).\n' % NONE)
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
