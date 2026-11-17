# The Outsiders Expansion's two maps for Root (slugfacekillah; the Drive's MAP folder): the Deep Woods ("the past") and the
# Wastelands ("the future"), boards of the usual size (DeepWoodsBoard and WastelandsBoard in root/maps.scala).
#
#   pdftoppm -r 91.65 -png Outsiders-MAPdeepwoods.pdf D; pdftoppm -r 91.65 -png Outsiders-MAPwastelands.pdf W
#   pdftoppm -r 91.65 -png Outsiders-MAPtokens1.pdf T1; pdftoppm -r 91.65 -png Outsiders-MAPtokens2.pdf T2   (the clearing tiles)
#   python3 root/tools/outsiders-maps.py <dir with those renders> <webp2/root/images>
#
# For each map (in deep-woods/ and wastelands/): the board art (2417x2233), the regions image the pieces are placed by (a disc for
# each clearing, a dot at each forest's centre; the centres are the same as in maps.scala), the water mask (a quarter of the size,
# red on the rivers, found by their colour near a line traced by hand) and the clearing names, drawn like the Urban map's.
# The Deep Woods board is drawn settled, each clearing tile on the clearing of its number (the Law of Slug's way to play it without
# settling, S.1.7). Its building slots (`gates` in maps.scala) are the tiles' white squares, printed by this script.
import sys, os, shutil
import numpy as np
from PIL import Image, ImageDraw, ImageFont
from scipy import ndimage

src, out = sys.argv[1], sys.argv[2]

DEEP = {
    "Thicket": (376, 400, 1), "Pinecrest": (1414, 346, 5), "Bramble": (2005, 599, 2), "Oldroot": (891, 745, 9),
    "Deepden": (1472, 855, 10), "Wildmere": (369, 1072, 8), "Briarfold": (2060, 1161, 6), "Elderglen": (875, 1362, 12),
    "Shadowvale": (1545, 1374, 11), "Mossgrove": (407, 1760, 4), "Hazeldell": (1221, 1803, 7), "Fernbank": (2035, 1787, 3),
}

WASTE = {
    "Driftcamp": (345, 650), "Scrapyard": (900, 300), "Ashpit": (1320, 270), "Tollpost": (2050, 700),
    "Farmstead": (2080, 1260), "Manor": (2000, 1760), "Ferrytown": (1480, 1780), "Dugout": (560, 1780), "Junkshop": (300, 1370),
    "Cistern": (1060, 830), "Gatehouse": (840, 1180), "Granary": (1300, 1150),
}

MAPS = {
    "deep-woods": dict(
        art = "D-1.png",
        clearings = { k : (x, y) for k, (x, y, _) in DEEP.items() },
        forests = [(1237, 573), (650, 1260), (1190, 980), (1787, 1008), (1421, 1604), (848, 1650), (1879, 1467)],
        rivers = [[(0, 930), (180, 880), (380, 860), (500, 920), (700, 1080), (860, 1160), (1000, 1160), (1200, 1110), (1400, 1050),
                   (1500, 1010), (1640, 1060), (1800, 1180), (1950, 1300), (2050, 1360), (2200, 1400), (2417, 1390)]],
        colour = lambda r, g, b: (b > r + 40) & (b > g + 5),
        width = 70,
        extent = 110,
    ),
    "wastelands": dict(
        art = "W-1.png",
        clearings = WASTE,
        forests = [(760, 820), (1100, 450), (1580, 700), (1820, 1050), (1630, 1400), (1850, 1600), (850, 1500), (560, 1450), (480, 1050)],
        # two rivers meet at the Cistern; the drain's river, out of the walled city, runs past Ferrytown
        rivers = [[(0, 450), (120, 500), (300, 580), (430, 640), (600, 640), (800, 580), (900, 640), (1000, 750), (1050, 820)],
                  [(2417, 1000), (2300, 900), (2200, 850), (2100, 820), (1900, 800), (1760, 840), (1600, 860), (1440, 850), (1320, 840), (1200, 820), (1120, 820)],
                  [(1100, 1420), (1040, 1520), (1120, 1580), (1300, 1580), (1400, 1600), (1480, 1680), (1560, 1760), (1620, 1840), (1680, 1960), (1740, 2100), (1780, 2233)]],
        colour = lambda r, g, b: (abs(r - g) < 22) & (r > 105) & (r < 150) & (b > 50) & (b < 100) & (g - b > 30),
        width = 54,
    ),
}

# the clearing tiles: their sheet, the tile numbers in reading order, three across and two down
TILES = [("T1-1.png", [1, 2, 3, 4, 5, 6]), ("T2-1.png", [9, 7, 11, 10, 8, 12])]
TILE_R = 185      # the tile's art around its centre on the sheet
CIRCLE_R = 172    # drawn over the board's clearing circle (169)

font = ImageFont.truetype("/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf", 38)

def name(text, path):
    im = Image.new("RGBA", (480, 60), (0, 0, 0, 0))
    d = ImageDraw.Draw(im)
    w = d.textlength(text, font = font)
    d.text(((480 - w) / 2, 8), text, font = font, fill = (38, 38, 38, 255), stroke_width = 5, stroke_fill = (255, 255, 255, 255))
    im.save(path, lossless = True)

def settle(art):
    by = { n : (x, y, k) for k, (x, y, n) in DEEP.items() }
    s = CIRCLE_R / TILE_R
    for sheet, nums in TILES:
        im = Image.open(f"{src}/{sheet}").convert("RGB")
        a = np.asarray(im).astype(int)
        for i, n in enumerate(nums):
            cx, cy = [365, 780, 1195][i % 3], [297, 712][i // 3]
            tile = im.crop((cx - TILE_R, cy - TILE_R, cx + TILE_R, cy + TILE_R)).resize((2 * CIRCLE_R, 2 * CIRCLE_R), Image.LANCZOS)
            m = Image.new("L", tile.size, 0)
            ImageDraw.Draw(m).ellipse((0, 0, 2 * CIRCLE_R - 1, 2 * CIRCLE_R - 1), fill = 255)
            x, y, k = by[n]
            art.paste(tile, (x - CIRCLE_R, y - CIRCLE_R), m)
            ImageDraw.Draw(art).ellipse((x - CIRCLE_R, y - CIRCLE_R, x + CIRCLE_R, y + CIRCLE_R), outline = (238, 232, 208), width = 4)

            # the building slots: the white squares
            sub = a[cy - TILE_R:cy + TILE_R, cx - TILE_R:cx + TILE_R]
            white = sub.min(axis = 2) > 225
            yy, xx = np.mgrid[-TILE_R:TILE_R, -TILE_R:TILE_R]
            white &= xx ** 2 + yy ** 2 < 175 ** 2
            lab, _ = ndimage.label(white)
            slots = []
            for o in ndimage.find_objects(lab):
                h, w = o[0].stop - o[0].start, o[1].stop - o[1].start
                if 45 < h < 90 and 45 < w < 90:
                    slots.append((round(x + ((o[1].start + o[1].stop) / 2 - TILE_R) * s), round(y + ((o[0].start + o[0].stop) / 2 - TILE_R) * s)))
            print(k, n, slots)

for m, spec in MAPS.items():
    d = f"{out}/{m}"
    os.makedirs(d, exist_ok = True)

    art = Image.open(f"{src}/{spec['art']}").convert("RGB")
    w, h = art.size

    # the water mask: the river's colour near the rivers traced by hand (their bridges filled in)
    band = Image.new("L", (w, h), 0)
    bd = ImageDraw.Draw(band)
    for l in spec["rivers"]:
        bd.line(l, fill = 255, width = 220, joint = "curve")
    a = np.asarray(art).astype(int)
    water = spec["colour"](a[..., 0], a[..., 1], a[..., 2]) & (np.asarray(band) > 0)
    lab, _ = ndimage.label(ndimage.binary_dilation(water, iterations = 18))
    long = [i + 1 for i, o in enumerate(ndimage.find_objects(lab)) if max(o[0].stop - o[0].start, o[1].stop - o[1].start) > spec.get("extent", 250)]
    keep = ndimage.binary_closing(np.isin(lab, long) & ndimage.binary_dilation(water, iterations = 3), iterations = 12)
    keep = ndimage.binary_dilation(keep, iterations = 4)
    mask = np.zeros((h, w, 4), np.uint8)
    mask[keep] = (255, 0, 0, 255)
    Image.fromarray(mask, "RGBA").resize((w // 4, h // 4), Image.NEAREST).save(f"{d}/map-water.webp", lossless = True)

    if m == "deep-woods":
        settle(art)

    art.save(f"{d}/map-art.webp", quality = 88)

    # the regions: a disc per clearing (each its own colour) and a dot per forest
    reg = Image.new("RGBA", (w, h), (0, 0, 0, 0))
    rd = ImageDraw.Draw(reg)
    rng = np.random.RandomState(7)
    for (x, y) in spec["clearings"].values():
        c = tuple(int(v) for v in rng.randint(40, 250, 3)) + (255,)
        rd.ellipse((x - 165, y - 150, x + 165, y + 150), fill = c)
    for (x, y) in spec["forests"]:
        c = tuple(int(v) for v in rng.randint(40, 250, 3)) + (255,)
        rd.ellipse((x - 45, y - 45, x + 45, y + 45), fill = c)
    reg.save(f"{d}/map-regions.webp", lossless = True)

    for n in spec["clearings"]:
        name(n, f"{d}/clearing-name-{n.lower()}.webp")

    shutil.copy(f"{out}/gorge/empty-building-white.webp", f"{d}/empty-building-white.webp")
