# The Doomed Expansion's Urban Legends maps for Root (slugfacekillah; the "And what is THIS" folder of its Drive):
# "Urban: Districts & Denizens" and "Legends", two boards of the usual size (UrbanBoard and LegendsBoard in root/maps.scala).
#
#   pdftoppm -r 91.65 -png UrbanLegends-MAPUrban.pdf U; pdftoppm -r 91.65 -png UrbanLegends-MAPLegends.pdf L
#   python3 root/tools/urban-legends-art.py <dir with U-1.png and L-1.png> <webp2/root/images>
#
# For each map (in urban/ and legends-map/, as legends/ holds the Legends Scenarios art): the board art (2417x2233), the regions image the pieces are placed by (a disc for
# each clearing, a dot at each forest's centre; the centres are the same as in maps.scala), the water mask (a quarter of the
# size, red on the rivers, found by their colour) and the clearing names, drawn like the Gorge map's.
import sys, os, shutil
import numpy as np
from PIL import Image, ImageDraw, ImageFont
from scipy import ndimage

src, out = sys.argv[1], sys.argv[2]

MAPS = {
    "urban": dict(
        art = "U-1.png",
        clearings = {
            "Tollgate": (311, 356), "Mill": (953, 311), "Guildhall": (1511, 429), "Belfry": (2108, 485),
            "Tannery": (869, 858), "Bazaar": (1544, 932), "Barracks": (340, 1219), "Wharf": (1140, 1385),
            "Abbey": (2136, 1225), "Sluice": (339, 1798), "Plaza": (1128, 1848), "Arcade": (2032, 1802),
        },
        forests = [(711, 508), (506, 811), (1219, 632), (1721, 615), (1929, 880), (1184, 1058), (783, 1154),
                   (1713, 1336), (736, 1562), (1433, 1678)],
        water = lambda r, g, b: (b > r + 12) & (b > g - 5) & (b > 90),
    ),
    "legends-map": dict(
        art = "L-1.png",
        clearings = {
            "Thornkeep": (333, 508), "Lumberyard": (1051, 315), "Stormkeep": (2070, 390), "Woodcutter": (800, 778),
            "Lodge": (1672, 712), "Hollow": (345, 1079), "Watchtower": (1344, 1162), "Smithy": (2081, 1046),
            "Hamlet": (867, 1427), "Oakkeep": (335, 1761), "Hermitage": (1290, 1847), "Redkeep": (2085, 1749),
        },
        forests = [(632, 670), (1174, 601), (1900, 640), (1474, 924), (839, 1111), (709, 1528), (1670, 1480)],
        water = lambda r, g, b: (b > r + 50) & (b > g + 10),
    ),
}

font = ImageFont.truetype("/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf", 38)

def name(text, path):
    im = Image.new("RGBA", (480, 60), (0, 0, 0, 0))
    d = ImageDraw.Draw(im)
    w = d.textlength(text, font = font)
    d.text(((480 - w) / 2, 8), text, font = font, fill = (38, 38, 38, 255), stroke_width = 5, stroke_fill = (255, 255, 255, 255))
    im.save(path, lossless = True)

for m, spec in MAPS.items():
    d = f"{out}/{m}"
    os.makedirs(d, exist_ok = True)

    art = Image.open(f"{src}/{spec['art']}").convert("RGB")
    art.save(f"{d}/map-art.webp", quality = 88)
    w, h = art.size

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

    # the water mask: the river's colour, keeping only the long stretches (not the bits of buildings in the same colours)
    a = np.asarray(art).astype(int)
    water = spec["water"](a[..., 0], a[..., 1], a[..., 2])
    lab, n = ndimage.label(ndimage.binary_dilation(water, iterations = 3))
    long = [i + 1 for i, s in enumerate(ndimage.find_objects(lab)) if max(s[0].stop - s[0].start, s[1].stop - s[1].start) > 150]
    keep = ndimage.binary_closing(np.isin(lab, long), iterations = 4)
    keep = ndimage.binary_dilation(keep, iterations = 4)
    mask = np.zeros((h, w, 4), np.uint8)
    mask[keep] = (255, 0, 0, 255)
    Image.fromarray(mask, "RGBA").resize((w // 4, h // 4), Image.NEAREST).save(f"{d}/map-water.webp", lossless = True)

    for n in spec["clearings"]:
        name(n, f"{d}/clearing-name-{n.lower()}.webp")

    shutil.copy(f"{out}/gorge/empty-building-white.webp", f"{d}/empty-building-white.webp")
