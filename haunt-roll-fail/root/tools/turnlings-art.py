# Art for the Doomed Expansion's Turnlings (Law of Doom D8), from its print-and-play files:
#   python3 turnlings-art.py <Turnlings-CARDS(70x120mm) dir> <stickers page at 300 dpi> <webp2/root/images/faction/hireling>
# - the cards (staunch on the front, demoted on the back), turned so their text reads across, at the other hireling cards' 800x483
# - the pieces cut from the stickers (each sticker is the connected shape around a seed point), and, for the pieces with no sticker
#   (Cardinals, Caravans, Doomsayers), the head drawn in the card's title bar
import sys
import numpy as np
from PIL import Image, ImageDraw
from scipy import ndimage

cards, stickers, out = sys.argv[1], sys.argv[2], sys.argv[3]

names = {
    "Whisperer": "tmm", "WhispererD": "tfc", "Cardinals": "tra", "CardinalsD": "tcc", "Infected": "tin", "InfectedD": "tis",
    "Golden": "tgk", "GoldenD": "tgp", "Union": "tum", "Union-D": "tco", "Vultures": "tcv", "Vultures-D": "tbf",
    "Bugs": "tbb", "BugsD": "tmc", "Pigs": "tgg", "PigsD": "tfs",
}

for f, style in names.items():
    im = Image.open(f"{cards}/{f}.png").convert("RGB").rotate(90, expand=True)
    im.resize((800, 483), Image.LANCZOS).save(f"{out}/card/{style}-card.webp", quality=88)

def fit(im, w, h):
    im = im.crop(im.getbbox())
    s = min(w / im.size[0], h / im.size[1])
    im = im.resize((max(1, round(im.size[0] * s)), max(1, round(im.size[1] * s))), Image.LANCZOS)
    piece = Image.new("RGBA", (w, h), (0, 0, 0, 0))
    piece.paste(im, ((w - im.size[0]) // 2, h - im.size[1]), im)
    return piece

sheet = Image.open(stickers).convert("RGBA")
a = np.asarray(sheet).astype(int)
ink = ~((a[:, :, 0] > 238) & (a[:, :, 1] > 238) & (a[:, :, 2] > 238))
labels, _ = ndimage.label(ink)

# seed points on the 3300x2550 page
seeds = {"whisperer": (2835, 1085), "vulture": (1300, 225), "bug": (1810, 540), "grunt": (2325, 800), "swine": (1285, 1675), "flame": (550, 2225)}

for name, (x, y) in seeds.items():
    m = labels == labels[y, x]
    m = ndimage.binary_fill_holes(m)
    ys, xs = np.where(m)
    box = (xs.min(), ys.min(), xs.max() + 1, ys.max() + 1)
    piece = sheet.crop(box)
    alpha = Image.fromarray((m[box[1]:box[3], box[0]:box[2]] * 255).astype(np.uint8))
    piece.putalpha(alpha)
    w, h = (90, 96) if name != "flame" else (90, 110)
    fit(piece, w, h).save(f"{out}/turnling-{name}.webp", lossless=True)

# heads from the cards' title bars, on a round token for the Caravans
def head(f):
    im = Image.open(f"{cards}/{f}.png").convert("RGBA").crop((20, 20, 140, 140))
    bg = im.getpixel((5, 5))
    d = np.asarray(im).astype(int)
    diff = np.abs(d[:, :, :3] - np.array(bg[:3])).sum(axis=2) > 60
    lab, _ = ndimage.label(diff)
    sizes = ndimage.sum(diff, lab, range(1, lab.max() + 1))
    m = ndimage.binary_fill_holes(lab == (np.argmax(sizes) + 1))
    im.putalpha(Image.fromarray((m * 255).astype(np.uint8)))
    return im

fit(head("Cardinals"), 80, 86).save(f"{out}/turnling-cardinal.webp", lossless=True)
fit(head("Union"), 80, 86).save(f"{out}/turnling-doomsayer.webp", lossless=True)

disc = Image.new("RGBA", (90, 90), (0, 0, 0, 0))
ImageDraw.Draw(disc).ellipse((2, 2, 87, 87), fill=(214, 170, 110, 255), outline=(90, 55, 25, 255), width=4)
h = fit(head("CardinalsD"), 60, 60)
disc.paste(h, (15, 12), h)
disc.save(f"{out}/turnling-caravan.webp", lossless=True)
