# Art for the Noxious Battery (S.P. Shaman's Nemesis Expansion, art by disasterman), from its print-and-play folder's ASSETS:
#   "The Mephitis Battery.png" as board.png, "Mephitis_Battery_Layer.png" as layer.png, "Meeple.png" as meeple.png,
#   cannonball.png, scent_marks.png as scent.png, "Relationship.png" as rel.png
#   python3 noxious-art.py <dir with those files> <webp2/root/images/faction/battery>
# - the board front at the size of the other fan boards (1232 wide), the title cut from its banner
# - the warrior is the meeple, with the empty and five-stack versions drawn from it
# - the cannon and the character are cut from the board's art layer; the damaged cannon is the cannon darkened and rusted
# - the glyph is the skunk's head, the tokens are the round token files
import os
import sys
import numpy as np
from PIL import Image, ImageDraw, ImageEnhance

src, out = sys.argv[1], sys.argv[2]
os.makedirs(out, exist_ok=True)

def fit(im, height):
    return im.resize((round(im.size[0] * height / im.size[1]), height), Image.LANCZOS)

def trim(im):
    return im.crop(im.getchannel("A").point(lambda v: 255 if v > 20 else 0).getbbox())

board = Image.open(f"{src}/board.png").convert("RGB")
board.resize((1232, round(board.size[1] * 1232 / board.size[0])), Image.LANCZOS).save(f"{out}/nx-board.webp", quality=88)

t = board.crop((800, 118, 1560, 228))
t.resize((920, round(920 * t.size[1] / t.size[0])), Image.LANCZOS).save(f"{out}/nx-title.webp", quality=88)

layer = Image.open(f"{src}/layer.png").convert("RGBA")

def largest(im):
    from scipy import ndimage
    a = np.asarray(im).copy()
    lab, n = ndimage.label(ndimage.binary_dilation(a[:, :, 3] > 20, iterations=6))
    keep = lab == (np.bincount(lab.ravel())[1:].argmax() + 1)
    a[:, :, 3] = np.where(keep, a[:, :, 3], 0)
    return trim(Image.fromarray(a, "RGBA"))

cannon = largest(layer.crop((242, 304, 1391, 1091)))
fit(cannon, 92).save(f"{out}/nx-cannon.webp", lossless=True)

# damaged: darker, redder, with a crack drawn across the barrel
d = ImageEnhance.Brightness(cannon).enhance(0.62)
a = np.asarray(d).astype(int)
a[:, :, 0] = np.minimum(255, a[:, :, 0] + 40)
a[:, :, 2] = (a[:, :, 2] * 0.7).astype(int)
d = Image.fromarray(a.astype(np.uint8), "RGBA")
w, h = d.size
crack = Image.new("RGBA", d.size, (0, 0, 0, 0))
ImageDraw.Draw(crack).line([(w * 0.18, h * 0.18), (w * 0.27, h * 0.27), (w * 0.24, h * 0.33), (w * 0.36, h * 0.43)], fill=(20, 10, 10, 255), width=max(6, w // 60))
d = Image.composite(crack, d, Image.fromarray(np.minimum(np.asarray(crack.getchannel("A")), np.asarray(d.getchannel("A"))).astype(np.uint8)))
fit(d, 92).save(f"{out}/nx-cannon-damaged.webp", lossless=True)

char = trim(layer.crop((1501, 582, 2033, 1085)))
c = fit(char, 194)
if c.size[0] > 160:
    c = c.resize((160, round(c.size[1] * 160 / c.size[0])), Image.LANCZOS)
canvas = Image.new("RGBA", (160, 194), (0, 0, 0, 0))
canvas.alpha_composite(c, ((160 - c.size[0]) // 2, 194 - c.size[1]))
canvas.save(f"{out}/nx-char.webp", lossless=True)

m = trim(Image.open(f"{src}/meeple.png").convert("RGBA"))
im = fit(m, 82)
# an outline, as the meeple has none
al = np.asarray(im.getchannel("A")) > 20
pad = 3
big = Image.new("RGBA", (im.size[0] + 2 * pad, im.size[1] + 2 * pad), (0, 0, 0, 0))
mask = Image.fromarray((al * 255).astype(np.uint8))
for dx in range(-pad, pad + 1):
    for dy in range(-pad, pad + 1):
        if dx * dx + dy * dy <= pad * pad:
            big.paste((40, 18, 18, 255), (pad + dx, pad + dy), mask)
big.alpha_composite(im, (pad, pad))
im = big
im.save(f"{out}/nx-skunk.webp", lossless=True)
e = np.asarray(im).astype(int).copy()
e[:, :, :3] = 34
empty = Image.fromarray(e.astype(np.uint8), "RGBA")
empty.save(f"{out}/nx-skunk-empty.webp", lossless=True)
for p, suffix in ((im, "-x5"), (empty, "-x5-empty")):
    w, h = p.size
    s = Image.new("RGBA", (w + 4 * (w // 4), h), (0, 0, 0, 0))
    for i in reversed(range(5)):
        s.paste(p, (i * (w // 4), 0), p)
    s.save(f"{out}/nx-skunk{suffix}.webp", lossless=True)

for name, size in (("cannonball", 80), ("scent", 80)):
    Image.open(f"{src}/{name}.png").convert("RGBA").resize((size, size), Image.LANCZOS).save(f"{out}/nx-{name}.webp", lossless=True)

# glyph and action icon: the head on its pink square, cut round
rel = Image.open(f"{src}/rel.png").convert("RGBA")
r = rel.crop((40, 40, 660, 660))
mk = Image.new("L", r.size, 0)
ImageDraw.Draw(mk).rounded_rectangle((0, 0, r.size[0] - 1, r.size[1] - 1), radius=r.size[0] // 8, fill=255)
r.putalpha(mk)
r.resize((86, 86), Image.LANCZOS).save(f"{out}/nx-glyph.webp", lossless=True)
r.resize((100, 100), Image.LANCZOS).save(f"{out}/nx-action.webp", lossless=True)
