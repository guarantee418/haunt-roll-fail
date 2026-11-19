# Art for United Dove Corps ii (MarcustheCat, from L'Arbre's, art by SuPaCatOnE) and Arachnid Association ii (chemicalreaction,
# art by SuPaCatOnE), from the Root Ultimate Collection TTS mod (Steam workshop 2516434159). The save is BSON, and each faction is the
# JSON objects in EVERYTHING['Fan Factions']['United Dove Corps II'] (or 'Arachnid Association II'); the images are their CustomImage
# ImageURL / ImageSecondaryURL, downloaded as:
#   dove:     front.img, back.img (the board), rel.img (Relationship), clinic.img, school.img, tokA.img (ceasefire), tokB.img (peace)
#   arachnid: front.img, back.img (the board), rel.img (Relationship), web.img, egg.img, hatched.img, mutation.img (a full web is
#             drawn from the web, with a cocoon)
#   python3 dove-arachnid-art.py dove <dir> <webp2/root/images/faction/dove>
#   python3 dove-arachnid-art.py arachnid <dir> <webp2/root/images/faction/arachnid>
# - the board front at the size of the other fan boards (1232 wide), the title cut from its banner
# - the warriors are cut from the board back's component manifest (its light background taken away), with an outline added,
#   and the empty and five-stack versions drawn from them
# - the character is cut from the board back's art, the glyph and the action icon are the Relationship token, cut round
import os
import sys
import numpy as np
from PIL import Image, ImageDraw
from scipy import ndimage

kind, src, out = sys.argv[1], sys.argv[2], sys.argv[3]
os.makedirs(out, exist_ok=True)

def fit(im, height):
    return im.resize((round(im.size[0] * height / im.size[1]), height), Image.LANCZOS)

def trim(im):
    return im.crop(im.getchannel("A").point(lambda v: 255 if v > 20 else 0).getbbox())

# the piece in a crop of a light, patterned background: everything joined to the crop's edge that is light and bluish (or tinted) goes
def cutout(im, light, open_iter=2):
    a = np.asarray(im.convert("RGB")).astype(int)
    bg = light(a)
    lab, n = ndimage.label(bg)
    border = set(lab[0, :]) | set(lab[-1, :]) | set(lab[:, 0]) | set(lab[:, -1])
    border.discard(0)
    fg = ~np.isin(lab, list(border))
    fg = ndimage.binary_opening(fg, iterations=open_iter)
    lab2, n2 = ndimage.label(fg)
    keep = lab2 == (np.bincount(lab2.ravel())[1:].argmax() + 1)
    keep = ndimage.binary_fill_holes(keep)
    return trim(Image.fromarray(np.dstack([a, keep * 255]).astype(np.uint8), "RGBA"))

def outline(im, color, pad=3):
    al = np.asarray(im.getchannel("A")) > 20
    big = Image.new("RGBA", (im.size[0] + 2 * pad, im.size[1] + 2 * pad), (0, 0, 0, 0))
    mask = Image.fromarray((al * 255).astype(np.uint8))
    for dx in range(-pad, pad + 1):
        for dy in range(-pad, pad + 1):
            if dx * dx + dy * dy <= pad * pad:
                big.paste(color, (pad + dx, pad + dy), mask)
    big.alpha_composite(im, (pad, pad))
    return big

def warrior(im, name):
    im.save(f"{out}/{name}.webp", lossless=True)
    e = np.asarray(im).astype(int).copy()
    e[:, :, :3] = 34
    empty = Image.fromarray(e.astype(np.uint8), "RGBA")
    empty.save(f"{out}/{name}-empty.webp", lossless=True)
    for p, suffix in ((im, "-x5"), (empty, "-x5-empty")):
        w, h = p.size
        s = Image.new("RGBA", (w + 4 * (w // 4), h), (0, 0, 0, 0))
        for i in reversed(range(5)):
            s.paste(p, (i * (w // 4), 0), p)
        s.save(f"{out}/{name}{suffix}.webp", lossless=True)

def round_token(im, size, inset=0.02):
    im = im.convert("RGBA")
    w, h = im.size
    m = Image.new("L", im.size, 0)
    ImageDraw.Draw(m).ellipse((w * inset, h * inset, w * (1 - inset) - 1, h * (1 - inset) - 1), fill=255)
    im.putalpha(m)
    return im.resize((size, size), Image.LANCZOS)

def glyph(prefix):
    rel = Image.open(f"{src}/rel.img").convert("RGBA")
    w, h = rel.size
    r = rel.crop((w * 0.06, h * 0.06, w * 0.94, h * 0.94))
    mk = Image.new("L", r.size, 0)
    ImageDraw.Draw(mk).rounded_rectangle((0, 0, r.size[0] - 1, r.size[1] - 1), radius=r.size[0] // 8, fill=255)
    r.putalpha(mk)
    r.resize((86, 86), Image.LANCZOS).save(f"{out}/{prefix}-glyph.webp", lossless=True)
    r.resize((100, 100), Image.LANCZOS).save(f"{out}/{prefix}-action.webp", lossless=True)

def board(prefix, title):
    b = Image.open(f"{src}/front.img").convert("RGB")
    b.resize((1232, round(b.size[1] * 1232 / b.size[0])), Image.LANCZOS).save(f"{out}/{prefix}-board.webp", quality=88)
    s = b.size[0] / 1600
    t = b.crop(tuple(round(v * s) for v in title))
    t.resize((920, round(920 * t.size[1] / t.size[0])), Image.LANCZOS).save(f"{out}/{prefix}-title.webp", quality=88)

def character(prefix, box, light):
    b = Image.open(f"{src}/back.img").convert("RGB")
    s = b.size[0] / 1600
    char = cutout(b.crop(tuple(round(v * s) for v in box)), light, 3)
    c = fit(char, 194)
    if c.size[0] > 160:
        c = c.resize((160, round(c.size[1] * 160 / c.size[0])), Image.LANCZOS)
    canvas = Image.new("RGBA", (160, 194), (0, 0, 0, 0))
    canvas.alpha_composite(c, ((160 - c.size[0]) // 2, 194 - c.size[1]))
    canvas.save(f"{out}/{prefix}-char.webp", lossless=True)

def manifest(box, light):
    b = Image.open(f"{src}/back.img").convert("RGB")
    s = b.size[0] / 1600
    return cutout(b.crop(tuple(round(v * s) for v in box)), light)

if kind == "dove":
    # the board back's background is a pale blue with lighter doves on it
    light = lambda a: (a[..., 0] < 250) & (a[..., 0] > 212) & (a[..., 2] - a[..., 0] > 4)

    board("udc", (500, 60, 1200, 150))
    character("udc", (1140, 700, 1530, 1200), light)
    glyph("udc")

    dove = manifest((150, 175, 270, 335), light)
    warrior(outline(fit(dove, 88), (18, 56, 72, 255)), "udc-dove")

    for name, f in (("clinic", "clinic"), ("school", "school")):
        Image.open(f"{src}/{f}.img").convert("RGB").resize((100, 100), Image.LANCZOS).save(f"{out}/udc-{name}.webp", quality=90)

    for name, f in (("ceasefire", "tokA"), ("peace", "tokB")):
        round_token(Image.open(f"{src}/{f}.img"), 80).save(f"{out}/udc-{name}.webp", lossless=True)

if kind == "arachnid":
    # the board back's background is a pale greyish blue, greener than the purple spiders (and their white webs)
    light = lambda a: (a[..., 1] >= a[..., 0] - 3) & (a[..., 2] > 185) & (a[..., 0] > 140)

    board("aa", (470, 60, 1130, 150))
    character("aa", (1080, 740, 1590, 1240), light)
    glyph("aa")

    adult = manifest((158, 158, 262, 295), light)
    warrior(outline(fit(adult, 96), (42, 29, 64, 255)), "aa-adult")
    spiderling = manifest((188, 325, 250, 398), light)
    warrior(outline(fit(spiderling, 66), (42, 29, 64, 255)), "aa-spiderling")

    for name in ("web", "egg", "hatched", "mutation"):
        round_token(Image.open(f"{src}/{name}.img"), 80).save(f"{out}/aa-{name}.webp", lossless=True)

    # a full web: the web with a silk cocoon wrapped over its middle
    full = round_token(Image.open(f"{src}/web.img"), 160)
    d = ImageDraw.Draw(full)
    d.ellipse((52, 34, 108, 130), fill=(236, 232, 240, 255), outline=(90, 80, 110, 255), width=3)
    for y in range(48, 124, 12):
        d.arc((50, y - 10, 110, y + 10), 10, 170, fill=(150, 140, 170, 255), width=2)
    full.resize((80, 80), Image.LANCZOS).save(f"{out}/aa-web-full.webp", lossless=True)
