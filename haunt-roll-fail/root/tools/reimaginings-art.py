# Art for the Doomed Expansion's Re-Imaginings (Eyrie's End, Dawn of the Marquistadors, Doomed Vagabond, Naifs of the Deep Woods),
# from their print-and-play PDFs on the expansion's Google Drive ("4+67 re-imaginings"):
#   pdftoppm -r 300 -png <stickers EE> stkee; pdftoppm -r 300 -png <stickers VB/Naifs> stkvb; pdftoppm -r 300 -png <cards back> vbback
#   pdftoppm -r 300 -png <cards front> cardsfront; pdftoppm -r 200 -png <X faction board front> <x>front (eefront, dmfront, dvfront, knfront)
#   python3 reimaginings-art.py <dir with those renders> <webp2/root/images>
# - the boards, at the size of the other fan boards (1232 wide), and the titles cut from their banners
# - Eyrie's End: the Cardinal meeple, the caravans (face up and down) and the glyph from the sticker sheet, the leader cards
#   (Patron, Hedonistic, Megalomaniac) from the cards sheet; the hawks and roosts are the Eyrie's (aliases in meta.scala)
# - Marquistadors: the glyph and ship from the sticker sheet, the outpost from the board's track, the warriors the Marquise's cat
#   in a darker red, the wood the Marquise's
# - Naifs: the glyph and acclaim from the sticker sheet, the warriors the Woodland Alliance's critter (they are Alliance warriors)
# - Doomed Vagabond: the Collect Bounty quest card from the cards sheet
import sys
import numpy as np
from PIL import Image, ImageDraw, ImageFont

src, out = sys.argv[1], sys.argv[2]
fac = out + "/faction"

def save(im, path):
    im.save(path, lossless=True) if im.mode == "RGBA" else im.save(path, quality=88)

def new_roost_3(im):
    # Eyrie's End's Birdsong step 3 says "up to 6 warriors"; the rules (EE.4.3) say 3, and Gman picked 3 (2026-11-15):
    # "up to 6" is painted over with a "3" (Liberation Serif, thickened to the board's weight) and "warriors" moved up
    bg = (248, 228, 179)
    y0, y1 = 1013, 1042
    war = im.crop((786, y0, 882, y1))
    ImageDraw.Draw(im).rectangle((694, y0, 890, y1 - 1), fill = bg)
    k = 8
    f = ImageFont.truetype("/usr/share/fonts/truetype/liberation/LiberationSerif-Regular.ttf", 26 * k)
    m = Image.new("L", (40 * k, 40 * k), 0)
    ImageDraw.Draw(m).text((2 * k, 0), "3", font = f, fill = 255, stroke_width = 3, stroke_fill = 255)
    m = m.crop(m.getbbox())
    m = m.resize((round(m.size[0] * 18 / m.size[1]), 18), Image.LANCZOS)
    im.paste(Image.new("RGB", m.size, (0, 0, 0)), (698, 1017), m)
    im.paste(war, (698 + m.size[0] + 9, y0))
    return im

def board(x, name, folder, fix = None):
    im = Image.open(f"{src}/{x}front-1.png").convert("RGB")
    if fix:
        im = fix(im)
    save(im.resize((1232, round(im.size[1] * 1232 / im.size[0])), Image.LANCZOS), f"{fac}/{folder}/{name}")

# boards are rendered at 200 dpi; boxes are given at 150 dpi (the scale of the earlier renders)
def b(box):
    return tuple(round(v * 200 / 150) for v in box)

# stickers and cards are rendered at 300 dpi; boxes at 150 dpi
def s(box):
    return tuple(v * 2 for v in box)

def title(x, box, name, folder):
    im = Image.open(f"{src}/{x}front-1.png").convert("RGB").crop(b(box))
    w = 920
    h = round(w * im.size[1] / im.size[0])
    im = im.resize((w, h), Image.LANCZOS)
    if h < 200:
        # padded to the usual 920x200 with the banner's own edge colour
        c = Image.new("RGB", (920, 200), im.getpixel((2, 2)))
        c.paste(im, (0, (200 - h) // 2))
        im = c
    save(im, f"{fac}/{folder}/{name}")

def crop(path, box, w, h, name):
    im = Image.open(path).convert("RGB").crop(box)
    sc = max(w / im.size[0], h / im.size[1])
    im = im.resize((round(im.size[0] * sc), round(im.size[1] * sc)), Image.LANCZOS)
    l, t = (im.size[0] - w) // 2, (im.size[1] - h) // 2
    save(im.crop((l, t, l + w, t + h)), name)

def disc(sheet, cx, cy, r, size, name):
    cx, cy, r = cx * 2, cy * 2, r * 2
    im = Image.open(f"{src}/{sheet}").convert("RGBA").crop((cx - r, cy - r, cx + r, cy + r))
    m = Image.new("L", im.size, 0)
    ImageDraw.Draw(m).ellipse((0, 0, 2 * r - 1, 2 * r - 1), fill=255)
    im.putalpha(m)
    im.resize((size, size), Image.LANCZOS).save(name, lossless=True)

def square(sheet, box, size, name):
    im = Image.open(f"{src}/{sheet}").convert("RGBA").crop(s(box))
    m = Image.new("L", im.size, 0)
    ImageDraw.Draw(m).rounded_rectangle((0, 0, im.size[0] - 1, im.size[1] - 1), radius = im.size[0] // 8, fill=255)
    im.putalpha(m)
    im.resize((size, size), Image.LANCZOS).save(name, lossless=True)

def meeple(sheet, box, h, name):
    # a meeple on the white sheet: white becomes transparent
    im = Image.open(f"{src}/{sheet}").convert("RGBA").crop(s(box))
    a = np.asarray(im).astype(int)
    white = (a[:, :, 0] > 235) & (a[:, :, 1] > 235) & (a[:, :, 2] > 235)
    a[:, :, 3] = np.where(white, 0, 255)
    im = Image.fromarray(a.astype(np.uint8), "RGBA")
    im = im.crop(im.getbbox())
    im = im.resize((round(im.size[0] * h / im.size[1]), h), Image.LANCZOS)
    im.save(name, lossless=True)
    return im

def empty(im):
    a = np.asarray(im).astype(int)
    white = (a[:, :, 0] > 225) & (a[:, :, 1] > 225) & (a[:, :, 2] > 225)
    e = a.copy()
    e[:, :, :3] = np.where(white[:, :, None], a[:, :, :3], 34)
    return Image.fromarray(e.astype(np.uint8), "RGBA")

def warrior(im, prefix):
    im.save(f"{prefix}.webp", lossless=True)
    e = empty(im)
    e.save(f"{prefix}-empty.webp", lossless=True)
    for p, suffix in ((im, "-x5"), (e, "-x5-empty")):
        w, h = p.size
        stack = Image.new("RGBA", (w + 4 * (w // 4), h), (0, 0, 0, 0))
        for i in reversed(range(5)):
            stack.paste(p, (i * (w // 4), 0), p)
        stack.save(f"{prefix}{suffix}.webp", lossless=True)

def tint(path, color, k = 0.55):
    im = Image.open(path).convert("RGBA")
    a = np.asarray(im).astype(float)
    c = np.array(color, dtype = float).reshape(1, 1, 3)
    a[:, :, :3] = a[:, :, :3] * (1 - k) + c * k * (a[:, :, :3].mean(axis = 2, keepdims = True) / 160)
    return Image.fromarray(np.clip(a, 0, 255).astype(np.uint8), "RGBA")

def card(sheet, box, name, size, square = None):
    im = Image.open(f"{src}/{sheet}").convert("RGB").crop(box).rotate(-90, expand = True)
    im = im.resize(size, Image.LANCZOS)
    if square:
        c = Image.new("RGBA", (square, square), (0, 0, 0, 0))
        im = im.resize((round(size[0] * square / size[1]), square), Image.LANCZOS)
        c.paste(im, ((square - im.size[0]) // 2, 0))
        c.save(name, lossless=True)
    else:
        save(im, name)

which = sys.argv[3:] or ["ee", "dm", "dv", "nw"]

if "ee" in which:
    d = f"{fac}/aviary"
    board("ee", "ee-board.webp", "aviary", new_roost_3)
    title("ee", (540, 268, 1080, 380), "ee-title.webp", "aviary")
    crop(f"{src}/eefront-1.png", b((1225, 215, 1355, 375)), 160, 194, f"{d}/ee-char.webp")
    square("stkee-1.png", (984, 288, 1091, 395), 90, f"{d}/ee-glyph.webp")
    disc("stkee-1.png", 868, 628, 56, 90, f"{d}/ee-caravandown.webp")
    disc("stkee-1.png", 868, 782, 56, 90, f"{d}/ee-caravan.webp")
    c = meeple("stkee-1.png", (608, 870, 728, 1008), 90, f"{d}/ee-cardinal.webp")
    empty(c).save(f"{d}/ee-cardinal-empty.webp", lossless=True)
    # the leader cards: Patron, Hedonistic, Megalomaniac (the overlay shows ed-<leader>, 640x640)
    for name, y0, y1 in (("patron", 410, 1200), ("hedonistic", 1220, 2050), ("megalomaniac", 2070, 2890)):
        card("cardsfront-1.png", (300, y0, 1420, y1), f"{d}/ed-{name}.webp", (y1 - y0, 1120), square = 640)

if "dm" in which:
    import os
    d = f"{fac}/marquistadors"
    os.makedirs(d, exist_ok = True)
    board("dm", "dm-board.webp", "marquistadors")
    title("dm", (430, 285, 1295, 372), "dm-title.webp", "marquistadors")
    crop(f"{src}/dmfront-1.png", b((1290, 870, 1615, 1265)), 160, 194, f"{d}/dm-char.webp")
    square("stkee-1.png", (984, 430, 1091, 537), 90, f"{d}/dm-glyph.webp")
    disc("stkee-1.png", 683, 628, 56, 90, f"{d}/dm-ship.webp")
    o = Image.open(f"{src}/dmfront-1.png").convert("RGBA").crop(b((1001, 623, 1104, 726)))
    m = Image.new("L", o.size, 0)
    ImageDraw.Draw(m).rounded_rectangle((0, 0, o.size[0] - 1, o.size[1] - 1), radius = o.size[0] // 8, fill=255)
    o.putalpha(m)
    o.resize((100, 100), Image.LANCZOS).save(f"{d}/dm-outpost.webp", lossless=True)
    Image.open(f"{fac}/feline/mc-wood.webp").save(f"{d}/dm-wood.webp", lossless=True)
    warrior(tint(f"{fac}/feline/mc-cat.webp", (150, 40, 20)), f"{d}/dm-marquistador")

if "dv" in which:
    d = f"{fac}/hero"
    board("dv", "dv-board.webp", "hero")
    title("dv", (585, 125, 1175, 205), "dv-title.webp", "hero")
    card("vbback-1.png", (300, 412, 1425, 1237), f"{out}/card/quest/collect-bounty.webp", (256, 354))
    card("vbback-1.png", (300, 412, 1425, 1237), f"{out}/artwork/quest/collect-bounty.webp", (825, 1125))

if "nw" in which:
    d = f"{fac}/abduct"
    board("kn", "nw-board.webp", "abduct")
    title("kn", (555, 95, 1215, 180), "nw-title.webp", "abduct")
    square("stkvb-1.png", (405, 372, 515, 480), 90, f"{d}/nw-glyph.webp")
    disc("stkvb-1.png", 467, 113, 58, 90, f"{d}/nw-acclaim.webp")
    warrior(Image.open(f"{fac}/insurgent/wa-critter.webp").convert("RGBA"), f"{d}/nw-skunk")
