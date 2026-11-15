# Knaves pieces for the Doomed Expansion's six Knave Captains (Law of Doom D6), cut from the
# meeple stickers of its print-and-play files ("Doomed VB, Captain and Naif stickers" PDF,
# rendered at 300 dpi with `pdftoppm -r 300 -png stickers.pdf s`):
#   python3 doomed-captains.py s-1.png <webp2/root/images/faction/abduct>
# The white sticker sheet around each head is made transparent by a flood fill from the box's edges.
import sys
from PIL import Image, ImageDraw

src, out = sys.argv[1], sys.argv[2]
sheet = Image.open(src).convert("RGBA")

# boxes on the 3300x2550 page
boxes = {
    "zealot":     (125, 1310, 335, 1580),
    "bard":       (920, 1310, 1140, 1585),
    "swindler":   (125, 1595, 365, 1870),
    "blacksmith": (400, 1535, 640, 1870),
    "berserker":  (635, 1665, 855, 1870),
    "barkeep":    (890, 1615, 1090, 1870),
}

for name, box in boxes.items():
    im = sheet.crop(box)
    w, h = im.size
    im = im.crop((-4, -4, w + 4, h + 4))
    bg = Image.new("RGBA", im.size, (255, 255, 255, 255))
    bg.paste(im, (0, 0), im)
    im = bg
    w, h = im.size
    mark = (255, 0, 255, 0)
    for x, y in [(0, 0), (w - 1, 0), (0, h - 1), (w - 1, h - 1)]:
        ImageDraw.floodfill(im, (x, y), mark, thresh=12)
    px = im.load()
    for y in range(h):
        for x in range(w):
            if px[x, y] == mark:
                px[x, y] = (0, 0, 0, 0)
    # the Zealot's teeth are white and open onto the sheet: give back what lies between the head's top and its jaw
    if name == "zealot":
        for x in range(w):
            ys = [y for y in range(h) if px[x, y][3] > 0]
            for y in range(ys[0], ys[-1] + 1) if ys else []:
                if px[x, y][3] == 0:
                    px[x, y] = (255, 255, 255, 255)
    im = im.crop(im.getbbox())
    w, h = im.size
    s = min(85 / w, 86 / h)
    im = im.resize((max(1, round(w * s)), max(1, round(h * s))), Image.LANCZOS)
    piece = Image.new("RGBA", (85, 86), (0, 0, 0, 0))
    piece.paste(im, ((85 - im.size[0]) // 2, 86 - im.size[1]), im)
    piece.save(f"{out}/kd-captain-{name}.webp", lossless=True)
