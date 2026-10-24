#!/usr/bin/env python3
# Fan-made clan cards drawn on the site's card pictures (card/clan/*.webp): the old text (and title) is painted out
# (OpenCV inpainting of the marks only, so the art behind shows through) and the new text set in Quicksand, white, with
# the card's icons inline. So far Robotos's "Lower skill floor Dragon variant" (DragonVariant in nort/options.scala).
#
#   pip install opencv-python-headless pillow numpy
#   card-text.py <Quicksand variable font .ttf> [output directory]
#
# Quicksand is Google Fonts' ofl/quicksand/Quicksand[wght].ttf.

import sys, os
import numpy as np
import cv2
from PIL import Image, ImageDraw, ImageFont, ImageFilter

HERE = os.path.dirname(os.path.abspath(__file__))
IMAGES = os.path.join(HERE, '..', '..', 'webp2', 'nort', 'images')
CARDS = os.path.join(IMAGES, 'card', 'clan')
FONT = sys.argv[1]
OUT = sys.argv[2] if len(sys.argv) > 2 else CARDS

def font(size, weight = 500):
    f = ImageFont.truetype(FONT, size)
    f.set_variation_by_axes([weight])
    return f

def unbacked(path):
    """A resource icon without the gray disk of the token it is on"""
    im = Image.open(path).convert('RGBA')
    a = np.array(im).astype(float)
    h, w = a.shape[:2]
    yy, xx = np.mgrid[:h, :w]
    r = np.hypot(yy - h / 2, xx - w / 2)
    ring = (r > w * 0.40) & (r < w * 0.45) & (a[..., 3] > 200)
    disk = np.median(a[ring][:, :3], 0)
    near = np.sqrt(((a[..., :3] - disk) ** 2).sum(-1)) < 28
    a[..., 3] = np.where(near | (r > w * 0.46), 0, a[..., 3])
    out = Image.fromarray(a.astype(np.uint8))
    return out.crop(out.getchannel('A').point(lambda v: 255 if v > 128 else 0).getbbox())

ICONS = {
    'food': unbacked(os.path.join(IMAGES, 'ui', 'food.webp')),
    'fame': Image.open(os.path.join(IMAGES, 'ui', 'fame.webp')).convert('RGBA'),
}
for k in ICONS:
    ICONS[k] = ICONS[k].crop(ICONS[k].getchannel('A').point(lambda v: 255 if v > 128 else 0).getbbox())

# A line is a list of pieces: text, or {icon} for an icon
def pieces(line):
    out = []
    while line:
        i = line.find('{')
        if i < 0:
            out.append(line)
            break
        if i > 0:
            out.append(line[:i])
        j = line.index('}', i)
        out.append(ICONS[line[i + 1:j]])
        line = line[j + 1:]
    return out

def erase(im, box):
    """Paints out the text and icons in box: the small marks that stand out from the blurred background around them"""
    x0, y0, x1, y1 = box
    a = np.array(im.convert('RGB'))
    region = a[y0:y1, x0:x1]
    blur = cv2.medianBlur(region, 21)
    diff = np.abs(region.astype(int) - blur.astype(int)).max(-1)
    gray = cv2.cvtColor(region, cv2.COLOR_RGB2GRAY)
    hat = cv2.morphologyEx(gray, cv2.MORPH_TOPHAT, cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (7, 7)))
    # and the colored icons (the food apple), not the blue background
    hsv = cv2.cvtColor(region, cv2.COLOR_RGB2HSV)
    sat = (hsv[..., 1] > 90) & (hsv[..., 2] > 50) & ((hsv[..., 0] < 85) | (hsv[..., 0] > 135))
    m = np.zeros(gray.shape, np.uint8)
    for marks in [(diff > 40), (hat > 45) & (gray > 140), sat]:
        n, labels, stats, _ = cv2.connectedComponentsWithStats(marks.astype(np.uint8))
        small = np.zeros(n, bool)
        for c in range(1, n):
            if stats[c, cv2.CC_STAT_HEIGHT] <= 24:
                small[c] = True
        m |= small[labels].astype(np.uint8) * 255
    m = cv2.dilate(m, np.ones((3, 3), np.uint8), iterations = 2)
    mask = np.zeros(a.shape[:2], np.uint8)
    mask[y0:y1, x0:x1] = m
    bgr = cv2.inpaint(cv2.cvtColor(a, cv2.COLOR_RGB2BGR), mask, 5, cv2.INPAINT_TELEA)
    return Image.fromarray(cv2.cvtColor(bgr, cv2.COLOR_BGR2RGB)).convert('RGBA')

def draw(im, cx, y, maxw, lines, size, step, weight = 500, spacing = 0, color = (255, 255, 255, 255), shadow = True):
    d = ImageDraw.Draw(im)
    while True:
        f = font(size, weight)
        ih = int(size * 1.2)
        def width(p):
            if isinstance(p, str):
                return d.textlength(p, font = f) + spacing * len(p)
            return p.width * ih / p.height + 3
        if max(sum(width(p) for p in pieces(line)) for line in lines) <= maxw:
            break
        size -= 0.5
    layer = Image.new('RGBA', im.size, (0, 0, 0, 0))
    d = ImageDraw.Draw(layer)
    for line in lines:
        ps = pieces(line)
        w = sum(width(p) for p in ps)
        x = cx - w / 2
        for p in ps:
            if isinstance(p, str):
                if spacing:
                    for ch in p:
                        d.text((x, y + step / 2), ch, font = f, fill = color, anchor = 'lm')
                        x += d.textlength(ch, font = f) + spacing
                    continue
                d.text((x, y + step / 2), p, font = f, fill = color, anchor = 'lm')
            else:
                icon = p.resize((max(1, round(p.width * ih / p.height)), ih), Image.LANCZOS)
                layer.alpha_composite(icon, (round(x + 1.5), round(y + step / 2 - ih / 2)))
            x += width(p)
        y += step
    if shadow:
        sh = Image.new('RGBA', im.size, (0, 0, 0, 0))
        sh.putalpha(layer.getchannel('A').filter(ImageFilter.GaussianBlur(size / 5)).point(lambda v: v * 0.9))
        im = Image.alpha_composite(im, sh)
    return Image.alpha_composite(im, layer)

# Each card: its source picture, the boxes whose text is painted out, then each new text: centre x, first line's top,
# widest line (the font shrinks to fit), lines, font size, line step and the draw options, in the picture's pixels
CARDS_OUT = {
    'dragon-variant': ('dragon', [(34, 512, 378, 600)], [
        (206, 516, 330, [
            "Recruit 1. If the Sacrificial Pyre",
            "is full, you may empty it",
            "to collect 1 {food}.",
        ], 16, 26, {}),
    ]),
    'dragon-reluctant-workforce-variant': ('dragon-reluctant-workforce', [(34, 518, 378, 600)], [
        (206, 520, 330, [
            "Draw 2 cards. If the Sacrificial Pyre",
            "is full, you may empty it",
            "to draw a 3rd card.",
        ], 16, 24, {}),
    ]),
    'dragon-enthrall': ('dragon-enslavement', [(80, 124, 340, 146), (34, 442, 378, 590)], [
        (206, 124, 250, ["ENTHRALL"], 18, 21, dict(weight = 700, spacing = 2.5, color = (205, 220, 222, 255), shadow = False)),
        (206, 446, 330, [
            "If the Sacrificial Pyre is full,",
            "empty it to gain 2 {fame}.",
            "Then put on the Sacrificial Pyre",
            "1 enemy unit from a territory",
            "adjacent to one of yours.",
        ], 16, 27, {}),
    ]),
}

if __name__ == '__main__':
    for name, (src, boxes, texts) in CARDS_OUT.items():
        im = Image.open(os.path.join(CARDS, src + '.webp')).convert('RGBA')
        for box in boxes:
            im = erase(im, box)
        for cx, y, maxw, lines, size, step, opts in texts:
            im = draw(im, cx, y, maxw, lines, size, step, **opts)
        im.convert('RGB').save(os.path.join(OUT, name + '.webp'), quality = 90, method = 6)
