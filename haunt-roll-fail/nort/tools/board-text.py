#!/usr/bin/env python3
# The printed New Blood clan board texts (Gman's photos of the print, 2026-10-23) drawn on the site's board pictures
# (expansion/board/<clan>.webp, from the TTS mod) in place of their old text, keeping the pictures as they are:
# the old body text and its icons are painted out (OpenCV inpainting) and the new text is set in Quicksand, white,
# with the board's icons inline. Writes expansion/board/<clan>-print.webp (Warchief.boardFile picks them in games
# with the PrintedNewBlood option). 'dragon-variant' is Robotos's fan-made Dragon power (DragonVariant in nort/options.scala),
# written to expansion/board/dragon-variant.webp; 'bear-border' is his border Bear's power (BearBorder), written to
# expansion/board/bear-border.webp.
#
#   pip install opencv-python-headless pillow numpy
#   board-text.py <Quicksand variable font .ttf> [board ...]
#
# Quicksand is Google Fonts' ofl/quicksand/Quicksand[wght].ttf. The Lynx card icon (lynx-card-icon.png, beside this
# script) is cut from the photo of the printed Lynx board; it has no file of its own on the site.

import sys, os
import numpy as np
import cv2
from PIL import Image, ImageDraw, ImageFont, ImageFilter

HERE = os.path.dirname(os.path.abspath(__file__))
IMAGES = os.path.join(HERE, '..', '..', 'webp2', 'nort', 'images')
FONT = sys.argv[1]
LYNX = os.path.join(HERE, 'lynx-card-icon.png')

def font(size, weight = 600):
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

def lynx_icon():
    im = Image.open(LYNX).convert('RGBA')
    a = np.array(im).astype(float)
    bg = np.median(np.concatenate([a[0, :, :3], a[-1, :, :3], a[:, 0, :3], a[:, -1, :3]]), 0)
    d = np.sqrt(((a[..., :3] - bg) ** 2).sum(-1))
    m = (d > 45).astype(np.uint8) * 255
    m = cv2.morphologyEx(m, cv2.MORPH_CLOSE, np.ones((5, 5), np.uint8))
    n, labels, stats, _ = cv2.connectedComponentsWithStats(m)
    big = 1 + np.argmax(stats[1:, cv2.CC_STAT_AREA])
    m = np.where(labels == big, 255, 0).astype(np.uint8)
    h, w = m.shape
    fill = m.copy()
    cv2.floodFill(fill, np.zeros((h + 2, w + 2), np.uint8), (0, 0), 255)
    m = m | cv2.bitwise_not(fill)
    a[..., 3] = cv2.GaussianBlur(m, (3, 3), 0)
    out = Image.fromarray(a.astype(np.uint8))
    return out.crop(out.getbbox())

ICONS = {
    'axe': Image.open(os.path.join(IMAGES, 'ui', 'axe.webp')).convert('RGBA'),
    'skull': Image.open(os.path.join(IMAGES, 'ui', 'skull.webp')).convert('RGBA'),
    'food': unbacked(os.path.join(IMAGES, 'ui', 'food.webp')),
    'wood': unbacked(os.path.join(IMAGES, 'ui', 'wood.webp')),
    'tide': Image.open(os.path.join(IMAGES, 'token', 'high-tide.webp')).convert('RGBA'),
    # the face of an Ancestral Equipment token, without its number
    'gear': Image.open(os.path.join(IMAGES, 'token', 'ox-back-1.webp')).convert('RGBA').crop((24, 0, 270, 220)),
    'lynx': lynx_icon(),
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

def fill(im, box):
    a = np.array(im.convert('RGB'))
    mask = np.zeros(a.shape[:2], np.uint8)
    x0, y0, x1, y1 = box
    mask[y0:y1, x0:x1] = 255
    bgr = cv2.inpaint(cv2.cvtColor(a, cv2.COLOR_RGB2BGR), mask, 5, cv2.INPAINT_TELEA)
    return Image.fromarray(cv2.cvtColor(bgr, cv2.COLOR_BGR2RGB)).convert('RGBA')

def erase(im, box, scale):
    """Paints out the text and icons in box: the small marks that stand out from the blurred background around them
    (no taller than a line of text: the emblem's ring and the portrait are left alone)"""
    x0, y0, x1, y1 = box
    a = np.array(im.convert('RGB'))
    region = a[y0:y1, x0:x1]
    blur = cv2.medianBlur(region, 31)
    diff = np.abs(region.astype(int) - blur.astype(int)).max(-1)
    # and the thin bright strokes of the letters where they cross the portrait (a white top-hat)
    gray = cv2.cvtColor(region, cv2.COLOR_RGB2GRAY)
    k = max(5, round(7 * scale)) | 1
    hat = cv2.morphologyEx(gray, cv2.MORPH_TOPHAT, cv2.getStructuringElement(cv2.MORPH_ELLIPSE, (k, k)))
    m = np.zeros(gray.shape, np.uint8)
    for marks in [(diff > 40), (hat > 45) & (gray > 140)]:
        n, labels, stats, _ = cv2.connectedComponentsWithStats(marks.astype(np.uint8))
        small = np.zeros(n, bool)
        for c in range(1, n):
            if stats[c, cv2.CC_STAT_HEIGHT] <= 26 * scale:
                small[c] = True
        m |= small[labels].astype(np.uint8) * 255
    m = cv2.dilate(m, np.ones((3, 3), np.uint8), iterations = 2)
    mask = np.zeros(a.shape[:2], np.uint8)
    mask[y0:y1, x0:x1] = m
    bgr = cv2.cvtColor(a, cv2.COLOR_RGB2BGR)
    bgr = cv2.inpaint(bgr, mask, 5, cv2.INPAINT_TELEA)
    return Image.fromarray(cv2.cvtColor(bgr, cv2.COLOR_BGR2RGB)).convert('RGBA')

def draw(im, cx, y, maxw, lines, size, step):
    d = ImageDraw.Draw(im)
    while True:
        f = font(size)
        ih = int(size * 1.3)
        def width(p):
            if isinstance(p, str):
                return d.textlength(p, font = f)
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
                d.text((x, y + step / 2), p, font = f, fill = (255, 255, 255, 255), anchor = 'lm')
            else:
                icon = p.resize((max(1, round(p.width * ih / p.height)), ih), Image.LANCZOS)
                layer.alpha_composite(icon, (round(x + 1.5), round(y + step / 2 - ih / 2)))
            x += width(p)
        y += step
    shadow = Image.new('RGBA', im.size, (0, 0, 0, 0))
    shadow.putalpha(layer.getchannel('A').filter(ImageFilter.GaussianBlur(size / 5)).point(lambda v: v * 0.9))
    im = Image.alpha_composite(im, shadow)
    return Image.alpha_composite(im, layer)

# Each board: the boxes its old body text was in (painted out), then each new text: centre x, first line's top, widest
# line (the font shrinks to fit), lines, font size and line step, in the board picture's pixels
BOARDS = {
    'lynx': ([(110, 64, 592, 170), (110, 170, 522, 222), (215, 468, 492, 570)], [
        (318, 64, 400, [
            "Lynx has two special units,",
            "Brundr and Kaelinn, each worth 1 {axe}.",
            "Before or after playing a Clan card",
            "with {lynx}, you may make a bonus",
            "Move of 1, involving only Brundr",
            "and/or Kaelinn. Only 1 combat",
            "may be triggered by this Move.",
        ], 16, 22),
        (352, 470, 280, [
            "If Mielikki is the attacker,",
            "she may Move either Brundr",
            "or Kaelinn from any territory",
            "to her territory.",
        ], 16, 24),
    ]),
    'kraken': ([(110, 62, 464, 238), (160, 486, 522, 572)], [
        (318, 62, 360, [
            "Enemies fighting Kraken in a territory",
            "containing a {tide} suffer 1 {skull}.",
            "Remove the {tide} from the territory",
            "if the combat is lost. Before or after",
            "playing a Kraken card, you can place",
            "or relocate a {tide}",
            "on one of your territories.",
        ], 16, 23),
        (340, 484, 350, [
            "If Kàra is the defender, before the",
            "combat starts, she may immediately",
            "place or relocate a {tide} on her territory.",
        ], 16, 27),
    ]),
    'dragon': ([(120, 58, 526, 228), (240, 226, 526, 248)], [
        (335, 60, 370, [
            "After each combat, place enemy",
            "casualties {skull} on the Sacrificial Pyre.",
            "To enable Harvest: if the Pyre is empty,",
            "move 1 of your deployed units to it",
            "OR sacrifice 1 unit from the Pyre",
            "and collect +1 {food} or +1 {wood}",
            "from one of your territories.",
        ], 16, 24),
    ]),
    'dragon-variant': ([(120, 58, 526, 228), (240, 226, 526, 248)], [
        (335, 60, 370, [
            "The Sacrificial Pyre starts with",
            "1 of your units. After each combat,",
            "place enemy casualties {skull} on it.",
            "At the start of each Harvest,",
            "you may move 1 unit from your",
            "reserve to an empty Pyre spot.",
        ], 16, 27),
    ]),
    'bear-border': ([(130, 56, 440, 190)], [
        (284, 58, 290, [
            "Kaija is a token on a border.",
            "Before resolving a clan card, you may",
            "place it on a regular border of one",
            "of your territories. All clans treat",
            "that border as Rough, unless they",
            "discard 1 resource to cross it.",
        ], 16, 22),
    ]),
    'squirrel': ([(150, 48, 416, 202)], [
        (285, 46, 290, [
            "After paying winter costs:",
            "If you have at least 1 {food}",
            "in your reserve, collect 1 {food}.",
            "If you have at least 1 {wood}",
            "in your reserve, collect 1 {wood}.",
            "Choose only one of",
            "the two possible rewards.",
        ], 16, 22),
    ]),
    'ox-2': ([(125, 62, 502, 228), (440, 58, 482, 94, 'fill')], [
        (340, 60, 360, [
            "When you Explore, place 1 {gear} on",
            "one of the small building spaces of",
            "the revealed tile. Before playing an",
            "Ox card, you may take 1 {gear} from",
            "one of your territories. During a",
            "combat, you can use 1 {gear} from",
            "your reserve, flipping it face down.",
        ], 16, 22),
    ]),
    'horse': ([(170, 80, 815, 306), (378, 832, 892, 1016), (350, 960, 395, 1000, 'fill')], [
        (476, 86, 640, [
            "When you close one of your own territories:",
            "Collect 1 {wood}",
            "OR",
            "Build 1 small building in it.",
            "(All Build rules must",
            "be respected.)",
        ], 29, 37),
        (650, 836, 570, [
            "The Horse clan has 2 warchiefs,",
            "each worth 2 {axe}. If Eitria and Brok",
            "are in the same territory, together they",
            "are only worth 3 {axe} in combat (and not 4 {axe}).",
        ], 29, 43),
    ]),
    'rat': ([(228, 84, 672, 306), (378, 852, 906, 998)], [
        (448, 88, 450, [
            "When you close one or more of your",
            "own territories, you may place",
            "1 unit from your reserve",
            "in each of these territories.",
        ], 29, 47),
        (650, 860, 560, [
            "Eir gains 1 {axe} when fighting",
            "in a territory with {food} in it that",
            "does not come from a building.",
        ], 29, 44),
    ]),
}

def run(name, out):
    src = os.path.join(IMAGES, 'expansion', 'board', name.replace('-variant', '').replace('-border', '') + '.webp')
    im = Image.open(src).convert('RGBA')
    boxes, texts = BOARDS[name]
    scale = im.width / 616
    for box in boxes:
        im = erase(im, box[:4], scale) if len(box) == 4 else fill(im, box[:4])
    for cx, y, maxw, lines, size, step in texts:
        im = draw(im, cx, y, maxw, lines, size, step)
    im.convert('RGB').save(os.path.join(IMAGES, 'expansion', 'board', out + '.webp'), quality = 90, method = 6)

if __name__ == '__main__':
    only = sys.argv[2:]
    for name in BOARDS:
        if only and name not in only:
            continue
        run(name, name if name.endswith('-variant') or name.endswith('-border') else name.replace('-2', '') + '-print')
