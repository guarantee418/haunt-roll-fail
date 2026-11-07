#!/usr/bin/env python3
# Military paths module (fan content): draws the Guardian path's Militia (token/unit/militia-<color>.webp), a peasant levy in
# the flat style of unit-original and the bodyguard (bodyguard-figure.py): no helmet or armor, bare-headed with short hair and
# a beard, a belted tunic over trousers, holding a pitchfork upright (Gman, 2026-11-06). It is drawn in the originals' brown
# and white, as militia-original, then recolored to each player color with the fill and detail colors the unit figures use.
# Run from haunt-roll-fail/: python3 nort/tools/militia-figure.py
from PIL import Image, ImageDraw, ImageFilter

OUT = 'webp2/nort/images/token/unit/'
S = 4               # supersampling
N = 512 * S
FILL = (168, 66, 46)
WHITE = (255, 255, 255)
# Player color: (fill, detail), as in unit-<color>.webp and bodyguard-figure.py
COLORS = {
    'red': ((232, 66, 13), (255, 255, 255)),
    'blue': ((79, 120, 188), (213, 254, 254)),
    'yellow': ((255, 255, 0), (0, 0, 0)),
    'green': ((79, 159, 59), (214, 255, 157)),
    'purple': ((165, 108, 166), (255, 255, 255)),
    'orange': ((255, 138, 20), (255, 255, 255)),
}

def p(*pts):
    return [(x * S, y * S) for x, y in pts]

def shapes(d, colour, detail):
    # Legs in trousers, wider at the foot
    d.polygon(p((206, 316), (240, 316), (238, 424), (198, 436), (192, 424), (206, 412)), fill=colour)
    d.polygon(p((260, 316), (294, 316), (294, 412), (308, 424), (302, 436), (262, 424)), fill=colour)
    # Tunic: shoulders down to a flared hem over the thighs
    d.polygon(p((198, 172), (302, 172), (316, 206), (306, 268), (322, 336), (178, 336), (194, 268), (184, 206)), fill=colour)
    # Arms: the left one hanging at his side, the right one a thick upper arm out from the shoulder and a forearm bent up to
    # the pitchfork's shaft, like the bodyguard's spear arm
    d.polygon(p((198, 176), (180, 186), (166, 262), (172, 290), (192, 290), (194, 262), (204, 206)), fill=colour)
    d.polygon(p((292, 172), (318, 180), (330, 230), (348, 196), (366, 208), (338, 262), (314, 262), (298, 222)), fill=colour)
    # Neck and bare head, with short hair and a beard
    d.rectangle(p((236, 150), (264, 178)), fill=colour)
    d.ellipse(p((214, 90), (286, 164)), fill=colour)
    d.ellipse(p((206, 118), (222, 142)), fill=colour)
    d.ellipse(p((278, 118), (294, 142)), fill=colour)
    d.chord(p((220, 128), (280, 192)), 0, 180, fill=colour)
    # Short hair: a rounded mop over the crown with a fringe over the forehead
    d.pieslice(p((208, 82), (292, 146)), 180, 360, fill=colour)
    d.ellipse(p((206, 94), (230, 124)), fill=colour)
    d.ellipse(p((270, 94), (294, 124)), fill=colour)
    # Pitchfork: shaft, crossbar and three tines
    d.rectangle(p((334, 70), (346, 436)), fill=colour)
    d.rectangle(p((302, 64), (378, 76)), fill=colour)
    for x in (302, 334, 366):
        d.polygon(p((x, 70), (x + 12, 70), (x + 10, 10), (x + 6, 0), (x + 2, 10)), fill=colour)

def details(d, colour, detail):
    w = 5 * S
    # Hairline and eyes
    d.line(p((214, 114), (230, 108), (244, 112), (258, 107), (272, 112), (286, 114)), fill=detail, width=w, joint='curve')
    d.rectangle(p((230, 118), (240, 126)), fill=detail)
    d.rectangle(p((260, 118), (270, 126)), fill=detail)
    # The short beard's edge and the mouth
    d.arc(p((222, 128), (278, 188)), 10, 170, fill=detail, width=4 * S)
    d.line(p((242, 148), (258, 148)), fill=detail, width=4 * S)
    # Rope belt, the tunic's neckline and hem, the trousers' split
    d.line(p((190, 262), (310, 262)), fill=detail, width=6 * S)
    d.line(p((250, 262), (244, 290)), fill=detail, width=4 * S)
    d.line(p((182, 330), (318, 330)), fill=detail, width=4 * S)
    # The arms' edges against the tunic, the left hand, and the right hand on the shaft
    d.line(p((204, 206), (196, 262)), fill=detail, width=4 * S)
    d.line(p((298, 186), (306, 222), (318, 250)), fill=detail, width=4 * S, joint='curve')
    d.ellipse(p((326, 190), (356, 220)), fill=detail)
    d.ellipse(p((330, 194), (352, 216)), fill=colour)

def draw(colour, detail):
    sil = Image.new('L', (N, N))
    shapes(ImageDraw.Draw(sil), 255, 255)
    details(ImageDraw.Draw(sil), 255, 255)
    # The outline around the whole figure, about 10 pixels, in the detail color
    out = sil.filter(ImageFilter.GaussianBlur(6 * S)).point(lambda v: 255 if v > 8 else 0)
    im = Image.new('RGBA', (N, N), detail + (0,))
    im.paste(detail + (255,), mask=out)
    d = ImageDraw.Draw(im)
    shapes(d, colour + (255,), detail + (255,))
    details(d, colour + (255,), detail + (255,))
    return im.resize((512, 512), Image.LANCZOS)

draw(FILL, WHITE).save(OUT + 'militia-original.webp', lossless=True)
for name, (colour, detail) in COLORS.items():
    draw(colour, detail).save(OUT + 'militia-' + name + '.webp', quality=90)
