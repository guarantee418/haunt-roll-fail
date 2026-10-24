#!/usr/bin/env python3
# Lion Kingdom (fan content): the clan's two warrior figures, in the flat style of unit-original and bodyguard-figure.py, with no
# Norse horns: a man-at-arms in a steel cap holding a longsword upright (lion-sword-<color>.webp), and a longbowman in a
# kettle hat with a quiver (lion-bow-<color>.webp). Asked for by Gman on 2026-10-24, so the Lion's warriors look English or French.
# Drawn in the originals' brown and white (-original), then in each player color with the unit figures' fill and detail colors.
# Run from haunt-roll-fail/: python3 nort/tools/lion-warriors.py
import math
from PIL import Image, ImageDraw, ImageFilter

OUT = 'webp2/nort/images/token/unit/'
S = 4               # supersampling
N = 512 * S
FILL = (168, 66, 46)
WHITE = (255, 255, 255)
# Player color: (fill, detail), as in unit-<color>.webp
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

def body(d, colour):
    # Legs and a tunic down to the knees
    d.polygon(p((206, 300), (238, 300), (236, 424), (196, 436), (190, 424), (204, 412)), fill=colour)
    d.polygon(p((262, 300), (294, 300), (296, 412), (312, 424), (306, 436), (266, 424)), fill=colour)
    d.polygon(p((198, 170), (302, 170), (316, 210), (312, 324), (188, 324), (184, 210)), fill=colour)

# --- the man-at-arms: a round steel cap, both hands on a longsword held upright at his right; drawn like the longbowman, with curves
# and rounded corners, and no shield or helm, so he looks nothing like the bodyguard (Gman, 2026-10-24)
POMMEL, BLADE = (336, 262), (0.30, -0.954)   # the sword's pommel and the direction of its point

def along(t, side=0):
    # the point t pixels from the pommel along the sword, side pixels across it
    return (POMMEL[0] + BLADE[0] * t - BLADE[1] * side, POMMEL[1] + BLADE[1] * t + BLADE[0] * side)

def sword_shapes(d, colour, detail):
    m = Image.new('L', (N, N))
    e = ImageDraw.Draw(m)
    limb(e, 255, [(232, 318), (214, 370), (204, 416)], 19, 13)
    limb(e, 255, [(270, 318), (288, 370), (298, 416)], 19, 13)
    e.ellipse(p((176, 410), (222, 434)), fill=255)
    e.ellipse(p((280, 410), (326, 434)), fill=255)
    # A surcoat over the mail, belted, down to the knees
    e.polygon(p(*curve([(204, 172), (228, 158), (276, 158), (300, 172), (304, 200), (290, 246), (300, 300), (310, 336), (276, 342), (250, 338),
        (224, 342), (192, 336), (202, 300), (212, 246), (198, 200)])), fill=255)
    # Neck, head, and a round steel cap with a rim
    e.polygon(p((206, 172), (226, 146), (278, 146), (298, 172)), fill=255)
    e.ellipse(p((236, 136), (268, 168)), fill=255)
    e.ellipse(p((222, 92), (282, 154)), fill=255)
    e.pieslice(p((218, 74), (286, 142)), 180, 360, fill=255)
    e.ellipse(p((212, 100), (292, 116)), fill=255)
    # Both arms to the grip at his right, the right elbow out, the left arm across the body, the fists around the grip
    h1, h2 = along(16), along(38)
    limb(e, 255, [(212, 178), (248, 234), (h1[0] - 4, h1[1])], 15, 11)
    limb(e, 255, [(292, 176), (326, 210), (h2[0] + 2, h2[1] + 2)], 15, 11)
    for x, y in (h1, h2):
        e.ellipse(p((x - 13, y - 12), (x + 13, y + 12)), fill=255)
    e.polygon(p((296, 196), (326, 210), h2, h1, (276, 244)), fill=255)
    m = m.filter(ImageFilter.GaussianBlur(3 * S)).point(lambda v: 255 if v > 128 else 0)
    d.bitmap((0, 0), m, fill=colour)
    # The belt
    d.line(p(*bezier((212, 252), (252, 262), (292, 252), 20)), fill=detail, width=5 * S)
    # The longsword held upright: pommel under the fists, a straight crossguard over them, a long blade tapering to its point
    d.ellipse(p(*(lambda c: ((c[0] - 9, c[1] - 9), (c[0] + 9, c[1] + 9)))(along(0))), fill=colour)
    d.polygon(p(along(0, -5), along(50, -5), along(50, 5), along(0, 5)), fill=colour)
    d.polygon(p(along(50, -30), along(58, -30), along(58, 30), along(50, 30)), fill=colour)
    for side in (-31, 31):
        d.ellipse(p(*(lambda c: ((c[0] - 6, c[1] - 6), (c[0] + 6, c[1] + 6)))(along(54, side))), fill=colour)
    d.polygon(p(along(56, -8), along(228, -6), along(252, 0), along(228, 6), along(56, 8)), fill=colour)

def sword_details(d, colour, detail):
    w = 4 * S
    # The cap's rim, the eyes, the collar
    d.line(p(*bezier((216, 108), (252, 114), (288, 108), 20)), fill=detail, width=w)
    d.ellipse(p((233, 124), (243, 133)), fill=detail)
    d.ellipse(p((261, 124), (271, 133)), fill=detail)
    d.line(p((232, 168), (252, 190), (272, 168)), fill=detail, width=w, joint='curve')
    # The blade's fuller
    d.line(p(along(64), along(218)), fill=detail, width=3 * S)

# --- the longbowman: kettle hat, drawing a longbow, a quiver of arrows at his hip; drawn with curves and then rounded, so he
# isn't built of boxes (Gman, 2026-10-24)
def curve(pts, n=12):
    # a closed Catmull-Rom curve through the points
    out = []
    for i in range(len(pts)):
        p0, p1, p2, p3 = pts[i - 1], pts[i], pts[(i + 1) % len(pts)], pts[(i + 2) % len(pts)]
        for k in range(n):
            t = k / n
            out.append(tuple(0.5 * (2 * p1[j] + (p2[j] - p0[j]) * t + (2 * p0[j] - 5 * p1[j] + 4 * p2[j] - p3[j]) * t * t + (3 * p1[j] - p0[j] - 3 * p2[j] + p3[j]) * t ** 3) for j in (0, 1)))
    return out

def limb(d, colour, pts, r0, r1):
    # a tapered limb along a path, thicker at its start
    for i in range(len(pts) - 1):
        (x0, y0), (x1, y1) = pts[i], pts[i + 1]
        for k in range(21):
            t = (i + k / 20) / (len(pts) - 1)
            x, y = x0 + (x1 - x0) * k / 20, y0 + (y1 - y0) * k / 20
            r = r0 + (r1 - r0) * t
            d.ellipse(p((x - r, y - r), (x + r, y + r)), fill=colour)

def bezier(a, c, b, n=60):
    return [((1 - t) ** 2 * a[0] + 2 * (1 - t) * t * c[0] + t * t * b[0], (1 - t) ** 2 * a[1] + 2 * (1 - t) * t * c[1] + t * t * b[1]) for t in (k / n for k in range(n + 1))]

TOP, GRIP, BOTTOM, HAND = (142, 20), (118, 162), (142, 304), (278, 148)

def bow_shapes(d, colour, detail):
    m = Image.new('L', (N, N))
    e = ImageDraw.Draw(m)
    # Legs in a wide stance, and the feet turned out
    limb(e, 255, [(232, 318), (214, 370), (204, 416)], 19, 13)
    limb(e, 255, [(270, 318), (288, 370), (298, 416)], 19, 13)
    e.ellipse(p((176, 410), (222, 434)), fill=255)
    e.ellipse(p((280, 410), (326, 434)), fill=255)
    # Tunic: broad shoulders, a belted waist, a skirt flaring to the knees
    e.polygon(p(*curve([(204, 172), (228, 158), (276, 158), (300, 172), (304, 200), (290, 246), (300, 300), (310, 336), (276, 342), (250, 338),
        (224, 342), (192, 336), (202, 300), (212, 246), (198, 200)])), fill=255)
    # Neck, head, kettle hat: a dome and a brim curving down at its ends
    e.ellipse(p((236, 136), (268, 168)), fill=255)
    e.polygon(p((206, 172), (226, 146), (278, 146), (298, 172)), fill=255)
    e.ellipse(p((222, 90), (282, 152)), fill=255)
    e.pieslice(p((220, 72), (284, 132)), 180, 360, fill=255)
    e.polygon(p(*curve([(188, 114), (214, 101), (252, 98), (290, 101), (316, 114), (290, 110), (252, 108), (214, 110)], 8)), fill=255)
    # Bow arm held out to the grip, and the drawing arm with its elbow raised behind
    limb(e, 255, [(214, 176), (168, 168), (126, 162)], 15, 11)
    e.ellipse(p((GRIP[0] - 4, GRIP[1] - 14), (GRIP[0] + 22, GRIP[1] + 12)), fill=255)
    limb(e, 255, [(290, 176), (330, 160), (HAND[0] + 6, HAND[1])], 15, 11)
    e.ellipse(p((HAND[0] - 10, HAND[1] - 12), (HAND[0] + 14, HAND[1] + 12)), fill=255)
    # Quiver at the right hip, with three fletchings showing
    e.polygon(p(*curve([(300, 232), (322, 226), (340, 316), (318, 324)], 6)), fill=255)
    for x, y in ((298, 206), (310, 200), (322, 206)):
        e.polygon(p((x - 7, y + 8), (x, y - 10), (x + 7, y + 8), (x + 2, y + 30), (x - 2, y + 30)), fill=255)
    # The longbow, thick at the grip and thin at the tips, with small recurved tips
    pts = bezier(TOP, (74, 162), GRIP) + bezier(GRIP, (74, 162), BOTTOM)[1:]
    for i, (x, y) in enumerate(pts):
        r = 4 + 5 * math.sin(math.pi * i / (len(pts) - 1))
        e.ellipse(p((x - r, y - r), (x + r, y + r)), fill=255)
    for (x, y), dy in ((TOP, 1), (BOTTOM, -1)):
        e.ellipse(p((x - 3, y - 3 * dy - 4), (x + 7, y - 3 * dy + 4)), fill=255)
    # Rounded: every corner softened
    m = m.filter(ImageFilter.GaussianBlur(3 * S)).point(lambda v: 255 if v > 128 else 0)
    d.bitmap((0, 0), m, fill=colour)
    # The string drawn back to the hand, and the arrow on it, its head past the bow
    d.line(p(TOP, HAND, BOTTOM), fill=colour, width=3 * S)
    d.line(p((HAND[0] + 4, HAND[1]), (96, 163)), fill=colour, width=5 * S)
    d.polygon(p((84, 164), (100, 154), (98, 172)), fill=colour)

def bow_details(d, colour, detail):
    w = 4 * S
    # The hat's brim edge and the eyes
    d.line(p(*curve([(196, 112), (252, 104), (308, 112), (252, 106)], 8)[:9]), fill=detail, width=w)
    d.ellipse(p((233, 122), (243, 131)), fill=detail)
    d.ellipse(p((261, 122), (271, 131)), fill=detail)
    # The tunic's collar, the belt and its buckle
    d.line(p((232, 168), (252, 190), (272, 168)), fill=detail, width=w, joint='curve')
    d.line(p(*bezier((212, 252), (252, 262), (292, 252), 20)), fill=detail, width=5 * S)
    d.rectangle(p((245, 251), (259, 264)), outline=detail, width=3 * S)

def draw(shapes, details, colour, detail):
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

for kind, shapes, details in (('sword', sword_shapes, sword_details), ('bow', bow_shapes, bow_details)):
    draw(shapes, details, FILL, WHITE).save(OUT + 'lion-' + kind + '-original.webp', lossless=True)
    for name, (colour, detail) in COLORS.items():
        draw(shapes, details, colour, detail).save(OUT + 'lion-' + kind + '-' + name + '.webp', quality=90)
