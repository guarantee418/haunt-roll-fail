#!/usr/bin/env python3
# Lion Kingdom (fan content): the clan's two warrior figures, in the flat style of unit-original and bodyguard-figure.py, with no
# Norse horns: a man-at-arms in a great helm with a sword and a heater shield (lion-sword-<color>.webp), and a longbowman in a
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

# --- the man-at-arms: great helm, sword raised, heater shield with a cross
def sword_shapes(d, colour, detail):
    body(d, colour)
    d.ellipse(p((222, 150), (278, 186)), fill=colour)
    # Great helm: a flat topped barrel
    d.polygon(p((212, 162), (210, 98), (224, 80), (276, 80), (290, 98), (288, 162)), fill=colour)
    # Sword arm, raised, and the sword: blade, point, crossguard, grip, pommel
    d.polygon(p((292, 174), (314, 176), (372, 118), (354, 100)), fill=colour)
    d.rectangle(p((356, 24), (370, 100)), fill=colour)
    d.polygon(p((356, 26), (363, 6), (370, 26)), fill=colour)
    d.rectangle(p((336, 96), (390, 108)), fill=colour)
    d.rectangle(p((357, 106), (369, 132)), fill=colour)
    d.ellipse(p((353, 128), (373, 148)), fill=colour)

def sword_details(d, colour, detail):
    w = 5 * S
    # The helm's eye slit and breathing cross
    d.line(p((214, 118), (286, 118)), fill=detail, width=8 * S)
    d.line(p((250, 126), (250, 156)), fill=detail, width=w)
    d.line(p((238, 140), (262, 140)), fill=detail, width=w)
    # The blade's fuller
    d.line(p((363, 22), (363, 92)), fill=detail, width=3 * S)
    # Belt
    d.line(p((250, 252), (310, 252)), fill=detail, width=w)
    # Heater shield in front: a detail-colored edge, the shield, an inner line, a cross
    shield = [(138, 190), (258, 190), (258, 262), (242, 318), (198, 372), (154, 318), (138, 262)]
    edge = [(x + (x - 198) * 0.09, y + (y - 270) * 0.08) for x, y in shield]
    d.polygon(p(*edge), fill=detail)
    d.polygon(p(*shield), fill=colour)
    inner = [(198 + (x - 198) * 0.8, 268 + (y - 268) * 0.82) for x, y in shield]
    d.line(p(*(inner + inner[:1])), fill=detail, width=4 * S, joint='curve')
    d.line(p((198, 214), (198, 336)), fill=detail, width=10 * S)
    d.line(p((156, 252), (240, 252)), fill=detail, width=10 * S)

# --- the longbowman: kettle hat, a longbow as tall as he is, a quiver of arrows on his back
BOW = (196, 236, 60, 206)   # the bow's arc: centre x, centre y, half width, half height

def bow_point(a):
    cx, cy, rx, ry = BOW
    return (cx + rx * math.cos(math.radians(a)), cy + ry * math.sin(math.radians(a)))

def bow_shapes(d, colour, detail):
    body(d, colour)
    # Quiver behind the right shoulder, and the arrows' shafts and fletchings above it
    d.polygon(p((294, 146), (318, 136), (350, 266), (326, 276)), fill=colour)
    for x, y in ((298, 102), (310, 96), (322, 100)):
        d.line(p((x + 4, y + 44), (x, y)), fill=colour, width=5 * S)
        d.polygon(p((x - 8, y + 6), (x, y - 12), (x + 8, y + 6), (x + 2, y + 20), (x - 2, y + 20)), fill=colour)
    # Head and a kettle hat: a dome with a wide brim
    d.ellipse(p((220, 98), (280, 166)), fill=colour)
    d.pieslice(p((218, 64), (282, 128)), 180, 360, fill=colour)
    d.ellipse(p((192, 88), (308, 112)), fill=colour)
    # Right arm down to the hip, left arm out to the bow's grip
    d.polygon(p((296, 174), (316, 182), (330, 276), (308, 280)), fill=colour)
    grip = bow_point(180)
    d.polygon(p((206, 172), (190, 200), (grip[0] + 2, grip[1] + 12), (grip[0] + 4, grip[1] - 8)), fill=colour)
    d.ellipse(p((grip[0] - 8, grip[1] - 12), (grip[0] + 16, grip[1] + 12)), fill=colour)
    # The longbow, thicker at the grip, and its string
    cx, cy, rx, ry = BOW
    for a in range(112, 249, 2):
        x, y = bow_point(a)
        r = 5 + 4 * math.cos(math.radians((a - 180) * 1.3))
        d.ellipse(p((x - r, y - r), (x + r, y + r)), fill=colour)
    d.line(p(bow_point(112), bow_point(248)), fill=colour, width=3 * S)

def bow_details(d, colour, detail):
    w = 5 * S
    # The hat's brim edge, and the eyes under it
    d.line(p((200, 103), (300, 103)), fill=detail, width=4 * S)
    d.rectangle(p((232, 124), (242, 132)), fill=detail)
    d.rectangle(p((258, 124), (268, 132)), fill=detail)
    # Belt, the tunic's hem, the quiver's band
    d.line(p((188, 252), (312, 252)), fill=detail, width=w)
    d.line(p((300, 170), (324, 160)), fill=detail, width=w)

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
