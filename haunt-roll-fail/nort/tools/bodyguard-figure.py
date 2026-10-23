#!/usr/bin/env python3
# Military paths module (fan content): draws the Leadership path's bodyguard (token/unit/bodyguard-<color>.webp), a third warrior
# figure in the flat style of unit-original and warchief-original: a horned huscarl behind a tall kite shield marked with
# Algiz (the protection rune), holding a spear. It is drawn in the originals' brown and white, as bodyguard-original, then
# recolored to each player color with the fill and detail colors the unit figures use (yellow's detail is black).
# Run from haunt-roll-fail/: python3 nort/tools/bodyguard-figure.py
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

def horn(side):
    # A thick horn like the unit figures': the lower outer quarter of a ring beside the helmet, from the helmet's side
    # out and up to a tip, thinner towards the tip as the hole is shifted out and up
    m = Image.new('1', (N, N))
    d = ImageDraw.Draw(m)
    cx, cy = 250 + side * 46, 58
    d.ellipse(p((cx - 56, cy - 56), (cx + 56, cy + 56)), fill=1)
    hx, hy = cx + side * 9, cy - 5
    d.ellipse(p((hx - 37, hy - 37), (hx + 37, hy + 37)), fill=0)
    d.rectangle(p((0, 0), (512, cy - 22)), fill=0)
    if side < 0:
        d.rectangle(p((cx + 6, 0), (512, 512)), fill=0)
    else:
        d.rectangle(p((0, 0), (cx - 6, 512)), fill=0)
    return m

def shapes(d, colour, detail):
    # The body: legs, torso, the spear arm
    d.polygon(p((206, 300), (238, 300), (236, 424), (196, 436), (190, 424), (204, 412)), fill=colour)
    d.polygon(p((262, 300), (294, 300), (296, 412), (312, 424), (306, 436), (266, 424)), fill=colour)
    d.polygon(p((196, 172), (304, 172), (318, 210), (300, 312), (200, 312), (186, 210)), fill=colour)
    d.polygon(p((296, 178), (316, 182), (362, 230), (352, 250), (308, 214)), fill=colour)
    # Head under the helmet, and the beard down the chest
    d.ellipse(p((214, 96), (286, 166)), fill=colour)
    d.polygon(p((216, 138), (284, 138), (262, 196), (250, 214), (238, 196)), fill=colour)
    # Helmet: dome, rim, a short spike, the horns
    d.pieslice(p((206, 70), (294, 150)), 180, 360, fill=colour)
    d.polygon(p((244, 76), (250, 56), (256, 76)), fill=colour)
    for side in (-1, 1):
        d.bitmap((0, 0), horn(side), fill=colour)
    # Spear: shaft, leaf-shaped head and a small butt cap
    d.rectangle(p((354, 52), (366, 432)), fill=colour)
    d.polygon(p((360, 6), (375, 34), (369, 60), (351, 60), (345, 34)), fill=colour)
    d.ellipse(p((345, 222), (375, 252)), fill=colour)

def details(d, colour, detail):
    w = 5 * S
    # Helmet rim and eyes
    d.line(p((210, 110), (290, 110)), fill=detail, width=w)
    d.rectangle(p((228, 120), (240, 128)), fill=detail)
    d.rectangle(p((260, 120), (272, 128)), fill=detail)
    # The beard's edge
    d.line(p((222, 142), (250, 210), (278, 142)), fill=detail, width=4 * S, joint='curve')
    # The spear head's midrib
    d.line(p((360, 18), (360, 54)), fill=detail, width=3 * S)
    # Kite shield in front: a detail-colored edge, the shield, an inner line, the Algiz rune
    shield = [(146, 186), (262, 186), (266, 236), (250, 316), (204, 396), (158, 316), (142, 236)]
    edge = [(x + (x - 204) * 0.09, y + (y - 270) * 0.08) for x, y in shield]
    d.polygon(p(*edge), fill=detail)
    d.polygon(p(*shield), fill=colour)
    inner = [(204 + (x - 204) * 0.8, 272 + (y - 272) * 0.82) for x, y in shield]
    d.line(p(*(inner + inner[:1])), fill=detail, width=4 * S, joint='curve')
    d.line(p((204, 222), (204, 346)), fill=detail, width=9 * S)
    d.line(p((204, 262), (174, 226)), fill=detail, width=9 * S)
    d.line(p((204, 262), (234, 226)), fill=detail, width=9 * S)

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

draw(FILL, WHITE).save(OUT + 'bodyguard-original.webp', lossless=True)
for name, (colour, detail) in COLORS.items():
    draw(colour, detail).save(OUT + 'bodyguard-' + name + '.webp', quality=90)
