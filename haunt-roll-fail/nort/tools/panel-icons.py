# Makes the player panel icons in webp2/nort/images/ui/ (run from haunt-roll-fail/):
# unit.webp, the unit figure from the Active Area board's Winter costs (the Recruit card's figure, without its + sign);
# card-hand.webp, card-draw.webp, card-discard.webp, the start card back outlined green, yellow and red;
# first-player.webp, the first player marker without its background.
from collections import deque
from PIL import Image, ImageDraw, ImageFilter

IMAGES = 'webp2/nort/images/'

# The pixels connected to the image's edge through pixels for which isbg is true
def background(img, isbg):
    w, h = img.size
    px = img.load()
    bg = bytearray(w * h)
    q = deque([(x, y) for x in range(w) for y in (0, h - 1)] + [(x, y) for y in range(h) for x in (0, w - 1)])
    while q:
        x, y = q.popleft()
        if x < 0 or y < 0 or x >= w or y >= h or bg[y * w + x] or not isbg(px[x, y]):
            continue
        bg[y * w + x] = 1
        q.extend([(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)])
    return Image.frombytes('L', (w, h), bytes(0 if v else 255 for v in bg))

# Cuts the image out with the mask and centers it on a 64x64 icon
def icon(img, mask, out):
    r = img.convert('RGBA')
    r.putalpha(mask)
    r = r.crop(mask.point(lambda v: 255 if v > 20 else 0).getbbox())
    s = 64 / max(r.size)
    r = r.resize((round(r.size[0] * s), round(r.size[1] * s)), Image.LANCZOS)
    c = Image.new('RGBA', (64, 64))
    c.paste(r, ((64 - r.size[0]) // 2, (64 - r.size[1]) // 2))
    c.save(IMAGES + 'ui/' + out, quality=90)

# The figure has a white outline: everything outside it is background
unit = Image.open(IMAGES + 'expansion/board/active-area.webp').convert('RGB').crop((395, 870, 480, 945))
icon(unit, background(unit, lambda p: min(p) <= 150).filter(ImageFilter.GaussianBlur(0.6)), 'unit.webp')

# The marker sits on a flat gray background with a bluish rim
marker = Image.open(IMAGES + 'token/first-player.webp').convert('RGB')
icon(marker, background(marker, lambda p: max(p) < 60 and max(p) - min(p) < 8).filter(ImageFilter.MinFilter(3)).filter(ImageFilter.GaussianBlur(1.5)), 'first-player.webp')

# A card tilted like the card icons printed on the cards, drawn 8 times larger then scaled down
K = 8
W, H = 40 * K, 56 * K
art = Image.open(IMAGES + 'card/back/start.webp').convert('RGBA').resize((W, H), Image.LANCZOS)
for name, color in [('hand', (110, 200, 80)), ('draw', (240, 200, 60)), ('discard', (220, 60, 50))]:
    shape = Image.new('L', (W, H))
    ImageDraw.Draw(shape).rounded_rectangle((0, 0, W - 1, H - 1), radius=5 * K, fill=255)
    card = Image.new('RGBA', (W, H))
    card.paste(art, (0, 0), shape)
    ImageDraw.Draw(card).rounded_rectangle((0, 0, W - 1, H - 1), radius=5 * K, outline=color + (255,), width=4 * K)
    c = Image.new('RGBA', (64 * K, 64 * K))
    c.paste(card, ((64 * K - W) // 2, (64 * K - H) // 2), card)
    c.rotate(-12, resample=Image.BICUBIC).resize((64, 64), Image.LANCZOS).save(IMAGES + 'ui/card-' + name + '.webp', quality=90)

# The harvest and Winter rows' icons, drawn 8 times larger then scaled down, with a dark outline
import math

def ellipse(d, cx, cy, rx, ry, angle, fill):
    a = math.radians(angle)
    pts = [(rx * math.cos(t), ry * math.sin(t)) for t in [i * math.pi / 24 for i in range(48)]]
    d.polygon([(cx + x * math.cos(a) - y * math.sin(a), cy + x * math.sin(a) + y * math.cos(a)) for x, y in pts], fill=fill)

def outlined(shape, out, outline=(40, 30, 20, 255)):
    S = shape.size[0]
    edge = Image.new('RGBA', shape.size, outline)
    edge.putalpha(shape.getchannel('A').filter(ImageFilter.MaxFilter(2 * K + 1)))
    edge.alpha_composite(shape)
    edge.resize((64, 64), Image.LANCZOS).save(IMAGES + 'ui/' + out, quality=90)

S = 64 * K

# harvest.webp: a wheat sheaf, stalks fanning out above and below a tied band
wheat = Image.new('RGBA', (S, S))
d = ImageDraw.Draw(wheat)
band = (S / 2, S * 0.60)
stalks = [-34, -17, 0, 17, 34]
for a in stalks:
    r = math.radians(a)
    top = (band[0] + math.sin(r) * S * 0.30, band[1] - math.cos(r) * S * 0.30)
    bottom = (band[0] - math.sin(r) * S * 0.36 * 0.55, band[1] + S * 0.36)
    d.line([bottom, band, top], fill=(196, 146, 52, 255), width=3 * K, joint='curve')
for a in stalks:
    r = math.radians(a)
    for i in range(4):
        t = S * (0.30 + 0.065 * i)
        cx, cy = band[0] + math.sin(r) * t, band[1] - math.cos(r) * t
        for side in (-1, 1):
            ox, oy = math.cos(r) * side * S * 0.028, math.sin(r) * side * S * 0.028
            ellipse(d, cx + ox, cy + oy, S * 0.024, S * 0.042, a + side * 28, (236, 190, 72, 255))
    t = S * 0.30 + 0.065 * 4 * S - S * 0.02
    ellipse(d, band[0] + math.sin(r) * t, band[1] - math.cos(r) * t, S * 0.022, S * 0.045, a, (236, 190, 72, 255))
d.rounded_rectangle((S * 0.36, band[1] - S * 0.035, S * 0.64, band[1] + S * 0.035), radius=S * 0.02, fill=(150, 84, 34, 255))
outlined(wheat, 'harvest.webp')

# winter.webp: a snowflake, six branched arms
flake = Image.new('RGBA', (S, S))
d = ImageDraw.Draw(flake)
c = S / 2
ice = (205, 235, 255, 255)
for i in range(6):
    r = math.radians(60 * i - 90)
    def at(t, off=0):
        return (c + math.cos(r) * t - math.sin(r) * off, c + math.sin(r) * t + math.cos(r) * off)
    d.line([at(0), at(S * 0.44)], fill=ice, width=5 * K)
    for t, l in [(0.20, 0.13), (0.32, 0.09)]:
        for side in (-1, 1):
            b = r + side * math.radians(55)
            p = at(S * t)
            d.line([p, (p[0] + math.cos(b) * S * l, p[1] + math.sin(b) * S * l)], fill=ice, width=4 * K)
d.ellipse((c - S * 0.07, c - S * 0.07, c + S * 0.07, c + S * 0.07), fill=ice)
outlined(flake, 'winter.webp', (30, 70, 120, 255))
