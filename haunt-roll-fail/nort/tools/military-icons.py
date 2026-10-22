#!/usr/bin/env python3
# Military paths module (fan content): draws the Palisade building token (token/building/palisade.webp, the size and look of
# the other building tokens, with the combat axe in its corner as the Defense Tower has its skull) and the Militia's shield
# badge (ui/militia.webp). Run from haunt-roll-fail/: python3 nort/tools/military-icons.py
import math, random
from PIL import Image, ImageDraw, ImageFilter

IMAGES = 'webp2/nort/images/'
W, H = 317, 300
S = 4  # supersampling

def lerp(a, b, t):
    return tuple(int(a[i] + (b[i] - a[i]) * t) for i in range(len(a)))

def palisade():
    random.seed(7)
    w, h = W * S, H * S
    im = Image.new('RGB', (w, h))
    d = ImageDraw.Draw(im)
    # Grass, lighter towards the top, like the other tokens' painted ground
    for y in range(h):
        d.line([(0, y), (w, y)], fill=lerp((96, 150, 92), (58, 112, 60), y / h))
    blobs = Image.new('RGBA', (w, h))
    bd = ImageDraw.Draw(blobs)
    for _ in range(40):
        x, y, r = random.randint(0, w), random.randint(0, h), random.randint(30, 90) * S // 4
        bd.ellipse([x - r, y - r // 2, x + r, y + r // 2], fill=random.choice([(80, 132, 70, 90), (120, 170, 100, 80), (70, 110, 90, 70)]))
    blobs = blobs.filter(ImageFilter.GaussianBlur(6 * S))
    im.paste(blobs, (0, 0), blobs)

    # Shadow under the wall
    shadow = Image.new('RGBA', (w, h))
    sd = ImageDraw.Draw(shadow)
    sd.ellipse([30 * S, 215 * S, 300 * S, 285 * S], fill=(20, 40, 20, 140))
    shadow = shadow.filter(ImageFilter.GaussianBlur(8 * S))
    im.paste(shadow, (0, 0), shadow)

    d = ImageDraw.Draw(im)
    n = 8
    logs = []
    for i in range(n):
        t = i / (n - 1)
        cx = (52 + t * 215) * S
        # A slight arc, the middle stakes further back
        base = (250 - 22 * math.sin(t * math.pi)) * S
        height = (150 + random.randint(-12, 12) - 10 * math.sin(t * math.pi)) * S
        r = (15 + random.randint(-1, 2)) * S
        logs.append((cx, base, height, r))

    for cx, base, height, r in sorted(logs, key=lambda l: l[1]):
        top = base - height
        tip = top - 2.2 * r
        bark = (122 + random.randint(-12, 12), 78 + random.randint(-8, 8), 42)
        dark = lerp(bark, (40, 24, 12), 0.55)
        light = lerp(bark, (230, 190, 130), 0.35)
        # Body, shaded from the left (light) to the right (dark)
        for k in range(2 * r):
            u = k / (2 * r)
            col = lerp(light, dark, u) if u > 0.35 else lerp(lerp(bark, light, 0.6), light, u / 0.35)
            x = cx - r + k
            d.line([(x, top), (x, base)], fill=col)
        d.ellipse([cx - r, base - r // 2, cx + r, base + r // 2], fill=dark)
        # The sharpened point: cut wood, pale
        d.polygon([(cx - r, top), (cx + r, top), (cx, tip)], fill=(214, 176, 120))
        d.polygon([(cx, tip), (cx + r, top), (cx + r // 3, top)], fill=(170, 128, 80))
        # Bark grain
        for _ in range(5):
            gx = cx + random.randint(-r + 3 * S, r - 3 * S)
            gy = random.randint(int(top + 8 * S), int(base - 20 * S))
            d.line([(gx, gy), (gx, gy + random.randint(10, 26) * S)], fill=dark, width=S)
        d.line([(cx - r, top), (cx - r, base)], fill=(50, 30, 14), width=2 * S)
        d.line([(cx + r, top), (cx + r, base)], fill=(50, 30, 14), width=2 * S)

    # Two rope bindings across the stakes
    for frac in (0.32, 0.7):
        pts = []
        for cx, base, height, r in logs:
            pts.append((cx, base - height * (1 - frac)))
        d.line(pts, fill=(70, 46, 22), width=7 * S, joint='curve')
        d.line([(x, y - 2 * S) for x, y in pts], fill=(196, 160, 98), width=3 * S, joint='curve')

    im = im.resize((W, H), Image.LANCZOS)

    # The combat axe in the top left corner, on a dark disc, as the Defense Tower shows its skull
    axe = Image.open(IMAGES + 'ui/axe.webp').convert('RGBA').resize((52, 52), Image.LANCZOS)
    disc = Image.new('RGBA', (66, 66))
    ImageDraw.Draw(disc).ellipse([0, 0, 65, 65], fill=(20, 24, 28, 200))
    disc.paste(axe, (7, 7), axe)
    im.paste(disc, (18, 16), disc)

    im.save(IMAGES + 'token/building/palisade.webp', quality=90)

def shield():
    s = 64 * S
    im = Image.new('RGBA', (s, s))
    d = ImageDraw.Draw(im)
    def outline(m):
        return [(m, m), (s - m, m), (s - m, s * 0.5), (s / 2, s - m), (m, s * 0.5)]
    def curved(m):
        pts = [(m, 6 * S + m)]
        for i in range(21):
            t = i / 20
            pts.append((m + t * (s - 2 * m), m + 4 * S * math.sin(t * math.pi) * -1 + 6 * S))
        for i in range(21):
            t = i / 20
            x = s - m - t * (s / 2 - m)
            y = 6 * S + m + (s - 7 * S - 2 * m) * (math.sin(t * math.pi / 2) ** 1.4)
            pts.append((x, y))
        for i in range(21):
            t = i / 20
            x = s / 2 - t * (s / 2 - m)
            y = s - m - 1 * S - (s - 7 * S - 2 * m) * (1 - math.cos(t * math.pi / 2)) ** 1.4 * 0 - (s - 7 * S - 2 * m) * (1 - math.sin((1 - t) * math.pi / 2) ** 1.4)
            pts.append((x, y))
        return pts
    d.polygon(curved(2 * S), fill=(30, 26, 22, 255))
    d.polygon(curved(6 * S), fill=(200, 200, 205, 255))
    d.polygon(curved(10 * S), fill=(138, 92, 52, 255))
    # A steel boss and band
    d.rectangle([s / 2 - 3 * S, 12 * S, s / 2 + 3 * S, s - 14 * S], fill=(205, 205, 210, 255))
    d.ellipse([s / 2 - 8 * S, s * 0.42 - 8 * S, s / 2 + 8 * S, s * 0.42 + 8 * S], fill=(225, 225, 230, 255), outline=(60, 60, 60, 255), width=2 * S)
    im = im.resize((64, 64), Image.LANCZOS)
    im.save(IMAGES + 'ui/militia.webp', lossless=True)

palisade()
shield()
