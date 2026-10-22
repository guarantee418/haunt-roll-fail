#!/usr/bin/env python3
# Military paths module (fan content): draws the Militia's shield badge (ui/militia.webp). (Guardian 3's Palisade is the
# Fortress, so its own token was dropped on 2026-10-22.) Run from haunt-roll-fail/: python3 nort/tools/military-icons.py
import math, random
from PIL import Image, ImageDraw, ImageFilter

IMAGES = 'webp2/nort/images/'
W, H = 317, 300
S = 4  # supersampling

def lerp(a, b, t):
    return tuple(int(a[i] + (b[i] - a[i]) * t) for i in range(len(a)))

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

shield()
