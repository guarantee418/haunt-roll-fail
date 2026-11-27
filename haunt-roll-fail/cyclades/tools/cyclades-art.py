#!/usr/bin/env python3
# Makes the Cyclades images in webp2/cyclades/images/ from the Tabletop Simulator mods' images.
#
# Each mod's save: POST itemcount=1&publishedfileids[0]=<id> to
# https://api.steampowered.com/ISteamRemoteStorage/GetPublishedFileDetails/v1/, download its file_url (BSON) and
# decode it (pymongo's bson). Old cloud-3.steamusercontent.com image URLs answer 403: use
# https://cdn.steamusercontent.com/ugc/<a>/<b>/ instead. The images used, saved into one directory under these names:
#
#   map23.img      mod 2104694895, the 2-3 player board photo  916921704022930267/515ABA2F44186F43420F134334989780AD6D8ED4
#   board4.img     mod 2424452225, "Cyclades 4 Player"         438364144701645328/5A8F0ABA6FEEBF0DFACB83FA65D6FF30A2F1951D
#   board5.img     mod 2424452225, "Cyclades 5 Players"        1771575870878912263/4676CAF2420A9AD623C116C77D04D9842B6D3A83
#   god1.img ... god4.img  mod 2424452225, the god tiles (Poseidon, Zeus, Athena, Ares)
#                  1771575870878911258/B4334D2C0566E3350051F2A17208E868A0ADC10B, 1771575870878908296/CEE7E324FC869E54657EB2448854BDDC90FD7D22,
#                  1771575870878911076/1F11B9F5D23B3477EDD0517FA197919762AAD17D, 1771575870878910736/556311FC91AD3E78E951443C2FC5CB51BFF96C2E
#   titanstate.img mod 2424452225, the gods track with Apollo  1771575870878911640/A8F2B3242263C44EB16D036C5BD3740202BB50F6
#   port.img, fortress.img, temple.img, university.img, metropolis.img, prosperity.img: mod 2424452225's tiles of those names
#   priest.img     mod 2424452225, deck "Priest" (its 1st card is the Philosopher, its 2nd the Priest)
#   creatures.img  mod 2104694895, the Creature cards (10x7, cards 3000-3016)  1019448453675947804/783B8E852FDA69F859DD0CF9E293F7D2DEC9CA18
#   setup.img      mod 2424452225's rulebook setup pages, whose Troop and Fleet silhouettes are recolored for the pieces
#
# The 2-player board the second mod has is missing an island, so the 2-3 player map is the first mod's photo.
#
# Usage: cyclades-art.py <dir> <out>, where <out> is webp2/cyclades/images.

import sys, os
import numpy as np
from PIL import Image, ImageFilter, ImageDraw

COLORS = {
    'red':    (206, 32, 36),
    'yellow': (238, 196, 24),
    'blue':   (36, 112, 214),
    'black':  (40, 40, 44),
    'green':  (40, 150, 60),
}

src, out = sys.argv[1], sys.argv[2]
os.makedirs(out, exist_ok=True)

def load(n):
    return Image.open(os.path.join(src, n + '.img'))

def save(im, n, q=88):
    p = os.path.join(out, n + '.webp')
    os.makedirs(os.path.dirname(p), exist_ok=True)
    im.save(p, 'WEBP', quality=q, method=6)

# MAPS: the grids in board.scala are measured on these sizes
save(load('board5').convert('RGB').resize((2048, 2048), Image.LANCZOS), 'map-5', 85)
save(load('board4').convert('RGB').resize((1680, 2048), Image.LANCZOS), 'map-4', 85)
m = load('map23').convert('RGB')
save(m.resize((round(m.width * 0.85), round(m.height * 0.85)), Image.LANCZOS), 'map-23', 85)

# GODS
for i, n in enumerate(['poseidon', 'zeus', 'athena', 'ares']):
    save(load('god' + str(i + 1)).convert('RGB').resize((1000, 272), Image.LANCZOS), 'god/' + n)
t = load('titanstate').convert('RGB')
s = t.width / 380
save(t.crop((round(40 * s), round(808 * s), round(355 * s), round(882 * s))).resize((1000, 235), Image.LANCZOS), 'god/apollo')
# round portraits, the first row of the track: Zeus, Athena, Poseidon, Ares; centred on their orange rims
# (measured 2026-11-27: the old centres sat about 11 units right, showing the next rim on the main menu)
for n, x in [('zeus', 86.1), ('athena', 156.0), ('poseidon', 226.0), ('ares', 295.9)]:
    c = t.crop((round((x - 29.8) * s), round((373.4 - 29.8) * s), round((x + 29.8) * s), round((373.4 + 29.8) * s))).resize((160, 160), Image.LANCZOS).convert('RGBA')
    mask = Image.new('L', (160, 160), 0)
    ImageDraw.Draw(mask).ellipse((2, 2, 157, 157), fill=255)
    c.putalpha(mask)
    save(c, 'god/head-' + n)
a = t.crop((round(48 * s), round(812 * s), round(110 * s), round(880 * s))).resize((150, 164), Image.LANCZOS)
save(a, 'god/head-apollo')

# BUILDINGS
for n in ['port', 'fortress', 'temple', 'university']:
    save(load(n).convert('RGB').resize((160, 160), Image.LANCZOS), 'building/' + n)
save(load('metropolis').convert('RGB').resize((240, 240), Image.LANCZOS), 'building/metropolis')
p = load('prosperity').convert('RGBA')
p.thumbnail((160, 160))
save(p, 'prosperity')

# PRIEST AND PHILOSOPHER CARDS
d = load('priest').convert('RGB')
w, h = d.width / 10, d.height / 7
for n, i in [('priest', 1), ('philosopher', 0)]:
    save(d.crop((round(i * w), 0, round((i + 1) * w), round(h))).resize((300, 430), Image.LANCZOS), 'card/' + n)

# CREATURES, in the mod's card order
names = ['minotaur', 'chimera', 'fates', 'chiron', 'sphinx', 'polyphemus', 'pegasus', 'medusa', 'harpy', 'dryad',
    'griffon', 'satyr', 'sylph', 'giant', 'kraken', 'siren', 'cyclops']
c = load('creatures').convert('RGB')
w, h = c.width / 10, c.height / 7
for i, n in enumerate(names):
    x, y = i % 10, i // 10
    card = c.crop((round(x * w), round(y * h), round((x + 1) * w), round((y + 1) * h)))
    save(card.resize((400, round(400 * card.height / card.width)), Image.LANCZOS), 'creature/' + n)
    # the art in the card's top, a round token for the figurines on the map
    t = card.crop((round(card.width * 0.12), round(card.height * 0.08), round(card.width * 0.88), round(card.height * 0.08 + card.width * 0.76))).resize((160, 160), Image.LANCZOS).convert('RGBA')
    mask = Image.new('L', (160, 160), 0)
    ImageDraw.Draw(mask).ellipse((3, 3, 156, 156), fill=255)
    t.putalpha(mask)
    ring = Image.new('RGBA', (160, 160), (0, 0, 0, 0))
    ImageDraw.Draw(ring).ellipse((1, 1, 158, 158), outline=(230, 200, 120, 255), width=6)
    save(Image.alpha_composite(t, ring), 'figure/' + n)

# the 8-connected parts of a mask at least 3% the size of the largest (the silhouette, without specks of the background)
def largest(m):
    seen = np.zeros(m.shape, bool)
    parts = []
    for y0, x0 in zip(*np.nonzero(m)):
        if seen[y0, x0]:
            continue
        part, stack = [], [(y0, x0)]
        seen[y0, x0] = True
        while stack:
            y, x = stack.pop()
            part.append((y, x))
            for dy in (-1, 0, 1):
                for dx in (-1, 0, 1):
                    v, u = y + dy, x + dx
                    if 0 <= v < m.shape[0] and 0 <= u < m.shape[1] and m[v, u] and not seen[v, u]:
                        seen[v, u] = True
                        stack.append((v, u))
        parts.append(part)
    big = max(len(p) for p in parts)
    r = np.zeros(m.shape, bool)
    for p in parts:
        if len(p) >= big * 0.03:
            for y, x in p:
                r[y, x] = True
    return r

# PIECES: the rulebook's red Troop and Fleet, recolored with a light outline
st = load('setup').convert('RGB')
for n, (x, y), r in [('troop', (3403, 446), 170), ('fleet', (3755, 362), 130)]:
    a = np.asarray(st.crop((x - r, y - r, x + r, y + r))).astype(int)
    m = (a[:, :, 0] > 150) & (a[:, :, 1] < 90) & (a[:, :, 2] < 100) & (a[:, :, 0] - a[:, :, 1] > 110)
    m = largest(m)
    mask = Image.fromarray((m * 255).astype('uint8')).filter(ImageFilter.MaxFilter(3)).filter(ImageFilter.MinFilter(3))
    bb = mask.getbbox()
    mask = mask.crop((bb[0] - 12, bb[1] - 12, bb[2] + 12, bb[3] + 12))
    mask = mask.resize((mask.width * 3, mask.height * 3), Image.BICUBIC).filter(ImageFilter.GaussianBlur(1.5)).point(lambda v: 255 if v > 110 else 0)
    mask = mask.filter(ImageFilter.GaussianBlur(1.2))
    edge = mask.filter(ImageFilter.MaxFilter(19)).filter(ImageFilter.GaussianBlur(1.2))
    mask.thumbnail((200, 200))
    edge = edge.resize(mask.size, Image.LANCZOS)
    for cn, col in COLORS.items():
        line = (245, 245, 245, 255) if cn == 'black' else (24, 24, 24, 255)
        im = Image.new('RGBA', mask.size, (0, 0, 0, 0))
        im.paste(Image.new('RGBA', mask.size, line), (0, 0), edge)
        im.paste(Image.new('RGBA', mask.size, col + (255,)), (0, 0), mask)
        save(im, 'piece/' + n + '-' + cn)
    # the territory marker: a disc of the color with the troop on it
    for cn, col in COLORS.items():
        s = 160
        im = Image.new('RGBA', (s, s), (0, 0, 0, 0))
        dr = ImageDraw.Draw(im)
        dr.ellipse((2, 2, s - 3, s - 3), fill=col + (255,), outline=(230, 200, 120, 255), width=8)
        if n == 'troop':
            k = mask.copy()
            k.thumbnail((100, 100))
            light = (245, 238, 220, 255)
            im.paste(Image.new('RGBA', k.size, light), ((s - k.width) // 2, (s - k.height) // 2), k)
            save(im, 'piece/territory-' + cn)

# the rings that mark the choices on the map
for n, col, w in [('select', (255, 236, 150, 255), 10), ('target', (255, 90, 70, 255), 12)]:
    s = 200
    im = Image.new('RGBA', (s, s), (0, 0, 0, 0))
    dr = ImageDraw.Draw(im)
    dr.ellipse((w, w, s - w - 1, s - w - 1), outline=col, width=w)
    save(im.filter(ImageFilter.GaussianBlur(1)), n)
