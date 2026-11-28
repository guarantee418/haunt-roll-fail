#!/usr/bin/env python3
# The Deutsches Afrikakorps' twelve Commander cards (8 core, 4 Desert Warfare) and the tokens of their units, from the pictures of
# Bad Crow's "3rd printing balance changes" sheet (a Google Doc the owner shared; its pictures 19 to 30, 1500x900 each).
#
#   dak-commander-art.py <dir> <out>   <dir> holds the pictures as 19.img ... 30.img; <out> is webp2/tactics/images
#   dak-commander-art.py preview <dir> <file>   a sheet of the token crops, to check them
#
# Each card is cut to its frame and saved as commander/<id>.webp (1016 wide, like the Faction Guide's); the tokens come from the
# portraits on the cards' panels (panel 2, 3, 4 at x 485, 805, 1130) in tactics-art.py's ring of the DAK colour.

import sys, os, importlib.util
from PIL import Image

here = os.path.dirname(os.path.abspath(__file__))
spec = importlib.util.spec_from_file_location('art', os.path.join(here, 'tactics-art.py'))
art = importlib.util.module_from_spec(spec)
spec.loader.exec_module(art)

DAK = art.COLORS['dak']

CARDS = {19: 'dak-armored-warfare', 20: 'dak-armored-airpower', 21: 'dak-combined-armor', 22: 'dak-combined-arms', 23: 'dak-field-engineers',
    24: 'dak-defensive-warfare', 25: 'dak-desert-skorpion', 26: 'dak-coastal', 27: 'dak-light-support', 28: 'dak-desert-raid',
    29: 'dak-assault-support', 30: 'dak-tank-assault'}

PANEL = {2: 485, 3: 805, 4: 1130}

# the token, the card and panel its portrait is on (or a box on the card), its kind
TOKENS = [
    ('flammpanzer-iii', 19, 2, 'R'),
    ('kradschutzen', 21, 2, 'Y'),
    ('semovente', 21, 3, 'R'),
    ('bersaglieri', 22, 2, 'G'),
    ('howitzer105', 22, 4, 'Y'),
    ('guastatori', 23, 3, 'G'),
    ('artillery21022', 26, 4, 'P'),
    ('l640', 27, 2, 'R'),
    ('elefant', 29, 4, 'R'),
    # the Panzerpioneer has only the small portrait at the bottom left of its card's first panel
    ('panzerpioneer', 24, (150, 330, 236, 416), 'G'),
    # the Kradschützen's dismounted rider: the Bersaglieri's portrait
    ('krad-scout', 22, 3, 'G'),
]

def crop(img, where):
    if isinstance(where, tuple):
        return img.crop(where)
    x = PANEL[where]
    return img.crop((x, 115, x + 225, 340))

def make(d, out):
    os.makedirs(os.path.join(out, 'commander'), exist_ok=True)
    for n, cid in CARDS.items():
        img = Image.open(os.path.join(d, '%d.img' % n)).convert('RGB').crop((75, 65, 1425, 830))
        img = img.resize((1016, int(765 * 1016 / 1350)), Image.LANCZOS)
        art.webp(img, os.path.join(out, 'commander', cid + '.webp'), q=85)
    for name, n, where, kind in TOKENS:
        img = Image.open(os.path.join(d, '%d.img' % n)).convert('RGB')
        art.webp(art.token(crop(img, where), kind, DAK), os.path.join(out, 'token', name + '-dak.webp'), q=90)
    # the MG Bunker the Panzerpioneers build: the Wehrmacht's token in the DAK ring
    t = Image.open(os.path.join(out, 'token', 'mg-bunker-wehrmacht.webp')).convert('RGBA')
    px = t.load()
    wm = art.COLORS['wehrmacht']
    for y in range(t.size[1]):
        for x in range(t.size[0]):
            r, g, b, a = px[x, y]
            if a > 0 and abs(r - wm[0]) + abs(g - wm[1]) + abs(b - wm[2]) < 90:
                px[x, y] = DAK + (a,)
    art.webp(t, os.path.join(out, 'token', 'mg-bunker-dak.webp'), q=90)

def preview(d, file):
    sheet = Image.new('RGBA', (160 * len(TOKENS), 160), (40, 40, 40, 255))
    for i, (name, n, where, kind) in enumerate(TOKENS):
        img = Image.open(os.path.join(d, '%d.img' % n)).convert('RGB')
        sheet.alpha_composite(art.token(crop(img, where), kind, DAK), (160 * i, 0))
    sheet.save(file)

if __name__ == '__main__':
    if sys.argv[1] == 'preview':
        preview(sys.argv[2], sys.argv[3])
    else:
        make(sys.argv[1], sys.argv[2])
