#!/usr/bin/env python3
# The Commander cards and the tokens of the units they bring (webp2/tactics/images/commander/, token/), cut from the 2nd Edition
# Faction Guide (the PDF in the Tabletop Simulator mod "Company of Heroes - 2nd Edition", Steam workshop 2314664989): its pages 11
# (US core Commanders) and 17 (Wehrmacht), rendered at 220 dpi with `pdftoppm -f 11 -l 11 -r 220 -png guide.pdf <dir>/hi` (and 17),
# and the mod's US and Wehrmacht Lost Brigades sheets for two portraits (Assault Engineers, MG Bunker; `tactics-art.py fetch` names).
#
#   commander-art.py <dir with hi-11.png, hi-17.png, US-5420.png> <webp2/tactics/images>

import sys, os, importlib.util
from PIL import Image

spec = importlib.util.spec_from_file_location('art', os.path.join(os.path.dirname(__file__), 'tactics-art.py'))
art = importlib.util.module_from_spec(spec)
spec.loader.exec_module(art)

# the six cards of a page: two columns, three rows (fractions of the page)
ROWS = [(0.105, 0.36), (0.385, 0.64), (0.665, 0.925)]
COLS = [(0.06, 0.48), (0.53, 0.95)]

# page, column, row -> the Commander's id (as `Commander.id` in game.scala)
CARDS = {
    ('hi-11', 0, 0): 'recon-support', ('hi-11', 0, 1): 'ranger', ('hi-11', 0, 2): 'armor-company',
    ('hi-11', 1, 0): 'mechanized-company', ('hi-11', 1, 1): 'airborne', ('hi-11', 1, 2): 'mobile-support',
    ('hi-17', 0, 0): 'jaeger', ('hi-17', 0, 1): 'spearhead', ('hi-17', 0, 2): 'mechanized-doctrine',
    ('hi-17', 1, 0): 'festung', ('hi-17', 1, 1): 'panzerkorps', ('hi-17', 1, 2): 'elite-troops',
}

# a panel's portrait in a card crop (about 1016 x 620): panel 0 to 3 from the left
def portrait(card, panel, top=0.2):
    w, h = card.size
    x0 = w * (0.085 + panel * 0.218)
    side = w * 0.15
    y0 = h * top
    return card.crop((int(x0), int(y0), int(x0 + side), int(y0 + side)))

# the units the Commanders bring: (token id, card id, panel, kind, faction)
UNITS = [
    ('priest', 'recon-support', 2, 'R', 'us'),
    ('easy-eight', 'armor-company', 3, 'R', 'us'),
    ('tiger', 'spearhead', 3, 'R', 'wehrmacht'),
    ('osttruppen', 'festung', 1, 'G', 'wehrmacht'),
    ('pak43', 'festung', 2, 'P', 'wehrmacht'),
    ('stormtroopers', 'elite-troops', 1, 'G', 'wehrmacht'),
]

def main(d, out):
    os.makedirs(os.path.join(out, 'commander'), exist_ok=True)
    os.makedirs(os.path.join(out, 'token'), exist_ok=True)
    cards = {}
    for (page, c, r), cid in CARDS.items():
        im = Image.open(os.path.join(d, page + '.png')).convert('RGB')
        w, h = im.size
        (x0, x1), (y0, y1) = COLS[c], ROWS[r]
        card = im.crop((int(w * x0), int(h * y0), int(w * x1), int(h * y1)))
        cards[cid] = card
        art.webp(card, os.path.join(out, 'commander', cid + '.webp'), q=86)
    for tid, cid, panel, kind, f in UNITS:
        p = portrait(cards[cid], panel)
        art.webp(art.token(p, kind, art.COLORS[f]), os.path.join(out, 'token', tid + '-' + f + '.webp'), q=90)
    # Assault Engineers and the MG Bunker: the Lost Brigades sheet (4 x 3 cards of 600 x 825), their cards' portraits
    sheet = Image.open(os.path.join(d, 'US-5420.png')).convert('RGB')
    def lb(col, row):
        return sheet.crop((col * 600 + 200, row * 825 + 75, col * 600 + 400, row * 825 + 275))
    art.webp(art.token(lb(0, 0), 'G', art.COLORS['us']), os.path.join(out, 'token', 'assault-engineers-us.webp'), q=90)
    art.webp(art.token(lb(0, 1), 'P', art.COLORS['wehrmacht']), os.path.join(out, 'token', 'mg-bunker-wehrmacht.webp'), q=90)

if __name__ == '__main__':
    main(sys.argv[1], sys.argv[2])
