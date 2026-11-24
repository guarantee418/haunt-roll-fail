#!/usr/bin/env python3
# The Commander cards and the tokens of the units they bring (webp2/tactics/images/commander/, token/), cut from the 2nd Edition
# Faction Guide (the PDF in the Tabletop Simulator mod "Company of Heroes - 2nd Edition", Steam workshop 2314664989): its pages 11
# (US core Commanders) and 17 (Wehrmacht), 12 and 13 (the US expansion Commanders), rendered at 220 dpi with `pdftoppm -f 11 -l 11 -r 220 -png guide.pdf <dir>/hi` (and 17;
# 23, 29 and 36 for the British, Soviet and OKW, saved as hi-23.png and so on),
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
# the British, Soviet and OKW pages place their cards a little lower and narrower
LAYOUT = {'hi-23': ([(0.124, 0.349), (0.409, 0.634), (0.694, 0.919)], [(0.064, 0.459), (0.539, 0.934)])}
LAYOUT['hi-29'] = LAYOUT['hi-36'] = LAYOUT['hi-23']
# the US expansion Commanders: the PDF's pages 12 and 13 (printed 11 and 12; page 13's right column is the Lost Brigades extensions)
LAYOUT['hi-12'] = ([(0.11, 0.369), (0.4, 0.659), (0.688, 0.947)], [(0.045, 0.477), (0.522, 0.954)])
LAYOUT['hi-13'] = ([(0.114, 0.374), (0.4, 0.659), (0.688, 0.947)], [(0.045, 0.477), (0.531, 0.945)])

# page, column, row -> the Commander's id (as `Commander.id` in game.scala)
CARDS = {
    ('hi-11', 0, 0): 'recon-support', ('hi-11', 0, 1): 'ranger', ('hi-11', 0, 2): 'armor-company',
    ('hi-11', 1, 0): 'mechanized-company', ('hi-11', 1, 1): 'airborne', ('hi-11', 1, 2): 'mobile-support',
    ('hi-17', 0, 0): 'jaeger', ('hi-17', 0, 1): 'spearhead', ('hi-17', 0, 2): 'mechanized-doctrine',
    ('hi-17', 1, 0): 'festung', ('hi-17', 1, 1): 'panzerkorps', ('hi-17', 1, 2): 'elite-troops',
    # the British, Soviet and OKW core Commanders: the PDF's pages 23, 29 and 36 (the Faction Guide's printed 22, 28 and 35)
    ('hi-23', 0, 0): 'advanced-emplacements', ('hi-23', 0, 1): 'commando', ('hi-23', 0, 2): 'lend-lease',
    ('hi-23', 1, 0): 'royal-engineer', ('hi-23', 1, 1): 'special-weapons', ('hi-23', 1, 2): 'infantry-support',
    ('hi-29', 0, 0): 'assault-tactics', ('hi-29', 0, 1): 'elite-artillery', ('hi-29', 0, 2): 'nkvd',
    ('hi-29', 1, 0): 'ranged-tactics', ('hi-29', 1, 1): 'conscript-support', ('hi-29', 1, 2): 'armored-assault',
    ('hi-36', 0, 0): 'breakthrough', ('hi-36', 0, 1): 'elite-armored', ('hi-36', 0, 2): 'firestorm',
    ('hi-36', 1, 0): 'scavenge', ('hi-36', 1, 1): 'fortifications', ('hi-36', 1, 2): 'grand-offense',
    ('hi-12', 0, 0): 'light-cavalry', ('hi-12', 0, 1): 'heavy-cavalry', ('hi-12', 0, 2): 'tactical-support',
    ('hi-12', 1, 0): 'motor-pool', ('hi-12', 1, 1): 'pathfinder', ('hi-12', 1, 2): 'devils-brigade',
    ('hi-13', 0, 0): 'cavalry-rifle', ('hi-13', 0, 1): 'urban-assault', ('hi-13', 0, 2): 'engineer-assault',
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
    ('royal-engineers', 'royal-engineer', 0, 'G', 'british'),
    ('forward-emplacement', 'advanced-emplacements', 1, 'P', 'british'),
    ('seventeen-pounder', 'advanced-emplacements', 3, 'P', 'british'),
    ('churchill-avre', 'royal-engineer', 2, 'R', 'british'),
    ('staghound', 'commando', 2, 'Y', 'british'),
    ('ap-sniper', 'special-weapons', 0, 'G', 'british'),
    ('dshk', 'ranged-tactics', 1, 'G', 'soviet'),
    ('t3485', 'conscript-support', 3, 'R', 'soviet'),
    ('forward-hq', 'nkvd', 1, 'P', 'soviet'),
    ('panzerfusilier', 'breakthrough', 1, 'G', 'okw'),
    ('jaeger-recon', 'scavenge', 1, 'G', 'okw'),
    ('puma', 'scavenge', 2, 'Y', 'okw'),
    ('lefh18', 'fortifications', 1, 'P', 'okw'),
    ('okw-pak43', 'fortifications', 3, 'P', 'okw'),
    ('kettenkrad', 'firestorm', 1, 'Y', 'okw'),
    ('stg44-volks', 'grand-offense', 0, 'G', 'okw'),
    ('stuart', 'light-cavalry', 1, 'R', 'us'),
    ('pershing', 'heavy-cavalry', 3, 'R', 'us'),
    ('calliope', 'tactical-support', 3, 'R', 'us'),
    ('jackson', 'urban-assault', 2, 'R', 'us'),
    ('dozer', 'cavalry-rifle', 3, 'R', 'us'),
    ('pathfinder', 'pathfinder', 0, 'G', 'us'),
    ('cavalry-riflemen', 'cavalry-rifle', 1, 'G', 'us'),
    ('devils-brigade', 'devils-brigade', 0, 'G', 'us'),
]

def main(d, out):
    os.makedirs(os.path.join(out, 'commander'), exist_ok=True)
    os.makedirs(os.path.join(out, 'token'), exist_ok=True)
    cards = {}
    for (page, c, r), cid in CARDS.items():
        im = Image.open(os.path.join(d, page + '.png')).convert('RGB')
        w, h = im.size
        rows, cols = LAYOUT.get(page, (ROWS, COLS))
        (x0, x1), (y0, y1) = cols[c], rows[r]
        card = im.crop((int(w * x0), int(h * y0), int(w * x1), int(h * y1)))
        cards[cid] = card
        art.webp(card, os.path.join(out, 'commander', cid + '.webp'), q=86)
    # the expansion pages' crops have a wider black margin: their portraits are taken from the card trimmed to the core pages' frame
    trimmed = {cid: c.crop((30, -24, 30 + 1016, -24 + 620)) for (page, _, _), cid in CARDS.items() if page in ('hi-12', 'hi-13') for c in [cards[cid]]}
    for tid, cid, panel, kind, f in UNITS:
        p = portrait(trimmed.get(cid, cards[cid]), panel)
        art.webp(art.token(p, kind, art.COLORS[f]), os.path.join(out, 'token', tid + '-' + f + '.webp'), q=90)
    # Assault Engineers and the MG Bunker: the Lost Brigades sheet (4 x 3 cards of 600 x 825), their cards' portraits
    sheet = Image.open(os.path.join(d, 'US-5420.png')).convert('RGB')
    def lb(col, row):
        return sheet.crop((col * 600 + 200, row * 825 + 75, col * 600 + 400, row * 825 + 275))
    art.webp(art.token(lb(0, 0), 'G', art.COLORS['us']), os.path.join(out, 'token', 'assault-engineers-us.webp'), q=90)
    art.webp(art.token(lb(0, 1), 'P', art.COLORS['wehrmacht']), os.path.join(out, 'token', 'mg-bunker-wehrmacht.webp'), q=90)
    # the US MG Bunker (Tactical Support Company): the same portrait in the US colour
    art.webp(art.token(lb(0, 1), 'P', art.COLORS['us']), os.path.join(out, 'token', 'mg-bunker-us.webp'), q=90)

if __name__ == '__main__':
    main(sys.argv[1], sys.argv[2])
