#!/usr/bin/env python3
# The Commander cards and the tokens of the units they bring (webp2/tactics/images/commander/, token/), cut from the 2nd Edition
# Faction Guide (the PDF in the Tabletop Simulator mod "Company of Heroes - 2nd Edition", Steam workshop 2314664989): its pages 11
# (US core Commanders) and 17 (Wehrmacht), 12 and 13 (the US expansion Commanders), 18 and 19 (the Wehrmacht's), 24 and 25 (the British), 30 and 31 (the Soviets), rendered at 220 dpi with `pdftoppm -f 11 -l 11 -r 220 -png guide.pdf <dir>/hi` (and 17;
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
# the Wehrmacht expansion Commanders: pages 18 and 19 (printed 17 and 18)
LAYOUT['hi-18'] = ([(0.112, 0.371), (0.397, 0.656), (0.683, 0.943)], [(0.043, 0.475), (0.524, 0.955)])
LAYOUT['hi-19'] = ([(0.114, 0.373), (0.399, 0.658), (0.686, 0.946)], [(0.045, 0.477), (0.533, 0.947)])
# the British expansion Commanders: pages 24 and 25 (printed 23 and 24)
LAYOUT['hi-24'] = ([(0.109, 0.369), (0.397, 0.656), (0.685, 0.945)], [(0.045, 0.477), (0.522, 0.954)])
LAYOUT['hi-25'] = ([(0.107, 0.367), (0.397, 0.656), (0.685, 0.945)], [(0.045, 0.477), (0.531, 0.945)])
# the Soviet expansion Commanders: pages 30 and 31 (printed 29 and 30)
LAYOUT['hi-30'] = LAYOUT['hi-31'] = ([(0.076, 0.336), (0.37, 0.63), (0.664, 0.923)], [(0.045, 0.477), (0.522, 0.955)])
EXPANSION = ('hi-12', 'hi-13', 'hi-18', 'hi-19', 'hi-24', 'hi-25', 'hi-30', 'hi-31')

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
    ('hi-18', 0, 0): 'stug-assault', ('hi-18', 0, 1): 'stug-tactics',
    ('hi-18', 1, 0): 'terror', ('hi-18', 1, 1): 'tank-destroyer', ('hi-18', 1, 2): 'assault-support',
    ('hi-19', 0, 0): 'infantry-storm', ('hi-19', 0, 1): 'festung-support', ('hi-19', 0, 2): 'lightning-war',
    ('hi-24', 0, 0): 'hammer', ('hi-24', 0, 1): 'royal-artillery', ('hi-24', 0, 2): 'royal-armored',
    ('hi-24', 1, 0): 'tank-hunter', ('hi-24', 1, 1): 'special-service',
    ('hi-25', 0, 0): 'assault-infantry', ('hi-25', 0, 1): 'assault-officer', ('hi-25', 0, 2): 'vanguard',
    ('hi-30', 0, 0): 'frontal-assault', ('hi-30', 0, 1): 'guards-motor', ('hi-30', 0, 2): 'urban-defense',
    ('hi-30', 1, 0): 'urban-assault-tactics', ('hi-30', 1, 1): 'counterattack', ('hi-30', 1, 2): 'stalingrad',
    ('hi-31', 0, 0): 'siege-militia', ('hi-31', 0, 1): 'frontline', ('hi-31', 0, 2): 'spetsnaz',
    ('hi-31', 1, 0): 'terror-tactics', ('hi-31', 1, 1): 'shock-rifle', ('hi-31', 1, 2): 'soviet-industry',
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
    ('stug', 'stug-assault', 1, 'R', 'wehrmacht'),
    ('king-tiger', 'terror', 3, 'R', 'wehrmacht'),
    ('jaeger-sniper', 'tank-destroyer', 1, 'G', 'wehrmacht'),
    ('jagdpanther', 'tank-destroyer', 3, 'R', 'wehrmacht'),
    ('elefant', 'assault-support', 3, 'R', 'wehrmacht'),
    ('sdkfz251', 'infantry-storm', 1, 'Y', 'wehrmacht'),
    ('sexton', 'hammer', 1, 'R', 'british'),
    ('comet', 'hammer', 3, 'R', 'british'),
    ('firefly', 'tank-hunter', 3, 'R', 'british'),
    ('valentine', 'assault-infantry', 1, 'R', 'british'),
    ('commandos', 'special-service', 0, 'G', 'british'),
    ('shock-troops', 'frontal-assault', 1, 'G', 'soviet'),
    ('is2', 'frontal-assault', 2, 'R', 'soviet'),
    ('guards-rifle', 'guards-motor', 1, 'G', 'soviet'),
    ('ai-partisans', 'urban-defense', 1, 'G', 'soviet'),
    ('at-partisans', 'urban-assault-tactics', 2, 'G', 'soviet'),
    ('forward-station', 'urban-defense', 2, 'P', 'soviet'),
    ('kv2', 'urban-defense', 3, 'R', 'soviet'),
    ('penal', 'counterattack', 0, 'G', 'soviet'),
    ('isu152', 'counterattack', 2, 'R', 'soviet'),
    ('kv8', 'terror-tactics', 2, 'R', 'soviet'),
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
    trimmed = {cid: c.crop((30, -24, 30 + 1016, -24 + 620)) for (page, _, _), cid in CARDS.items() if page in EXPANSION for c in [cards[cid]]}
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
