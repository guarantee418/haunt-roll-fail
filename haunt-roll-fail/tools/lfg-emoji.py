#!/usr/bin/env python3
# The Discord LFG posts' icons (good-game/Lfg.scala, LfgIcons; asked for on 2026-11-21: "use icons/emojis as much as possible"):
# each setup option that has a picture in the game's art gets a 128x128 PNG, webp2/lfg/emoji/<slug>.png, which the server uploads
# as the bot's own application emoji hrf_<slug> when the Discord server has no emoji of its own for it; webp2/lfg/emoji/icons.tsv
# says which option (its saved name, as the client's catalog writes it) shows which icon.
# Run from haunt-roll-fail/: python3 tools/lfg-emoji.py (the ribbons' font is DejaVu Serif Bold)
import os, sys
from PIL import Image, ImageDraw, ImageFont, ImageFilter, ImageChops

W = 'webp2'
OUT = os.path.join(W, 'lfg', 'emoji')
R = W + '/root/images/'
N = W + '/nort/images/'
SIZE = 128

def load(p):
    return Image.open(p).convert('RGBA')

def trim(im):
    b = im.getchannel('A').point(lambda a: 255 if a > 16 else 0).getbbox()
    return im.crop(b) if b else im

def square(im):
    w, h = im.size
    s = max(w, h)
    out = Image.new('RGBA', (s, s), (0, 0, 0, 0))
    out.paste(im, ((s - w) // 2, (s - h) // 2))
    return out.resize((SIZE, SIZE), Image.LANCZOS)

# the whole picture, its transparent edges trimmed, fitted in the square
def fit(p):
    return square(trim(load(p)))

# a box of the picture, in fractions of its width and height, fitted in the square
def box(p, x0, y0, x1, y1):
    im = load(p)
    w, h = im.size
    return square(im.crop((int(x0 * w), int(y0 * h), int(x1 * w), int(y1 * h))))

# the square at the top of a portrait card (its art), a little inside its edges
def top(p, inset=0.04):
    im = trim(load(p))
    w, h = im.size
    m = int(w * inset)
    s = w - 2 * m
    return square(im.crop((m, m, m + s, m + s)))

# the middle of a picture: the square of `frac` of its smaller side
def center(p, frac=0.6):
    im = load(p)
    w, h = im.size
    s = int(min(w, h) * frac)
    return square(im.crop(((w - s) // 2, (h - s) // 2, (w + s) // 2, (h + s) // 2)))

# THE SERVER'S STYLE (its 50 emojis, seen on 2026-11-21): Root's factions as their heads alone, maps as a square of the map, decks
# as a card tilted in a white border, and Northgard's things in a round badge with a gold ring (the clans' emblems), with a ribbon
# naming it where the server's WILD and WASTE badges have one

def rounded(im, r):
    m = Image.new('L', im.size, 0)
    ImageDraw.Draw(m).rounded_rectangle((0, 0, im.size[0] - 1, im.size[1] - 1), r, fill=255)
    out = Image.new('RGBA', im.size, (0, 0, 0, 0))
    out.paste(im, (0, 0), ImageChops.multiply(m, im.getchannel('A')))
    return out

# a map: a square of its middle, its corners a little round
def map_tile(p, frac=0.82):
    im = load(p)
    w, h = im.size
    s = int(min(w, h) * frac)
    im = im.crop(((w - s) // 2, (h - s) // 2, (w + s) // 2, (h + s) // 2)).resize((SIZE - 8, SIZE - 8), Image.LANCZOS)
    out = Image.new('RGBA', (SIZE, SIZE), (0, 0, 0, 0))
    out.paste(rounded(im, 8), (4, 4))
    return out

# a deck: its card in a white border, tilted, with a soft shadow
def deck_card(im, angle=-32):
    im = trim(im)
    w, h = im.size
    k = 300 / h
    im = im.resize((int(w * k), 300), Image.LANCZOS)
    b = 14
    card = Image.new('RGBA', (im.size[0] + 2 * b, im.size[1] + 2 * b), (255, 255, 255, 255))
    card.paste(im, (b, b), im)
    card = rounded(card, 10)
    t = card.rotate(angle, expand=True, resample=Image.BICUBIC)
    sh = Image.new('RGBA', t.size, (0, 0, 0, 0))
    sh.putalpha(t.getchannel('A').point(lambda a: a * 0.45).filter(ImageFilter.GaussianBlur(8)))
    out = Image.new('RGBA', (t.size[0] + 12, t.size[1] + 12), (0, 0, 0, 0))
    out.paste(sh, (10, 10), sh)
    out.paste(t, (0, 0), t)
    return square(trim(out))

FONT = '/usr/share/fonts/truetype/dejavu/DejaVuSerif-Bold.ttf'
GOLD = (226, 180, 82, 255)
SLATE = (36, 54, 72, 255)
GREEN = (46, 107, 46, 255)
BROWN = (122, 59, 30, 255)

# a Northgard badge: the art in a circle with a gold ring, and its ribbon
def badge(art, label=None, ribbon=SLATE, fill=True):
    S = 512
    out = Image.new('RGBA', (S, S), (0, 0, 0, 0))
    d = ImageDraw.Draw(out)
    r0 = 20
    d.ellipse((r0, r0, S - r0, S - r0), fill=SLATE)
    inner = S - 2 * (r0 + 22)
    a = trim(art)
    if fill:
        w, h = a.size
        m = min(w, h)
        a = a.crop(((w - m) // 2, (h - m) // 2, (w + m) // 2, (h + m) // 2)).resize((inner, inner), Image.LANCZOS)
    else:
        a = square(a).resize((int(inner * 0.86), int(inner * 0.86)), Image.LANCZOS)
    mask = Image.new('L', (inner, inner), 0)
    ImageDraw.Draw(mask).ellipse((0, 0, inner - 1, inner - 1), fill=255)
    layer = Image.new('RGBA', (inner, inner), (0, 0, 0, 0))
    layer.paste(a, ((inner - a.size[0]) // 2, (inner - a.size[1]) // 2), a)
    layer.putalpha(ImageChops.multiply(layer.getchannel('A'), mask))
    out.paste(layer, (r0 + 22, r0 + 22), layer)
    d.ellipse((r0, r0, S - r0, S - r0), outline=GOLD, width=22)
    if label:
        f = 116
        while True:
            font = ImageFont.truetype(FONT, f)
            tw = d.textbbox((0, 0), label, font=font)
            if tw[2] - tw[0] <= S - 70 or f <= 40:
                break
            f -= 4
        th = 132
        y = S - th - 4
        d.rounded_rectangle((14, y, S - 14, y + th), 14, fill=ribbon, outline=GOLD, width=8)
        bb = d.textbbox((0, 0), label, font=font)
        d.text(((S - (bb[2] - bb[0])) // 2 - bb[0], y + (th - (bb[3] - bb[1])) // 2 - bb[1]), label, font=font, fill=(255, 255, 255, 255))
    return out.resize((SIZE, SIZE), Image.LANCZOS)

icons = {}   # slug -> image
keys = []    # (option, slug)

def icon(slug, im, *options):
    if slug not in icons:
        icons[slug] = im
    for o in options:
        keys.append((o, slug))

# ROOT: the factions by their heads (the server's style), and the tweaks of each faction's section by its head
glyphs = {'MC': 'feline/mc', 'ED': 'aviary/ed', 'WA': 'insurgent/wa', 'VB': 'hero/vb', 'RF': 'trader/rf', 'LC': 'fanatic/lc',
          'UD': 'underground/ud', 'CC': 'mischief/cc', 'LH': 'horde/lh', 'KI': 'expedition/ki', 'KD': 'abduct/kd', 'TC': 'legal/tc',
          'NB': 'hero/nb', 'LDvE': 'invasive/hld-glyph-meeple',
          'AA': 'arachnid/aa', 'BP': 'bone/bp', 'CUv2': 'utopia/cu', 'DM': 'marquistadors/dm', 'DU': 'doomsayers/du',
          'DV': 'hero/mb', 'EE': 'aviary/ee', 'FH': 'farmer/fh', 'NW': 'abduct/nw', 'NX': 'battery/nx', 'OF': 'order/of',
          'OMT': 'tinker/omt', 'TW': 'colonies/tw', 'UDC': 'dove/udc', 'WG': 'whisperers/wg', 'WM': 'menace/wm', 'WW': 'wake/ww',
          'XC': 'caster/xc'}
short = {'LDvE': 'ld'}
for f, g in glyphs.items():
    path = R + 'faction/' + g + ('.webp' if g.endswith('meeple') else '-glyph.webp')
    icon('rt_' + short.get(f, f.lower()), fit(path), 'IncludeFaction(' + f + ')')
for group, s in [('Marquise de Cat', 'mc'), ('Vagabond', 'vb'), ('The Lizard Cult', 'lc'), ('Underground Duchy', 'ud'),
                 ('Corvid Conspiracy', 'cc'), ('Lord of the Hundreds', 'lh'), ('Keepers in Iron', 'ki'), ('Riverfolk Company', 'rf'),
                 ('Knaves of the Deepwood', 'kd')]:
    keys.append(('#' + group, 'rt_' + s))

# the maps, as a square of the map
for o, d, f in [('AutumnMap', 'autumn', 'map-bright-new-expand'), ('WinterMap', 'winter', 'map-bright'), ('LakeMap', 'lake', 'map-bright'),
                ('MountainMap', 'mountain', 'map-bright'), ('GorgeMap', 'gorge', 'map-art'), ('MarshMap', 'marsh', 'map-art'),
                ('TidalMap', 'tidal', 'map-bright'), ('TundraMap', 'tundra', 'map-bright'), ('GloomMap', 'gloom', 'map-bright'),
                ('UrbanMap', 'urban', 'map-art'), ('LegendsMap', 'legends-map', 'map-art'), ('DeepWoodsMap', 'deep-woods', 'map-art'),
                ('WastelandsMap', 'wastelands', 'map-art')]:
    icon('rm_' + d.replace('-', ''), map_tile(R + d + '/' + f + '.webp'), o)

# the decks, as a card of theirs tilted in a white border (their back, else a card)
first = lambda d: R + 'card/' + d + '/' + sorted(x for x in os.listdir(R + 'card/' + d) if x.endswith('.webp'))[0]
for o, s_, p in [('StandardDeck', 'base', R + 'card/card-back-art.webp'), ('ExilesDeck', 'ep', R + 'card/card-back-art-ep.webp'),
                 ('SquiresDeck', 'sd', R + 'card/card-back-art-sd.webp'), ('DarkDeck', 'dd', R + 'card/card-back-art-dd.webp'),
                 ('FrankenDeck', 'fd', R + 'card/card-back-art-fd.webp'), ('SorceryDeck', 'se', R + 'card/card-back-art-se.webp'),
                 ('UpstartsDeck', 'ur', R + 'card/card-back-art-ur.webp'), ('CapitalistsDeck', 'ca', first('capitalists')),
                 ('DuskDeck', 'dusk', R + 'card/dusk/dusk-awakening.webp'), ('ActionBooster', 'action', first('action'))]:
    icon('rd_' + s_, deck_card(load(p)), o)

# the landmarks
for o, f in [('FerryLandmark', 'ferry'), ('FoxburrowLandmark', 'foxburrow'), ('LostCityLandmark', 'lost-city'),
             ('MouseholdLandmark', 'mousehold'), ('RabbittownLandmark', 'rabbittown'), ('TowerLandmark', 'tower'),
             ('BirdsongTowerLandmark', 'tower'), ('RoyalTreeLandmark', 'royal-tree'), ('SimpleForgeLandmark', 'simple-forge'),
             ('SimpleMarketLandmark', 'simple-market')]:
    icon('rl_' + f.replace('-', ''), fit(R + 'feature/' + f + '.webp'), o)

# the hirelings: the pieces of their card (both sides of a card share them)
H = R + 'faction/hireling/'
pieces = {
    'stb-weasel': ['StreetBand', 'PopularBand'], 'stp-deer': ['StoicProtector'], 'fpr-elk': ['FuriousProtector'],
    'bdg-porcupine': ['BanditGangs'], 'hwb-bandit': ['HighwayBandits'], 'frp-stray-cat': ['ForestPatrol', 'FelinePhysicians'],
    'ldn-vulture': ['LastDynasty', 'BluebirdNobles'], 'sup-animal': ['SpringUprising', 'RabbitScouts'], 'exl-bear': ['TheExile', 'Brigand'],
    'wsp-chameleon': ['WarmSunProphets', 'LizardEnvoys'], 'rff-flotilla': ['RiverfolkFlotilla', 'OtterDivers'],
    'swe-mouldwarp': ['SunwardExpedition', 'MoleArtisians'], 'csp-magpie': ['CorvidSpies', 'RavenSentries'],
    'fmb-varmint': ['FlameBearers', 'RatSmugglers'], 'vlk-vole': ['VaultKeepers', 'BadgerBodyguards'],
    'sad-advocate': ['SunnyAdvocates', 'BatMessengers'], 'rvr-roamer': ['RiverRoamers', 'FrogTinkers'],
    'pfr-farmer': ['ProsperousFarmers'], 'sfr-farmer': ['StrugglingFarmers'],
    'turnling-whisperer': ['MidnightMurderers', 'ForestConnections'], 'ddi-infected': ['TheInfected', 'InfectiousSpirits'],
    'turnling-cardinal': ['RougeAgents'], 'turnling-caravan': ['CardinalCarts'], 'turnling-doomsayer': ['UnionMembers'],
    'turnling-flame': ['CorporateOverreach'], 'turnling-vulture': ['ConvergingCoup', 'FeederTurnling'],
    'turnling-bug': ['BuzzlingBuggers', 'MiracleCureAll'], 'turnling-grunt': ['GristlyGrunts'], 'turnling-swine': ['FineSwine', 'GoldenPig'],
}
for p, hh in pieces.items():
    icon('rh_' + p.split('-')[-1] if p.startswith('turnling') else 'rh_' + p[:3], fit(H + p + '.webp'), *['IncludeHireling(' + h + ')' for h in hh])
icon('rh_knight', fit(R + 'feature/dark-knight-token.webp'), 'IncludeHireling(KnightTurnling)')

# the Legendary Monsters
for o, m in [('PickBeaverbot', 'beaverbot'), ('PickDemonBear', 'demon-bear'), ('PickRedKnight', 'red-knight'),
             ('PickShadowmaw', 'shadowmaw'), ('PickWhiteWolf', 'white-wolf')]:
    icon('rx_' + m.replace('-', ''), fit(R + 'legends/monster-' + m + '.webp'), o)

# NORTHGARD: the modules and expansions in badges, like the server's clans and its WILD and WASTE
B = lambda p, label=None, ribbon=SLATE, fill=True: badge(load(p), label, ribbon, fill)
icon('ng_creatures', B(N + 'token/creature/wolf-1.webp', 'CREATURES'), 'ModuleOption(Creatures)')
icon('ng_morecreatures', B(N + 'token/creature/draugr-1.webp', 'MORE'), 'MoreCreatures')
icon('ng_wilderness', B(N + 'tile/wild-den.webp', 'WILD', GREEN), 'WildernessTiles', 'WildPartsTiles', 'LandsWilderness')
icon('ng_wildcreatures', B(N + 'token/creature/fallen-valkyrie-1.webp', 'WILD', GREEN), 'WildernessCreatures', 'WildPartsCreatures', 'WildPartsBoth')
icon('ng_wastelands', B(N + 'tile/waste-vedrfolnir.webp', 'WASTE', BROWN), 'WastelandsTiles', 'WastePartsTiles', 'LandsWastelands')
icon('ng_wastecreatures', B(N + 'token/creature/hrimgandr-1.webp', 'WASTE', BROWN), 'WastelandsCreatures', 'WastePartsCreatures', 'WastePartsBoth')
icon('ng_warchief', B(N + 'token/unit/chief-wolf-blue.webp', 'CHIEFS', fill=False), 'ModuleOption(Warchiefs)', 'WarchiefPawns')
icon('ng_warchiefcard', B(N + 'card/back/clan.webp', 'CARDS'), 'WarchiefCards')
icon('ng_sea', B(N + 'ui/ship.webp', 'SEA', fill=False), 'ModuleOption(Sea)')
icon('ng_events', B(N + 'card/event/blizzard.webp', 'EVENTS'), 'ModuleOption(EventsModule)')
icon('ng_horizons', B(N + 'tile/horizon-bridge.webp', 'HORIZON'), 'HorizonsTiles')
icon('ng_devs', B(N + 'card/back/development.webp', 'CARDS'), 'HorizonsDevelopments', 'ExtraDevelopment', 'FewerDevelopments',
     'NoDrawDevelopments', 'NoDefensiveStrategy')
icon('ng_achievement', B(N + 'card/back/achievement.webp', 'ACHIEVE'), 'ExtraAchievement')
icon('ng_balanced', B(N + 'tile/start.webp', 'START'), 'BalancedStartTiles', 'StandardCentral')
icon('ng_aggressive', B(N + 'token/creature/eldthurs-1.webp', 'FIERCE'), 'ModuleOption(AggressiveCreatures)')
icon('ng_fameup', B(N + 'ui/fame.webp', 'FAME', fill=False), 'ModuleOption(FameUpgradesModule)', 'PublicFame')
icon('ng_military', B(N + 'ui/mxp.webp', 'MXP', fill=False), 'ModuleOption(MilitaryModule)')
icon('ng_minerals', B(N + 'ui/gem.webp', 'MINES', fill=False), 'ModuleOption(MineralsModule)')
icon('ng_pc', B(N + 'token/building/town-hall-upgraded.webp', 'PC', fill=False), 'ModuleOption(PCVersion)')
icon('ng_tower', B(N + 'token/building/defense-tower.webp', 'TOWER', fill=False), 'ModuleOption(TowersModule)')
icon('ng_chiefpc', B(N + 'token/unit/bodyguard-blue.webp', 'PC', fill=False), 'ModuleOption(WarchiefsPC)')
icon('ng_domination', B(N + 'token/building/town-hall.webp', 'DOMINATE', fill=False), 'DominationVictory', 'DominationOnly')
icon('ng_famegains', B(N + 'token/building/altar-of-kings.webp', 'FAME', fill=False), 'ModuleOption(FameGainsModule)', 'FameOnly')
icon('ng_standardwin', B(N + 'token/building/fortress.webp', 'VICTORY', fill=False), 'StandardVictory')
vic = lambda c: load(N + 'card/victory/' + c).crop
def victory(c, label):
    im = load(N + 'card/victory/' + c)
    w, h = im.size
    return badge(im.crop((int(0.06 * w), int(0.2 * h), int(0.94 * w), int(0.8 * h))), label)
icon('ng_altvictory', victory('conquest.webp', 'ALT WIN'), 'AltVictoryChosen', 'AltVictoryRandom')
for c in sorted(os.listdir(N + 'card/victory')):
    v = c[:-5]
    # (short, so the ribbon's text stays readable at emoji size)
    label = {'architecture': 'ARCHITECT', 'building-ownership': 'OWNERS', 'creature-territories': 'CREATURE', 'development': 'DEVELOP',
             'exploration': 'EXPLORE', 'knowledge': 'LORE', 'large-buildings': 'BUILDINGS', 'large-territories': 'LARGE',
             'many-territories': 'MANY', 'mountains': 'MOUNTAIN', 'population': 'PEOPLE', 'production': 'PRODUCE', 'prosperity': 'WEALTH',
             'refinement': 'REFINE', 'spreading': 'SPREAD', 'two-larger-territories': '2 LARGER', 'vast-territory': 'VAST'}.get(v, v.upper())
    icon('ng_v_' + v.replace('-', '')[:20], victory(c, label), 'VictoryCardOption(VictoryCard("' + v + '"))')
# the central tiles: their middles, named
for o, t, label in [('DenCentral', 'start-den', 'DEN'), ('GreatLakeCentral', 'start-lake', 'LAKE'), ('HelheimCentral', 'start-helheim', 'HELHEIM'),
                    ('HrimgandrCentral', 'start-hrimgandr', 'LAIR'), ('MagmaFlowCentral', 'start-magma', 'MAGMA'),
                    ('MimirCentral', 'start-mimir', 'MIMIR'), ('RelicCentral', 'start-relic', 'RELIC'), ('VolcanoCentral', 'start-volcano', 'VOLCANO'),
                    ('WildLakeCentral', 'wild-lake', 'LAKE'), ('YggdrasilCentral', 'start-yggdrasil', 'TREE')]:
    im = load(N + 'tile/' + t + '.webp')
    w, h = im.size
    m = int(min(w, h) * 0.5)
    icon('ng_c_' + t.split('-')[-1], badge(im.crop(((w - m) // 2, (h - m) // 2, (w + m) // 2, (h + m) // 2)), label,
         GREEN if t.startswith('wild') else BROWN if t != 'start-lake' else SLATE), o)

os.makedirs(OUT, exist_ok=True)
for s, im in icons.items():
    assert len('hrf_' + s) <= 32, s
    # a palette of 256 colours keeps each file a few KB (Discord takes up to 256 KB)
    im.quantize(256, method=Image.FASTOCTREE).save(os.path.join(OUT, s + '.png'), optimize=True)
with open(os.path.join(OUT, 'icons.tsv'), 'w') as f:
    for o, s in keys:
        f.write(o + '\t' + s + '\n')
print(len(icons), 'icons,', len(keys), 'options')
