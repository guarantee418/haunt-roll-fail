#!/usr/bin/env python3
# The Discord LFG posts' icons (good-game/Lfg.scala, LfgIcons; asked for on 2026-11-21: "use icons/emojis as much as possible"):
# each setup option that has a picture in the game's art gets a 128x128 PNG, webp2/lfg/emoji/<slug>.png, which the server uploads
# as the bot's own application emoji hrf_<slug> when the Discord server has no emoji of its own for it; webp2/lfg/emoji/icons.tsv
# says which option (its saved name, as the client's catalog writes it) shows which icon.
# Run from haunt-roll-fail/: python3 tools/lfg-emoji.py
import os, sys
from PIL import Image

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

icons = {}   # slug -> image
keys = []    # (option, slug)

def icon(slug, im, *options):
    if slug not in icons:
        icons[slug] = im
    for o in options:
        keys.append((o, slug))

# ROOT: the factions (their VP marker, else their glyph), and the tweaks of each faction's section by its icon
vp = {'MC': 'mc', 'ED': 'ed', 'WA': 'wa', 'VB': 'vb', 'RF': 'rf', 'LC': 'lc', 'UD': 'ud', 'CC': 'cc', 'LH': 'lh', 'KI': 'ki',
      'LDvE': 'ld', 'KD': 'kd', 'TC': 'tc', 'NB': 'nb'}
for f, s in vp.items():
    icon('rt_' + s, fit(R + 'vp/vp-' + s + '.webp'), 'IncludeFaction(' + f + ')')
glyphs = {'AA': 'arachnid/aa', 'BP': 'bone/bp', 'CUv2': 'utopia/cu', 'DM': 'marquistadors/dm', 'DU': 'doomsayers/du',
          'DV': 'hero/mb', 'EE': 'aviary/ee', 'FH': 'farmer/fh', 'NW': 'abduct/nw', 'NX': 'battery/nx', 'OF': 'order/of',
          'OMT': 'tinker/omt', 'TW': 'colonies/tw', 'UDC': 'dove/udc', 'WG': 'whisperers/wg', 'WM': 'menace/wm', 'WW': 'wake/ww',
          'XC': 'caster/xc'}
for f, g in glyphs.items():
    icon('rt_' + f.lower(), fit(R + 'faction/' + g + '-glyph.webp'), 'IncludeFaction(' + f + ')')
for group, s in [('Marquise de Cat', 'mc'), ('Vagabond', 'vb'), ('The Lizard Cult', 'lc'), ('Underground Duchy', 'ud'),
                 ('Corvid Conspiracy', 'cc'), ('Lord of the Hundreds', 'lh'), ('Keepers in Iron', 'ki'), ('Riverfolk Company', 'rf'),
                 ('Knaves of the Deepwood', 'kd')]:
    keys.append(('#' + group, 'rt_' + s))

# the maps, whole
for o, d, f in [('AutumnMap', 'autumn', 'map-bright-new-expand'), ('WinterMap', 'winter', 'map-bright'), ('LakeMap', 'lake', 'map-bright'),
                ('MountainMap', 'mountain', 'map-bright'), ('GorgeMap', 'gorge', 'map-art'), ('MarshMap', 'marsh', 'map-art'),
                ('TidalMap', 'tidal', 'map-bright'), ('TundraMap', 'tundra', 'map-bright'), ('GloomMap', 'gloom', 'map-bright'),
                ('UrbanMap', 'urban', 'map-art'), ('LegendsMap', 'legends-map', 'map-art'), ('DeepWoodsMap', 'deep-woods', 'map-art'),
                ('WastelandsMap', 'wastelands', 'map-art')]:
    icon('rm_' + d.replace('-', ''), fit(R + d + '/' + f + '.webp'), o)

# the decks: their card backs, else a card of theirs
for o, s, p in [('StandardDeck', 'base', 'card/card-back-art'), ('ExilesDeck', 'ep', 'card/card-back-art-ep'),
                ('SquiresDeck', 'sd', 'card/card-back-art-sd'), ('DarkDeck', 'dd', 'card/card-back-art-dd'),
                ('FrankenDeck', 'fd', 'card/card-back-art-fd'), ('SorceryDeck', 'se', 'card/card-back-art-se'),
                ('UpstartsDeck', 'ur', 'card/card-back-art-ur')]:
    icon('rd_' + s, top(R + p + '.webp'), o)
first = lambda d: R + 'card/' + d + '/' + sorted(x for x in os.listdir(R + 'card/' + d) if x.endswith('.webp'))[0]
icon('rd_ca', box(first('capitalists'), 0.08, 0.1, 0.92, 0.5), 'CapitalistsDeck')
icon('rd_dusk', box(R + 'card/dusk/dusk-awakening.webp', 0.08, 0.1, 0.92, 0.5), 'DuskDeck')
icon('rd_action', box(first('action'), 0.08, 0.1, 0.92, 0.5), 'ActionBooster')

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

# NORTHGARD: the modules and expansions by a piece of theirs
icon('ng_creatures', fit(N + 'token/creature/wolf-1.webp'), 'ModuleOption(Creatures)')
icon('ng_morecreatures', fit(N + 'token/creature/draugr-1.webp'), 'MoreCreatures')
icon('ng_wilderness', center(N + 'tile/wild-den.webp', 0.55), 'WildernessTiles', 'WildPartsTiles', 'LandsWilderness')
icon('ng_wildcreatures', fit(N + 'token/creature/fallen-valkyrie-1.webp'), 'WildernessCreatures', 'WildPartsCreatures', 'WildPartsBoth')
icon('ng_wastelands', center(N + 'tile/waste-vedrfolnir.webp', 0.55), 'WastelandsTiles', 'WastePartsTiles', 'LandsWastelands')
icon('ng_wastecreatures', fit(N + 'token/creature/hrimgandr-1.webp'), 'WastelandsCreatures', 'WastePartsCreatures', 'WastePartsBoth')
icon('ng_warchief', fit(N + 'token/unit/chief-wolf-blue.webp'), 'ModuleOption(Warchiefs)', 'WarchiefPawns')
icon('ng_warchiefcard', top(N + 'card/back/clan.webp'), 'WarchiefCards')
icon('ng_sea', fit(N + 'ui/ship.webp'), 'ModuleOption(Sea)')
icon('ng_events', box(N + 'card/event/blizzard.webp', 0.1, 0.05, 0.9, 0.95), 'ModuleOption(EventsModule)')
icon('ng_horizons', center(N + 'tile/horizon-bridge.webp', 0.55), 'HorizonsTiles')
icon('ng_devs', top(N + 'card/back/development.webp'), 'HorizonsDevelopments', 'ExtraDevelopment', 'FewerDevelopments',
     'NoDrawDevelopments', 'NoDefensiveStrategy')
icon('ng_achievement', top(N + 'card/back/achievement.webp'), 'ExtraAchievement')
icon('ng_balanced', center(N + 'tile/start.webp', 0.6), 'BalancedStartTiles', 'StandardCentral')
icon('ng_aggressive', fit(N + 'token/creature/eldthurs-1.webp'), 'ModuleOption(AggressiveCreatures)')
icon('ng_fameup', fit(N + 'ui/fame.webp'), 'ModuleOption(FameUpgradesModule)', 'PublicFame')
icon('ng_military', fit(N + 'ui/mxp.webp'), 'ModuleOption(MilitaryModule)')
icon('ng_minerals', fit(N + 'ui/gem.webp'), 'ModuleOption(MineralsModule)')
icon('ng_pc', fit(N + 'token/building/town-hall-upgraded.webp'), 'ModuleOption(PCVersion)')
icon('ng_tower', fit(N + 'token/building/defense-tower.webp'), 'ModuleOption(TowersModule)')
icon('ng_chiefpc', fit(N + 'token/unit/bodyguard-blue.webp'), 'ModuleOption(WarchiefsPC)')
icon('ng_domination', fit(N + 'token/building/town-hall.webp'), 'DominationVictory', 'DominationOnly')
icon('ng_famegains', fit(N + 'token/building/altar-of-kings.webp'), 'ModuleOption(FameGainsModule)', 'FameOnly')
icon('ng_standardwin', fit(N + 'token/building/fortress.webp'), 'StandardVictory')
icon('ng_altvictory', box(N + 'card/victory/conquest.webp', 0.06, 0.2, 0.94, 0.8), 'AltVictoryChosen', 'AltVictoryRandom')
for c in sorted(os.listdir(N + 'card/victory')):
    v = c[:-5]
    icon('ng_v_' + v.replace('-', '')[:20], box(N + 'card/victory/' + c, 0.06, 0.2, 0.94, 0.8), 'VictoryCardOption(VictoryCard("' + v + '"))')
# the central tiles, their middles
for o, t in [('DenCentral', 'start-den'), ('GreatLakeCentral', 'start-lake'), ('HelheimCentral', 'start-helheim'),
             ('HrimgandrCentral', 'start-hrimgandr'), ('MagmaFlowCentral', 'start-magma'), ('MimirCentral', 'start-mimir'),
             ('RelicCentral', 'start-relic'), ('VolcanoCentral', 'start-volcano'), ('WildLakeCentral', 'wild-lake'),
             ('YggdrasilCentral', 'start-yggdrasil')]:
    icon('ng_c_' + t.split('-')[-1], center(N + 'tile/' + t + '.webp', 0.5), o)

os.makedirs(OUT, exist_ok=True)
for s, im in icons.items():
    assert len('hrf_' + s) <= 32, s
    # a palette of 256 colours keeps each file a few KB (Discord takes up to 256 KB)
    im.quantize(256, method=Image.FASTOCTREE).save(os.path.join(OUT, s + '.png'), optimize=True)
with open(os.path.join(OUT, 'icons.tsv'), 'w') as f:
    for o, s in keys:
        f.write(o + '\t' + s + '\n')
print(len(icons), 'icons,', len(keys), 'options')
