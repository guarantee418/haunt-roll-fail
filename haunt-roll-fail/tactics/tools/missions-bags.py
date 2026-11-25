#!/usr/bin/env python3
# The rest of the Mission Booklet's missions on the Large Map Pack panels (4.1 2-panel, 5.0 and 5.2 3-panel, 6.0 and 6.1 4-panel in
# a row, and the free-for-alls 8.1 and 8.2), each laid as its bag in the mod lays it: missions-bags.py <dir> <out>, <dir> holding the
# panels (named by their hashes, as the mod's save.json names them; the save itself as save.json); <out> gets map-<panels>.webp
# (1200 px a panel; a panel turned round is named with an "r") and missions-bags.scala.txt.
# A bag lays its panels left to right by posX; one with rotY 0 is turned round (180 degrees), as the booklet's arrow marks it. The
# hand-read features of each panel (the BASE pairs: Hill 331, Trois Ponts, Stalingrad and Monastery read here, Moro River and Ardennes
# Forest by missions.py) are carried over with it, turned round with it; the cover lines are read again on the whole map by
# map-edges.py, the Control Points taken from the resource tokens in the bag. The Spawn Areas, and the Buildings, Bunkers and Cathedrals
# the booklet adds, are read off the booklet's pictures (pages 11 to 25 and 30) as fractions of the map's width and height.
# Monastery: MO1 is the panel printed "MONASTERY - 1" (2FBB4A...), MO2 "MONASTERY - 2" (88D46C...); the mod's two Stalingrad copies
# (2AB1A9..., B27812...) are the same art as SG2 and SG1.
import sys, os, json, subprocess
from PIL import Image
Image.MAX_IMAGE_PIXELS = None
D, OUT = (sys.argv + ['', ''])[1:3]
ONLY = sys.argv[3:]
HERE = os.path.dirname(os.path.abspath(__file__))
PANEL = {'HL1': '43EC122398E918BD6063DC597F153B5595003E35', 'HL2': '05913A192FB9B46BDCC51CB41526E456944A4F96',
         'TP1': '32F7897D7910788E4C3AD6C35F47BFF1C4162410', 'TP2': '09C63468D71609D36360BF6288D23A1BCB1ACACB',
         'SG1': '4CBB5917D1188AB3A82F68D63FA36737612B72AB', 'SG2': '943F8DBA40970061747CF47323BCC53002DDD8A0',
         'MO1': '2FBB4A77413277E109A1BD2F1CB7AE420F68D840', 'MO2': '88D46CACA4777D7F1CC6E47ECD95121572DF26FB',
         'MR1': 'DFF22E047EEC2B3D162B3A1A384815F403ED3C05', 'MR2': '9CA2558A6336BADA801A44D1AA70518520F19CDE',
         'AF1': 'BA5B22330444EFD4368DFF43D78742B9213D8882', 'AF2': 'A02EBD6944A47B3931A2F89115F1D7B5F2FD349E'}
NAME = {h: n for n, h in PANEL.items()}
NAME['2AB1A9D2D9DABCAA226CDC24579050FF8D014E12'] = 'SG2'
NAME['B278129E1CC6068553A345A75ECB32CF700B37D5'] = 'SG1'
# the panels whose hexes line up with X0 926 as stored (the others 464.5; turned round, the other way)
FIRST = {'HL1', 'TP2', 'SG2', 'MO1', 'MR1', 'AF2'}
Y0, DX, S = 741, 923.0, 501.0
DY = 1.5 * S
def center(r, c, x0): return (x0 + c * DX + (r % 2) * DX / 2, Y0 + r * DY)
def hexat(x, y, x0):
    r = round((y - Y0) / DY)
    return (r, round((x - x0 - (r % 2) * DX / 2) / DX))
# hand-read features in a base layout: (left panel, right panel, X0, {kind: hexes}); K is a Bunker, T a tank trap, 'r,c/e' an edge
# map-edges.py reads as a line that isn't one
BASE = [
    ('HL1', 'HL2', 926, {'B': {'1,2', '1,8'}, 'K': {'5,5'}, 'D': {'8,3', '9,5', '8,8'}}),
    ('TP1', 'TP2', 464.5, {'B': {'4,5', '2,11', '4,7', '8,1', '8,5', '7,7'}, 'D': {'3,2', '8,10'},
                           'W': {'2,7', '3,6', '7,6', '8,7', '10,8', '11,8'}}),
    ('SG1', 'SG2', 464.5, {'B': {'3,2', '3,5', '3,6', '6,1', '6,2', '6,8', '6,9', '5,11'}, 'D': {'1,2', '3,3', '0,10', '10,2', '8,7', '8,10'},
                           'I': {'0,12', '1,10', '1,11', '2,10', '2,11', '3,8', '3,10', '4,3', '4,7', '4,8', '4,9', '5,0', '5,1', '5,2', '5,3',
                                 '5,4', '5,5', '5,7', '5,8', '6,5', '7,4', '8,1', '8,4', '8,5', '9,0', '9,1', '9,2', '9,3', '9,4'}}),
    # the Monastery's cross-shaped church is one multi-hex Building (4,11 5,9 5,10 5,11 6,11); the small roofless sheds are cover
    ('MO2', 'MO1', 464.5, {'B': {'5,2', '4,8', '6,8', '4,11', '5,9', '5,10', '5,11', '6,11'}, 'D': {'2,0', '8,0', '2,9', '8,9'}}),
    ('MR1', 'MR2', 926, {'W': {'-1,2', '0,2', '4,4', '5,5', '6,7', '10,10', '11,11'}, 'B': {'4,3', '5,3', '5,0', '5,7', '6,8', '5,10'},
                         'D': {'6,5', '4,6'}, 'T': {'1,4', '6,2', '4,5', '4,9', '6,6', '9,6'},
                         'drop': {'1,-1/3', '8,0/3', '3,7/5', '-1,10/4', '11,-1/1'}}),
    ('AF2', 'AF1', 926, {'B': {'3,-1', '3,11', '5,0', '5,10', '6,2', '7,1', '7,8', '7,9'}, 'C': {'2,8', '2,9', '3,7', '3,8', '4,9'},
                         'D': {'1,10', '4,2', '5,7', '7,3', '7,11', '9,1'}, 'I': {'4,5', '4,7', '5,3', '5,4', '5,5', '5,6', '5,8', '6,4', '6,6', '7,4'}}),
]
# each panel's features as points in its stored picture: [(kind, x, y, edge)]
LOCAL = {}
for bl, br, bx0, feats in BASE:
    for kind, hs in feats.items():
        for k in hs:
            k, e = (k.split('/') + [None])[:2]
            r, c = map(int, k.split(','))
            x, y = center(r, c, bx0)
            panel, local = (bl, x) if x < 6000 else (br, x - 6000)
            LOCAL.setdefault(panel, []).append((kind, local, y, None if e is None else int(e)))
# Spawn Areas: a list for each team (or player), of ('T', a, b) the top row's half hexes from a to b of the map's width, ('B', a, b)
# the bottom row's, ('L') and ('R') the hexes along the left and right edges, ('P', x, y) the hex at that point (fractions)
FULL = [[('T', 0, 1)], [('B', 0, 1)]]
def tb(a, b, c, d): return [[('T', a, b)], [('B', c, d)]]
def pts(*ps): return [('P', x, y) for x, y in ps]
# id, name, bag, stockpile (the booklet's), spawn, extra features [(kind, x, y)] as fractions, players, trigger, infantry, text
M = [
    ('cathedral', 'Cathedral', 'Components - 5VP Cathedral - 2 Panels - Stockpiles 4/4/3', (4, 4, 2), FULL,
        [('K', 278 / 380, 144 / 282), ('K', 307 / 380, 144 / 282)], None, None, None,
        "the Monastery's two churches, a Bunker between them"),
    ('cologne', 'Cologne', 'Components - 5VP Cologne - 2 Panels Stockpiles 3/4/4', (4, 4, 6), tb(0, 0.5, 0, 0.5), [], None, None, None,
        'the town and the river on the left, the Monastery turned round; Spawn Areas on the left half'),
    ('oasis', 'Desert Oasis', 'Components - 5VP Desert Oasis - 2 Panels Stockpiles 6/4/2', (6, 2, 2), tb(0.02, 0.6, 0.35, 0.97),
        [('K', 190 / 380, 143 / 285)], None, None, None, 'the hill, its Bunker, and the river town'),
    ('nomansland', "No Man's Land", "Components - 5VP No Man's Land - 2 Panels Stockpiles 2/3/0", (2, 5, 0), FULL, [], None, None, None,
        'the hills and the Monastery turned round'),
    ('moscowout', 'Moscow Outskirts', 'Components - 6VP Moscow Outskirts- 2 Panels Stockpiles 5/4/4', (5, 4, 4), tb(0, 0.5, 0.5, 1), [],
        None, None, None, 'Stalingrad turned round and the snowy forest; Spawn Areas at opposite corners'),
    ('faith', 'Opposing Faiths', 'Components - 5VP Opposing Faith - 2 Panels Stockpiles 7/4/1', (7, 4, 1), FULL,
        [('C', 87 / 380, 138 / 280), ('C', 73 / 380, 115 / 280), ('C', 30 / 380, 138 / 280), ('C', 58 / 380, 138 / 280), ('C', 73 / 380, 162 / 280)],
        None, None, None, 'the Monastery; a multi-hex Cathedral on the left'),
    ('semois', 'Semois', 'Components - 6VP Semois - 2 Panels Stockpiles 5/4/3', (5, 3, 4),
        [[('T', 0.4, 1)], pts(*[((x - 40) / 380, (y - 138) / 287) for x, y in [(55, 305), (70, 330), (55, 355), (70, 378), (55, 403), (87, 403), (117, 403), (147, 403)]])],
        [], None, None, None, 'Moro River and the Monastery; Team 2 spawns in the bottom left corner, among tank traps; highly asymmetrical'),
    ('hinderdam', 'Hinderdam', 'Components - 6VP Hinderdam - 2 Panels Stockpiles 4/4/3', (4, 4, 3), tb(0, 0.6, 0.4, 1), [], None, None, None,
        'the Monastery and the Moro River with its fords'),
    ('ancientfields', 'Ancient Fields', 'Components - 8 VP Ancient Fields - 3 Panels - Stockpiles 4/6/2', (2, 6, 2), tb(0.22, 0.97, 0, 0.56),
        [('K', 78 / 513, 128 / 256), ('B', 180 / 513, 58 / 256), ('B', 473 / 513, 170 / 256)], None, None, None,
        'the Monastery and Hill 331, a Bunker and two Buildings added'),
    ('airfield', 'Airfield', 'Components - 7VP Airfield - 3 Panels Stockpiles 4/3/2', (4, 3, 3), tb(0, 0.66, 0.26, 0.98),
        [('B', 460 / 513, 62 / 256)], None, None, None, 'Trois Ponts and the Monastery with its airstrip'),
    ('hilltop', 'Hilltop Monastery', 'Components - 7VP Hilltop Monastery - 3 Panels - Stockpiles 6/6/4', (6, 6, 4), tb(0, 0.77, 0.2, 0.77), [],
        None, None, None, 'Hill 331 and both Monastery panels'),
    ('budapest', 'Budapest', 'Components - 6VP Budapest - 3 Panels Stockpiles 4/2/2', (4, 2, 3), tb(0.18, 0.97, 0.02, 0.8), [], None, None, None,
        'Moro River and the Monastery'),
    ('siege', 'Siege of Leningrad', 'Components - 7VP Siege of Leningrad- 3 Panels Stockpiles 6/4/0', (6, 4, 0),
        [[('T', 0.31, 0.665)], [('B', 0, 0.31), ('B', 0.665, 1)]], [], None, None, None,
        'Stalingrad turned round and the snowy forest; Team 2 spawns at both bottom corners; highly asymmetrical'),
    ('faith3', 'Opposing Faiths (3 panels)', 'Components - 6 VP Opposing Faiths - 3 Panels - Stockpiles 6/4/2', (6, 4, 2), FULL,
        [('K', 250 / 513, 127 / 255), ('K', 277 / 513, 127 / 255)], None, None, None, 'the Monastery and another MO1, a Bunker in the middle'),
    ('widefront', 'The Wide Front', 'Components - 7 VP The Wide Front - 3 Panels - Stockpiles 4/6/4', (4, 6, 4), tb(0.2, 0.77, 0.2, 0.77), [],
        None, None, None, 'three Hill 331 panels'),
    ('troisponts3', 'Trois Ponts (3 panels)', 'Components - 7 VP Trois Ponts - 3 Panels - Stockpiles 6/6/2', (6, 6, 2),
        [[('T', 0, 0.34), ('T', 0.665, 1)], [('B', 0, 0.4), ('B', 0.665, 1)]], [], None, None, None,
        'Trois Ponts and another TP1; each team spawns at both ends'),
    ('twoislands', 'Two Islands', 'Components - 7VP Two Islands - 3 Panels - Stockpiles 3/2/2', (3, 2, 2), tb(0, 0.72, 0.26, 1), [],
        None, None, None, 'Stalingrad and another SG1: the frozen Volga twice'),
    ('cologne4', 'Cologne (4 panels)', 'Components - 8VP Cologne - 4 Panels - Stockpiles 4/2/2', (4, 4, 6), tb(0.19, 1, 0, 0.81), [],
        None, None, None, 'Trois Ponts and both Monastery panels turned round'),
    ('tpmonastery', 'Trois Ponts Monastery', 'Components - 8 VP Trois Ponts Monastery - 4 Panels  - Stockpiles 6/6/4', (6, 6, 4),
        [[('T', 0, 0.26), ('T', 0.5, 1)], [('B', 0.13, 0.38), ('B', 0.73, 1)]], [], None, None, None,
        'Trois Ponts and both Monastery panels turned round'),
    ('hillmonastery', 'Hill 331 Monastery', 'Components - 8 VP Hill 331 Monastery - 4 Panels  - Stockpiles 4/5/6', (3, 2, 1),
        tb(0.17, 0.77, 0.17, 0.77), [], None, None, None, 'Hill 331 and the Monastery'),
    ('vengeance', 'Vengeance', 'Components - 8 VP Vengenance - 4 Panels -  Stockpiles 6/4/4', (6, 4, 4),
        [[('T', 0.25, 0.77), ('B', 0.25, 0.77)], [('L',), ('R',)]], [], None, None, None,
        'four Monastery panels; Team 1 spawns at the top and bottom, Team 2 at the left and right edges'),
    ('faith4', 'Opposing Faiths (4 panels)', 'Components - 8 VP Opposing Faiths - 4 Panels -  Stockpiles 4/2/2', (4, 2, 2),
        tb(0.25, 0.98, 0.02, 0.76), [], None, None, None, 'four Monastery panels'),
    ('twohilltops', 'Two Hilltops', 'Components - 8 VP Two Hilltops - 4 Panels  - Stockpiles 4/4/6', (4, 4, 6),
        [[('L',), ('T', 0.25, 0.59)], [('R',), ('B', 0.25, 0.59)]], [], None, None, None,
        'four Hill 331 panels; each team spawns along its edge and on one side'),
    ('frozenfork', 'Frozen Fork', 'Components - 8VP Frozen Fork - 4 Panels - Stockpiles 3/3/3', (3, 3, 3),
        [[('T', 0.25, 0.48), ('T', 0.74, 1)], [('B', 0, 0.25), ('B', 0.5, 0.73)]], [], None, None, None, 'four Stalingrad panels'),
    ('sixbridges', 'Six Bridges', 'Components - 8 VP Six Bridges - 4 Panels - Stockpiles 5/4/3', (5, 4, 3), tb(0.27, 0.77, 0.23, 0.77), [],
        None, None, None, 'four Trois Ponts panels, the river crossed six times'),
    ('hill331ffa', 'Hill 331, 3 Way Free-for-all', 'Components - 3FFA 5VP Hill 331 - 2 Panels Stockpiles 6/4/2', (6, 2, 4),
        [pts(*[(x / 1140, 80 / 870) for x in (440, 527, 613, 700)]),
         pts((1053 / 1140, 660 / 870), (1010 / 1140, 735 / 870), (1097 / 1140, 735 / 870), (1053 / 1140, 808 / 870)),
         pts((88 / 1140, 660 / 870), (45 / 1140, 735 / 870), (133 / 1140, 735 / 870), (88 / 1140, 808 / 870))],
        [('D', 570 / 1140, 435 / 870), ('B', 527 / 1140, 505 / 870), ('B', 610 / 1140, 505 / 870), ('B', 570 / 1140, 580 / 870)], 3, 15, None,
        'three players, each on their own; the Bunker is a Destroyed Building, three Buildings below it'),
    ('cologneffa', 'Cologne, 4 Way Free-for-all', 'Components - 4FFA - 8VP  Cologne 3 Panels - Stockpiles 6/4/4', (6, 4, 4),
        [pts(*[((x - 40) / 570, 23 / 286) for x in (55, 85, 117)]), pts(*[((x - 40) / 570, 23 / 286) for x in (520, 552, 583)]),
         pts(*[((x - 40) / 570, 263 / 286) for x in (520, 552, 583)]), pts(*[((x - 40) / 570, 263 / 286) for x in (55, 85, 117)])],
        [], 4, 15, None, 'four players, each on their own, one in each corner'),
]
_save = []
def bag(name):
    if not _save: _save.append(json.load(open(os.path.join(D, 'save.json'))))
    save = _save[0]
    out = []
    def walk(o):
        if isinstance(o, dict):
            if o.get('Name') == 'Bag' and o.get('Nickname') == name: out.append(o)
            for v in o.values(): walk(v)
        elif isinstance(o, list):
            for v in o: walk(v)
    walk(save)
    return out[0]
RES = {'F656625625C74E8EFDD086FBAFDB1AABC921B481': 'V', 'FE68E027387D8B0CE1CB90CD6C54985FB2359B5A': 'M',
       'D460F15B813E4215F438753ECD8BF18997A0027A': 'F', '84AA70409E802FC5F6C55B00AC2CA09C6E4BA4D5': 'U'}
W = 26.35; H = W * 1.5
# the bags that lay the booklet's map turned round (its panels in the other order, each turned the other way, and the tokens with them):
# the map is built the booklet's way
FLIP = {'cathedral', 'cologne', 'faith'}
def layout(name, flip):
    b = bag(name); panels = {}; toks = []
    s = -1 if flip else 1
    for c in b['ContainedObjects']:
        u = ((c.get('CustomImage') or {}).get('ImageURL') or '').rstrip('/').split('/')[-1]; t = c['Transform']
        if c['Name'] == 'Custom_Tile' and t.get('scaleX', 1) > 5:
            panels[round(s * t['posX'])] = (NAME[u], (round(t.get('rotY', 180)) % 360 != 180) != flip, s * t['posX'], s * t['posZ'])
        elif u in RES: toks.append((RES[u], s * t['posX'], s * t['posZ']))
    ps = [panels[k] for k in sorted(panels)]
    return ps, toks
# the booklet's optional Modular Bunkers (A, B and C hexes; setup option OptionalMapFeatures) and the Hilltop Monastery's optional Victory
# Point in its Cathedral tower: (kind, x, y) as fractions of the map, K a Bunker, V a Victory Point
def opt(box, *ps, kind='K'): return [(kind, (x - box[0]) / (box[2] - box[0]), (y - box[1]) / (box[3] - box[1])) for x, y in ps]
OPT = {
    'nomansland': opt((40, 165, 420, 440), (333, 295), (125, 295), (153, 295), (303, 295)),
    'oasis': opt((460, 430, 840, 715), (635, 548), (665, 548), (635, 598), (665, 598)),
    'hinderdam': opt((40, 463, 420, 748), (127, 606), (155, 606), (185, 606), (215, 606), (113, 630)),
    'ancientfields': opt((327, 42, 840, 298), (670, 168), (655, 145), (685, 145), (655, 190), (685, 190)),
    'airfield': opt((327, 320, 840, 576), (748, 448), (405, 448), (432, 448), (722, 448)),
    'hilltop': opt((327, 590, 840, 845), (748, 717), (405, 717), (392, 740), (762, 695)) + opt((327, 590, 840, 845), (628, 717), kind='V'),
    'widefront': opt((327, 42, 840, 298), (670, 168), (655, 145), (685, 145), (655, 190), (685, 190), (485, 168), (512, 168)),
    'hillmonastery': opt((80, 130, 840, 415), (240, 272), (268, 272), (296, 272), (738, 272), (766, 272)),
    'twohilltops': opt((80, 450, 840, 737), (270, 592), (298, 592), (620, 592), (650, 592)),
}
# the bags whose panels the booklet has otherwise (by its pictures' printed compass marks), where the mod's don't line up: Hilltop
# Monastery's bag has HL1 on the left, the booklet HL2; The Wide Front's turns its first HL2 round, the booklet doesn't; Opposing
# Faiths on 4 panels has MO2, MO1 first, the booklet MO1, MO2. The tokens stay where the bag has them.
SWAP = {'hilltop': {0: ('HL2', False)}, 'widefront': {0: ('HL2', False)}, 'faith4': {0: ('MO1', False), 1: ('MO2', False)}}
def points(ps, toks, x0):
    lx, lz = ps[0][2] - W / 2, ps[0][3]
    used = set(); out = {}
    for i, a in enumerate(toks):
        if i in used: continue
        j = min((j for j in range(len(toks)) if j != i and j not in used), key=lambda j: (toks[j][1] - a[1]) ** 2 + (toks[j][2] - a[2]) ** 2, default=None)
        if j is None: j = i
        if j == i or (toks[j][1] - a[1]) ** 2 + (toks[j][2] - a[2]) ** 2 > 4: j = None
        b2 = [] if j is None else [toks[j]]
        used |= {i} | ({j} if j is not None else set())
        ts = [a] + b2
        mx, mz = sum(t[1] for t in ts) / len(ts), sum(t[2] for t in ts) / len(ts)
        r, c = hexat((mx - lx) / W * 6000, (lz + H / 2 - mz) / H * 9000, x0)
        k = '%d,%d' % (r, c)
        out[k] = ''.join(sorted(out.get(k, '') + ''.join(t[0] for t in ts), key='VMUF'.index))
    return out
OBJ = {'cathedral': 'CathedralMission', 'cologne': 'CologneMission', 'oasis': 'DesertOasis', 'nomansland': 'NoMansLand',
       'moscowout': 'MoscowOutskirts', 'faith': 'OpposingFaiths', 'semois': 'Semois', 'hinderdam': 'Hinderdam', 'ancientfields': 'AncientFields',
       'airfield': 'AirfieldMission', 'hilltop': 'HilltopMonastery', 'budapest': 'Budapest', 'siege': 'SiegeOfLeningrad',
       'faith3': 'OpposingFaithsThree', 'widefront': 'TheWideFront', 'troisponts3': 'TroisPontsThree', 'twoislands': 'TwoIslands',
       'cologne4': 'CologneFour', 'tpmonastery': 'TroisPontsMonastery', 'hillmonastery': 'HillMonastery', 'vengeance': 'Vengeance',
       'faith4': 'OpposingFaithsFour', 'twohilltops': 'TwoHilltops', 'frozenfork': 'FrozenFork', 'sixbridges': 'SixBridges',
       'hill331ffa': 'HillThreeThirtyOneFFA', 'cologneffa': 'CologneFFA'}
def cls(mid): return OBJ[mid]
if __name__ == '__main__':

    out = []
    for mid, name, bagname, stock, spawn, extra, players, trigger, infantry, text in M:
        if ONLY and mid not in ONLY: continue
        ps, toks = layout(bagname, mid in FLIP)
        ps = [SWAP.get(mid, {}).get(i, p[:2]) + p[2:] for i, p in enumerate(ps)]
        n = len(ps)
        left, lturn = ps[0][0], ps[0][1]
        x0 = 926 if (left in FIRST) != lturn else 464.5
        for i, (p, turned, _, _) in enumerate(ps):
            assert (926 if (p in FIRST) != turned else 464.5) == (x0 if i % 2 == 0 else (1390.5 - x0)), (mid, ps)
        img = 'map-' + ''.join(p.lower() + ('r' if t else '') for p, t, _, _ in ps)
        full = os.path.join(D, 'mission-' + mid + '.png')
        MW = 6000 * n
        if not os.path.exists(full) or not os.path.exists(os.path.join(OUT, img + '.webp')):
            m = Image.new('RGB', (MW, 9000))
            for i, (p, turned, _, _) in enumerate(ps):
                im = Image.open(os.path.join(D, PANEL[p])).convert('RGB')
                if turned: im = im.rotate(180)
                m.paste(im, (6000 * i, 0))
            if not os.path.exists(full): m.save(full)
            m.resize((MW // 5, 1800), Image.LANCZOS).save(os.path.join(OUT, img + '.webp'), 'WEBP', quality=82, method=6)
        ej = os.path.join(D, 'edges-' + mid + '.json')
        if not os.path.exists(ej): subprocess.run([sys.executable, os.path.join(HERE, 'map-edges.py'), full, ej, os.path.join(D, 'overlay-' + mid + '.png'), str(x0)], check=True)
        edges = json.load(open(ej))
        feats = {}; drops = set()
        for i, (p, turned, _, _) in enumerate(ps):
            for kind, x, y, e in LOCAL.get(p, []):
                if turned: x, y, e = 6000 - x, 9000 - y, None if e is None else (e + 3) % 6
                r, c = hexat(x + 6000 * i, y, x0)
                if e is None: feats['%d,%d' % (r, c)] = kind
                else: drops.add(('%d,%d' % (r, c), e))
        for kind, fx, fy in extra:
            r, c = hexat(fx * MW, fy * 9000, x0)
            feats['%d,%d' % (r, c)] = kind
        cps = points(ps, toks, x0)
        rows = []; hexes = []
        for r in range(-1, 12):
            for c in range(-2, int(MW / DX) + 2):
                x, y = center(r, c, x0)
                if not (-50 < x < MW + 50): continue
                k = '%d,%d' % (r, c)
                kind = feats.get(k, '.')
                e = [None if (k, i) in drops else v for i, v in enumerate(edges.get(k, [None] * 6))]
                s = ''.join({'red': 'X', 'green': 'H', 'yellow': 'L', None: '.', 'purple': '.'}[v] for v in e)
                if kind in 'BCDK': s = '......'
                t = 'B' if kind == 'K' else (kind if kind in 'WBCDI' else '.')
                ex = ('K' if kind == 'K' else '') + ('T' if kind == 'T' else '') + cps.get(k, '')
                rows.append('"%d %d %s %s %s"' % (r + 1, c + 1, t, s, ex or '-'))
                hexes.append((r, c, x, y, t))
        lost = [k for k in cps if not any(('%d,%d' % (h[0], h[1])) == k for h in hexes)]
        def area(parts):
            hs = []
            for p in parts:
                for (r, c, x, y, t) in hexes:
                    if t == 'W': continue
                    ok = (p[0] == 'T' and r == -1 and p[1] * MW <= x <= p[2] * MW) or (p[0] == 'B' and r == 11 and p[1] * MW <= x <= p[2] * MW) or \
                         (p[0] == 'L' and x < DX * 0.75) or (p[0] == 'R' and x > MW - DX * 0.75) or \
                         (p[0] == 'P' and (r, c) == hexat(p[1] * MW, p[2] * 9000, x0))
                    if ok and (c, r) not in hs: hs.append((c, r))
            return hs
        areas = None if spawn is FULL else [area(a) for a in spawn]
        optional = []
        for kind, fx, fy in OPT.get(mid, []):
            rr, c = hexat(fx * MW, fy * 9000, x0)
            i = [j for j, x in enumerate(rows) if x.startswith('"%d %d ' % (rr + 1, c + 1))][0]
            l = rows[i].strip('"').split(' ')
            ex = l[4].replace('-', '').replace('K', '')
            if kind == 'V' and 'V' in ex:
                # the bag has the optional Victory Point among its tokens: the map leaves it out, the option puts it back
                rest = ex.replace('V', '', 1)
                rows[i] = '"%s %s %s %s %s"' % (l[0], l[1], l[2], l[3], ('K' if 'K' in l[4] else '') + rest or '-')
                ex = rest
            optional.append('"%d %d %s %s"' % (rr + 1, c + 1, 'B' if kind == 'K' else l[2], ('K' + ex) if kind == 'K' else (ex + 'V')))
        print('   optional', optional)
        print(mid, [p + ('r' if t else '') for p, t, _, _ in ps], 'x0', x0, len(rows), 'rows', len(cps), 'points', sorted(cps.items()), 'lost', lost,
              'areas', areas and [len(a) for a in areas], 'feats', {k: v for k, v in feats.items() if v in 'KC'})
        out.append((mid, name, ps, x0, rows, areas, img, stock, players, trigger, infantry, text, n, optional))
    with open(os.path.join(OUT, 'missions-bags.scala.txt'), 'w') as f:
        for mid, name, ps, x0, rows, areas, img, stock, players, trigger, infantry, text, n, optional in out:
            o = cls(mid)
            args = []
            if x0 != 926: args.append('x0 = 92.9')
            if areas: args.append('areas = %sRows.areas' % o)
            if n != 2: args.append('panels = %d' % n)
            if players: args.append('players = |(%d)' % players)
            if trigger: args.append('trigger = |(%d)' % trigger)
            if optional: args.append('optional = %sRows.optional' % cls(mid))
            bs = set((int(x.strip('"').split(' ')[0]) - 1, int(x.strip('"').split(' ')[1]) - 1) for x in rows if x.strip('"').split(' ')[2] in ('B', 'C'))
            def nb(r, c): return [(r, c - 1), (r, c + 1)] + [(r + d, c + e) for d in (-1, 1) for e in ((-1, 0) if r % 2 == 0 else (0, 1))]
            multi = any(q in bs for (r, c) in bs for q in nb(r, c))
            f.write('// %s: %s (the bag "%s")\n' % (name, ' | '.join(p + (' turned round' if t else '') for p, t, _, _ in ps), mid))
            f.write('object %s extends MapData("%s", "%s", "%s", (%d, %d, %d), %s, %sRows.rows%s)\n\n' % (o, mid, name, img, stock[0], stock[1], stock[2],
                'true' if multi else 'false', o, ''.join(', ' + a for a in args)))
            f.write('object %sRows {\n' % o)
            if optional: f.write('    val optional = $(%s)\n\n' % ', '.join(optional))
            if areas: f.write('    val areas = $(%s)\n\n' % ', '.join('$(' + ', '.join('Hex(%d, %d)' % (c + 1, r + 1) for c, r in a) + ')' for a in areas))
            f.write('    val rows = $(\n        %s\n    )\n}\n\n' % ',\n        '.join(rows))
        f.write('// choices\n')
        for mid, name, ps, x0, rows, areas, img, stock, players, trigger, infantry, text, n, optional in out:
            lbl = ', '.join(p + ('r' if t else '') for p, t, _, _ in ps)
            f.write('case object %sMap extends MapChoice(%s, "Large Map Pack, %s: %s. Starting stockpile %d Manpower, %d Munitions, %d Fuel.")\n' % (
                cls(mid), cls(mid), lbl, text[0].upper() + text[1:], stock[0], stock[1], stock[2]))
        f.write('// all: ' + ', '.join(cls(m[0]) + 'Map' for m in out) + '\n')
