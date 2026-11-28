#!/usr/bin/env python3
# The 3rd printing's 17 1v1 scenarios (Bad Crow's "3rd printing balance changes" sheet, its "New Scenario's", pictures 308 to 324 counting the
# document's pictures from 1), on the new Black Gold (BG1, BG2) and Twin Beaches (TB1, TB2) panels and the older ones:
#   missions-printing.py <dir> <pics> <out>   <dir> holds the panels (named by their hashes, as the mod's save.json names them; the save
#   itself as save.json, the mod's BSON save turned to JSON), <pics> the sheet's pictures as <n>.png; <out> gets map-<panels>.webp and
#   missions-printing.scala.txt.
# Each map's panels were found by matching the sheet's picture with the panels' art (turned round or not); the cover lines are read by
# map-edges.py in its beach mode (the brown shorelines, and the permanent barbed wire symbols apart); the Spawn Areas are the sheet's blue
# (Team 1) and red (Team 2) bars, with the half row behind a bar, or its circles, one hex each; the Control Points are the sheet's counters,
# read by hand off the pictures (the counter finder this script started from missed some on the green panels), but Black Gold's and
# Across Moro River's, which come from their bags in the mod (Twin Beaches' bag holds its counters twice). The new panels' features were read by hand (2026-11-28):
#   Black Gold: Buildings, Destroyed Buildings, tank traps, and the Fortifications (ringed light blue).
#   Twin Beaches: Buildings, tank traps, the Sea (the water and beach hexes inside the brown shorelines, which every unit may enter and
#   only Troop Transports keep to) and the Anchorages (water ringed in red with an anchor: only Troop Transports enter them).
import sys, os, json, subprocess, importlib.util
from PIL import Image
Image.MAX_IMAGE_PIXELS = None
D, PICS, OUT = sys.argv[1:4]
HERE = os.path.dirname(os.path.abspath(__file__))
spec = importlib.util.spec_from_file_location('mb', os.path.join(HERE, 'missions-bags.py'))
mb = importlib.util.module_from_spec(spec)
sys.argv = [sys.argv[0], D, OUT]
spec.loader.exec_module(mb)
PANEL = dict(mb.PANEL)
PANEL.update({'BG1': '8CA87ABBBF8A92F2D5C0A7EDBCD268B33CADD13D', 'BG2': '3463FF971225BBCD5B3E1D0B8C262459C987C4CB',
              'TB1': 'A3F51A7C0815EE9C4435B3F181648C21AE2F59F7', 'TB2': '7F8643B5A41EB5043671E2202654FCC83A96056F'})
FIRST = set(mb.FIRST) | {'BG1', 'TB2'}
center, hexat = mb.center, mb.hexat
DX = mb.DX
def panelhexes(x0):
    return [(r, c) for r in range(-1, 12) for c in range(-2, 9) if -100 < center(r, c, x0)[0] < 6100]
def hl(s): return [tuple(map(int, k.split(','))) for k in s.split()]
# the new panels' hand-read features, in their stored picture with their own X0 when on the left: (kind, hexes); kinds B Building,
# D Destroyed Building, F Fortification, S Sea, A Anchorage, W Water, T tank trap (an extra, beside the terrain)
NEW = {
    'BG1': (926, [('B', hl('3,4 9,2 5,5')), ('D', hl('5,3 9,5')), ('F', hl('4,2 7,3')), ('T', hl('9,1'))]),
    'BG2': (464.5, [('B', hl('1,2 7,0 5,-1')), ('D', hl('9,-1')), ('F', hl('3,1 6,3')), ('T', hl('1,3'))]),
    'TB1': (464.5, [('B', hl('3,3 3,4 4,0 5,1')), ('W', hl('5,-1')),
                    ('S', hl('6,-1 6,0 7,0 8,0 9,0 8,1 9,1 9,2 9,3 9,4 9,5') + [h for h in panelhexes(464.5) if h[0] >= 10]),
                    ('T', hl('4,5 4,6 5,4 6,4 7,5 8,1 9,1 9,4'))]),
    'TB2': (926, [('B', hl('2,3 2,4 6,5')), ('A', hl('8,2 9,1 9,2 10,1 10,2 10,3')),
                  ('S', hl('0,2 1,0 1,1 1,2 2,0 2,1 2,2 3,0 3,2 3,3 4,0 4,1 4,5 5,1 5,2 5,3 6,1 6,2 7,1 7,2 7,3 8,1 8,3 9,0 9,3 10,0 10,4')
                        + [h for h in panelhexes(926) if h[0] >= 11]),
                  ('T', hl('-1,1 0,5 1,-1 1,1 4,-1 5,5 6,3 7,-1 7,1 9,0'))]),
}
LOCAL = {p: list(v) for p, v in mb.LOCAL.items()}
for p, (x0, feats) in NEW.items():
    LOCAL[p] = [(k, center(r, c, x0)[0], center(r, c, x0)[1], None) for k, hs in feats for (r, c) in hs]
def pts(s): return dict(kv.split(':') for kv in s.split())
# id, object, name, picture, panels [(panel, turned)], stockpile (the sheet's), Control Points ("r,c:VMUF", None: the bag's), the bag,
# boats (the teams that may unlock Troop Transports; None: those whose Spawn Area is by the Sea), text
M = [
    ('blackgold', 'BlackGold', 'Black Gold', 308, [('BG1', 0), ('BG2', 0)], (2, 2, 6), None, 'Components - 6VP Black Gold - 2 Panels Stockpiles 2/2/6', None,
        'the oil fields, with their Fortifications'),
    ('twinbeaches', 'TwinBeaches', 'Twin Beaches', 309, [('TB1', 0), ('TB2', 0)], (5, 3, 4),
        pts('3,2:VM 3,8:VF 3,5:UU 3,11:VU 5,6:VU 5,9:MF 5,0:VU 6,4:FF 7,2:VF 7,5:MU 7,8:MF 8,11:VM'), 'Components - 7VP Twin Beaches - 2 Panels Stockpiles 5/3/4', [1],
        'the two beaches; Team 2 may unlock Troop Transports'),
    ('acrossmoro', 'AcrossMoroRiver', 'Across Moro River', 310, [('MR1', 0), ('TB1', 0)], (4, 4, 3), None, 'Components - 7VP Across Moro River- 2 Panels Stockpiles 4/3/3', [1],
        'Moro River and a beach; Team 2 may unlock Troop Transports'),
    ('elalamein', 'ElAlamein', 'El Alamein', 311, [('BG2', 0), ('BG1', 0)], (4, 2, 3),
        pts('1,1:UU 2,5:VM 3,2:VM 5,0:MU 5,4:VF 7,0:FF 3,11:FF 5,7:VF 5,11:MU 7,9:VM 8,7:VM 9,10:UU'), None, None, 'both Black Gold panels'),
    ('ironhill', 'IronHill', 'Iron Hill', 312, [('BG2', 0), ('HL2', 1)], (5, 4, 5),
        pts('3,0:UF 3,4:MM 2,9:UF 2,12:VU 4,6:VU 5,1:VU 5,8:VV 6,6:VU 7,2:UF 7,4:MM 8,9:UF 8,12:VU'), None, None, 'Black Gold and Hill 331 turned round'),
    ('desertsedge', 'DesertsEdge', "Desert's Edge", 313, [('MR1', 1), ('BG1', 0)], (4, 3, 5),
        pts('3,1:MU 5,0:V 5,3:VM 7,0:MU 8,5:UF 3,6:MM 4,9:UF 2,11:VF 5,11:VU 7,9:VF 9,6:MM'), None, None, 'Moro River turned round and Black Gold'),
    ('scheldt', 'BattleOfTheScheldt', 'Battle of the Scheldt', 314, [('TB2', 0), ('TB1', 0)], (5, 3, 3),
        pts('0,2:UF 0,7:VF 0,9:MU 2,6:VF 4,6:VF 4,9:MM 5,2:MM 6,5:VF 7,1:MU 8,9:UF 9,4:VU 10,6:VU'), None, None, 'both Twin Beaches panels; each team spawns along its edge'),
    ('kasserine', 'KasserinePass', 'Kasserine Pass', 315, [('HL2', 1), ('BG1', 1)], (4, 3, 3),
        pts('1,8:VM 3,1:MU 3,4:UF 4,9:VM 4,11:MU 5,6:VF 6,0:MF 6,2:VU 7,5:UF 7,10:MU 9,2:VM'), None, None, 'Hill 331 and Black Gold, both turned round'),
    ('crusader', 'OperationCrusader', 'Operation Crusader', 316, [('HL1', 0), ('BG2', 0)], (5, 4, 4),
        pts('1,9:VM 2,6:FF 3,2:UF 4,8:VU 5,-1:MU 5,1:MU 5,3:VM 5,8:VU 5,10:M 7,2:UF 8,7:FF 9,9:VM'), None, None, 'Hill 331 and Black Gold; both teams spawn on the left'),
    ('tobruk', 'Tobruk', 'Tobruk', 317, [('BG1', 0), ('TB1', 0)], (4, 2, 2),
        pts('1,2:VM 1,8:MF 4,2:MU 4,5:VU 4,8:VF 6,6:VU 6,9:MU 7,3:VF 8,2:MF 9,8:VM'), None, None,
        'Black Gold and a beach, a Destroyed Building between them at the bottom; each team spawns along its edge'),
    ('mareth', 'MarethLine', 'Mareth Line', 318, [('TB2', 0), ('BG2', 0)], (3, 4, 3),
        pts('0,2:MF 1,6:VF 2,9:MU 3,3:UF 4,7:VM 5,1:VU 5,8:VU 6,4:VM 8,1:MU 8,7:UF 9,4:VF 9,9:MF'), None, None,
        'a beach and Black Gold, a Destroyed Building between them at the bottom; each team spawns along its edge'),
    ('husky', 'OperationHusky', 'Operation Husky', 319, [('TB2', 0), ('MR2', 0)], (5, 4, 3),
        pts('3,3:VU 4,0:VU 5,2:VF 6,0:VU 7,3:VU 3,5:F 3,7:MF 2,10:MM 5,9:V 7,5:F 7,8:MF 9,9:MM'), None, None, 'a beach and Moro River'),
    ('saintemere', 'SainteMereEglise', 'Sainte-Mère-Église', 320, [('MO1', 1), ('TB2', 0)], (5, 2, 4),
        pts('1,4:VU 2,3:U 5,1:MU 5,4:VM 9,4:VF 8,3:F 2,7:V 4,9:UU 5,7:M 7,9:MF 8,7:V 0,9:MF'), None, None,
        "the Monastery turned round and a beach; the church tower's Manpower and Munitions point goes when the tower is destroyed; each team spawns along its edge"),
    ('salerno', 'Salerno', 'Salerno', 321, [('MR2', 1), ('TB1', 0)], (4, 3, 3),
        pts('1,1:VM 4,1:VM 3,4:U 5,4:V 7,5:MF 7,-1:F 3,7:MM 3,11:UF 6,10:V 7,9:V 9,10:MF 10,8:UF'), None, None, 'Moro River turned round and a beach'),
    ('closedport', 'ClosedPort', 'Closed Port', 322, [('MO2', 0), ('TB2', 0)], (6, 2, 4),
        pts('1,1:VU 2,4:MF 5,1:F 5,4:VM 7,0:VU 8,4:MF 1,10:MF 4,9:VF 5,8:U 5,10:M 6,7:VF 7,10:MF'), None, None,
        'the Monastery and a beach; Team 2 spawns at both bottom corners'),
    ('seaside', 'SeasideVillage', 'Seaside Village', 323, [('TP1', 0), ('TB2', 0)], (4, 6, 3),
        pts('1,1:MU 3,4:MF 4,0:VF 5,3:VF 7,0:MU 9,5:MF 1,11:MF 4,9:VU 5,10:VM 6,7:VM 7,8:VU 7,11:MF'), None, None,
        'Trois Ponts and a beach; Team 2 spawns at both bottom corners'),
    ('stormbeach', 'StormTheBeach', 'Storm the Beach', 324, [('TP2', 0), ('TB1', 0)], (6, 4, 4),
        pts('1,9:MU 3,0:MF 3,3:UF 4,7:VU 5,3:VM 5,4:VF 5,11:VF 6,1:UU 6,7:VU 7,4:UF 8,10:MU 9,1:MF'), None, None, 'Trois Ponts and a beach'),
]
# the sheet's notes: a Destroyed Building placed on the map (Tobruk, Mareth Line), and the point in a tower (Sainte-Mère-Église)
EXTRA = {'tobruk': {'9,5': 'D'}, 'mareth': {'9,5': 'D'}}
TOWER = {'saintemere': '5,1'}
RES = mb.RES
W, H = mb.W, mb.H
def bagpoints(name, left_hash, x0):
    b = mb.bag(name); lp = None; toks = []
    for c in b['ContainedObjects']:
        u = ((c.get('CustomImage') or {}).get('ImageURL') or '').rstrip('/').split('/')[-1]; t = c['Transform']
        if c['Name'] == 'Custom_Tile' and t.get('scaleX', 1) > 5 and u == left_hash and (lp is None or t['posX'] < lp[0]): lp = (t['posX'], t['posZ'])
        elif u in RES: toks.append((RES[u], t['posX'], t['posZ']))
    return mb.points([(None, None, lp[0], lp[1])], toks, x0)
def frame(px): r, g, b = px; return r < 120 and g < 40 and b < 50 and r > g + 20
def picbox(im):
    a = im.load(); w, h = im.size
    rows = [y for y in range(h) if sum(not frame(a[x, y]) for x in range(0, w, 7)) > 0.8 * (w / 7)]
    cols = [x for x in range(w) if sum(not frame(a[x, y]) for y in range(0, h, 7)) > 0.8 * (h / 7)]
    return cols[0], rows[0], cols[-1], rows[-1]
out = []
for mid, obj, name, pic, ps, stock, cps, bagname, boats, text in M:
    x0 = 926 if (ps[0][0] in FIRST) != bool(ps[0][1]) else 464.5
    for i, (p, t) in enumerate(ps):
        assert (926 if (p in FIRST) != bool(t) else 464.5) == (x0 if i % 2 == 0 else 1390.5 - x0), (mid, ps)
    img = 'map-' + ''.join(p.lower() + ('r' if t else '') for p, t in ps)
    full = os.path.join(D, 'mission-' + mid + '.png')
    MW = 12000
    if not os.path.exists(full) or not os.path.exists(os.path.join(OUT, img + '.webp')):
        m = Image.new('RGB', (MW, 9000))
        for i, (p, t) in enumerate(ps):
            im = Image.open(os.path.join(D, PANEL[p])).convert('RGB').resize((6000, 9000))
            if t: im = im.rotate(180)
            m.paste(im, (6000 * i, 0))
        if not os.path.exists(full): m.save(full)
        m.resize((2400, 1800), Image.LANCZOS).save(os.path.join(OUT, img + '.webp'), 'WEBP', quality=82, method=6)
    ej = os.path.join(D, 'edges-' + mid + '.json')
    if not os.path.exists(ej): subprocess.run([sys.executable, os.path.join(HERE, 'map-edges.py'), full, ej, os.path.join(D, 'overlay-' + mid + '.png'), str(x0), 'beach'], check=True)
    edges = json.load(open(ej)); wires = json.load(open(ej + '.wire.json'))
    feats = {}; traps = set(); drops = set()
    for i, (p, t) in enumerate(ps):
        for kind, x, y, e in LOCAL.get(p, []):
            if t: x, y, e = 6000 - x, 9000 - y, None if e is None else (e + 3) % 6
            r, c = hexat(x + 6000 * i, y, x0); k = '%d,%d' % (r, c)
            if e is not None: drops.add((k, e))
            elif kind == 'T': traps.add(k)
            else: feats[k] = kind
    feats.update(EXTRA.get(mid, {}))
    if cps is None: cps = bagpoints(bagname, PANEL[ps[0][0]], x0)
    beach = [i for i, (p, t) in enumerate(ps) if p.startswith('TB')]
    # the sheet's Spawn Areas: its bars and circles, blue Team 1, red Team 2
    sim = Image.open(os.path.join(PICS, '%d.png' % pic)).convert('RGB'); bx1, by1, bx2, by2 = picbox(sim)
    def col(r, c):
        x, y = center(r, c, x0)
        px = sim.getpixel((int(bx1 + x / MW * (bx2 - bx1)), int(by1 + y / 9000 * (by2 - by1))))
        if px[2] > 190 and px[0] < 90 and px[1] < 170: return 0
        if px[0] > 180 and px[1] < 60 and px[2] < 60: return 1
        return None
    rows = []; hexes = []; areas = [[], []]
    for r in range(-1, 12):
        for c in range(-2, int(MW / DX) + 2):
            x, y = center(r, c, x0)
            if not (-50 < x < MW + 50): continue
            k = '%d,%d' % (r, c)
            kind = feats.get(k, '.')
            onbeach = int(x // 6000) in beach
            if kind == 'A' or any(feats.get('%d,%d' % n, '.') == 'A' for n in [(r, c - 1), (r, c + 1)] + [(r + d, c + e) for d in (-1, 1) for e in ((-1, 0) if r % 2 == 0 else (0, 1))]):
                # the Anchorages' red rings close nothing to a Troop Transport (and their Water to every other unit)
                e = [None if v == 'red' else v for v in edges.get(k, [None] * 6)]
            else:
                e = edges.get(k, [None] * 6)
            e = [None if (k, i) in drops else v for i, v in enumerate(e)]
            s = ''.join({'red': 'X', 'green': 'H', 'yellow': 'L', None: '.', 'purple': '.', 'brown': '.'}[v] for v in e)
            if kind in 'BCDKF': s = '......'
            t = 'B' if kind == 'K' else (kind if kind in 'WBCDISAF' else '.')
            w = [i for i, v in enumerate(wires.get(k, [False] * 6)) if v and e[i] != 'red'] if onbeach else []
            ex = ('T' if k in traps else '') + cps.get(k, '') + ('p' + ''.join(map(str, w)) if w else '') + ('t' if TOWER.get(mid) == k else '')
            rows.append('"%d %d %s %s %s"' % (r + 1, c + 1, t, s, ex or '-'))
            hexes.append((r, c, t))
            team = col(r, c) if 0 <= y < 9000 else None
            if team is not None and t not in 'WA':
                areas[team].append((c, r))
                if r in (0, 10):
                    for rr in ((r - 1,) if r == 0 else (r + 1,)):
                        for cc in (c, c + (1 if rr % 2 == 0 else -1) if False else c):
                            pass
    # a bar on the first or last full row takes the half row behind it too, under its width
    for team, back in ((0, -1), (1, 11)):
        bar = [(c, r) for (c, r) in areas[team] if r in (0, 10)]
        if bar:
            xs = [center(r, c, x0)[0] for (c, r) in bar]
            lo, hi = min(xs) - DX / 2, max(xs) + DX / 2
            for (r, c, t) in hexes:
                if r == back and t not in 'WA' and lo <= center(r, c, x0)[0] <= hi and (c, r) not in areas[team]: areas[team].append((c, r))
    lost = [k for k in cps if not any('%d,%d' % (h[0], h[1]) == k for h in hexes)]
    assert not lost, (mid, lost)
    nw = sum(r.count('p') for r in rows)
    print(mid, ps, 'x0', x0, len(rows), 'rows', len(cps), 'points', 'areas', [len(a) for a in areas], 'wire edges', sum(len(r.split(' ')[4].split('p')[1].rstrip('t"')) if 'p' in r.split(' ')[4] else 0 for r in rows),
          'sea', sum(1 for h in hexes if h[2] == 'S'), 'anchor', sum(1 for h in hexes if h[2] == 'A'), 'fort', sum(1 for h in hexes if h[2] == 'F'))
    out.append((mid, obj, name, ps, x0, rows, areas, img, stock, boats, text))
with open(os.path.join(OUT, 'missions-printing.scala.txt'), 'w') as f:
    for mid, obj, name, ps, x0, rows, areas, img, stock, boats, text in out:
        args = ['x0 = 92.9'] if x0 != 926 else []
        args.append('areas = %sRows.areas' % obj)
        if boats is not None: args.append('boats = |($(%s))' % ', '.join(map(str, boats)))
        bs = set((int(x.strip('"').split(' ')[0]) - 1, int(x.strip('"').split(' ')[1]) - 1) for x in rows if x.strip('"').split(' ')[2] in ('B', 'C'))
        def nb(r, c): return [(r, c - 1), (r, c + 1)] + [(r + d, c + e) for d in (-1, 1) for e in ((-1, 0) if r % 2 == 0 else (0, 1))]
        multi = any(q in bs for (r, c) in bs for q in nb(r, c))
        f.write('// %s: %s\n' % (name, ' | '.join(p + (' turned round' if t else '') for p, t in ps)))
        f.write('object %s extends MapData("%s", "%s", "%s", (%d, %d, %d), %s, %sRows.rows%s)\n\n' % (obj, mid, name, img, stock[0], stock[1], stock[2],
            'true' if multi else 'false', obj, ''.join(', ' + a for a in args)))
        f.write('object %sRows {\n' % obj)
        f.write('    val areas = $(%s)\n\n' % ', '.join('$(' + ', '.join('Hex(%d, %d)' % (c + 1, r + 1) for c, r in sorted(a, key=lambda h: (h[1], h[0]))) + ')' for a in areas))
        f.write('    val rows = $(\n        %s\n    )\n}\n\n' % ',\n        '.join(rows))
    f.write('// choices\n')
    for mid, obj, name, ps, x0, rows, areas, img, stock, boats, text in out:
        lbl = ', '.join(p + ('r' if t else '') for p, t in ps)
        f.write('case object %sMap extends MapChoice(%s, "3rd printing, %s: %s. Starting stockpile %d Manpower, %d Munitions, %d Fuel.")\n' % (
            obj, obj, lbl, text[0].upper() + text[1:], stock[0], stock[1], stock[2]))
    f.write('// all: ' + ', '.join(o[1] + 'Map' for o in out) + '\n')
