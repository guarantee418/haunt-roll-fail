#!/usr/bin/env python3
# The Mission Booklet's last missions: the 7.0 square 4-panel missions and the 8.3 and 8.4 5 Way free-for-alls (two rows of two panels,
# MapData.rows 2), and the missions the Tabletop Simulator mod has no bag for (A River Too Far, Race Across the Rhine, Frozen River on 3
# and 4 panels, Mud, Winter and Summer Solstice, Race to Rome, St. Marie du Mont): missions-grid.py <dir> <out> <booklet.pdf> [ids],
# <dir> as for missions-bags.py (the panels and save.json). <out> gets map-<panels>.webp (1200 px a panel) and missions-grid.scala.txt.
# The booklet draws the square maps with their panels upright and side by side, two rows of them (the mod lays them turned a quarter
# round on the table), so their hexes are pointy-topped like every other map's; The Volga's lower row is shifted right. Each mission's
# panels are laid as the booklet's picture has them (LAYOUT: row, x in pixels, panel, turned round), their hand-read features carried
# over from missions-bags.py's BASE, the cover lines read again by map-edges.py. The Control Points: a bag's tokens, seen from the side
# of the table the booklet is (bx = z, by = x, or both negated), else the tokens found on the booklet's picture by booklet-tokens.py:
# every mission here, as the square maps' bags put their tokens where the booklet doesn't (the booklet is followed).
# The Spawn Areas, the booklet's added features and its optional Modular Bunkers (A and B hexes; setup option OptionalMapFeatures) are
# read off its pictures (pages 16, 17, 21, 22 and 26 to 31) as fractions of the map.
import sys, os, json, subprocess, importlib.util
from PIL import Image
Image.MAX_IMAGE_PIXELS = None
HERE = os.path.dirname(os.path.abspath(__file__))
spec = importlib.util.spec_from_file_location('mb', os.path.join(HERE, 'missions-bags.py'))
mb = importlib.util.module_from_spec(spec); spec.loader.exec_module(mb)
D, OUT, PDF = sys.argv[1], sys.argv[2], sys.argv[3]
ONLY = sys.argv[4:]
mb.D = D
DX, Y0, DY = mb.DX, mb.Y0, mb.DY
center, hexat = mb.center, mb.hexat
F, T = False, True
def row1(*ps): return [(0, 6000 * i, p, False) for i, p in enumerate(ps)]
def sq(a, b, c, d): return [(0, 0) + a, (0, 6000) + b, (1, 0) + c, (1, 6000) + d]
# The Volga's lower row starts 5.5 hexes in (the booklet's picture: 160 of its 380 pixels), where its hexes line up with the upper row's
def zoom(cx, cy, ox): return ((cx - ox) / 950, (cy - 20.5) / 1425)  # a point on page 31 at 200 dpi, cropped to one map
def pts(*ps): return [('P', x, y) for x, y in ps]
LR = [[('L', 0, 1)], [('R', 0, 1)]]
def tb(*a): return [[('T',) + x for x in a[0]], [('B',) + x for x in a[1]]]
# id, object, name, layout, stockpile, Control Points ('bag', name, side) or ('page', page, box at 80 dpi), spawn, extra features,
# optional features (kind K a Bunker, V a Victory Point; fractions), players, trigger, infantry, text
M = [
    ('lorraine', 'Lorraine', 'Lorraine', sq(('MR1', F), ('MR2', F), ('HL1', F), ('HL2', F)), (5, 4, 3),
        ('page', 26, (40, 233, 420, 808)), LR, [],
        [('K', (x - 40) / 380, (y - 233) / 575) for x, y in [(232, 662), (215, 637), (245, 637), (215, 687), (245, 687)]], None, None, None,
        'Moro River above Hill 331; the Spawn Areas are the left and right edges'),
    ('railsmetal', 'RailsAndMetal', 'Rails and Metal', sq(('AF2', F), ('AF1', F), ('SG1', T), ('SG2', T)), (4, 4, 4),
        ('page', 26, (460, 233, 840, 806)), LR, [], [], None, None, None,
        'the snowy forest above Stalingrad turned round; the Spawn Areas are the left and right edges'),
    ('sthilaire', 'StHilaire', 'St. Hilaire', sq(('TP1', T), ('TP2', T), ('MO1', F), ('MO2', F)), (4, 4, 4),
        ('page', 27, (40, 237, 420, 808)), [[('L', 0, 0.55)], [('R', 0.4, 1)]], [], [], None, None, None,
        'Trois Ponts turned round above the Monastery; Team 1 spawns along the upper left edge, Team 2 the lower right'),
    ('winterstorm', 'WinterStorm', 'Winter Storm', sq(('SG1', F), ('SG2', F), ('AF1', F), ('AF2', F)), (6, 4, 2),
        ('page', 28, (460, 268, 840, 840)), LR, [], [], None, None, None,
        'Stalingrad above the snowy forest; the Spawn Areas are the left and right edges'),
    ('volga', 'TheVolga', 'The Volga', [(0, 0, 'TP1', F), (0, 6000, 'TP2', F), (1, 5076.5, 'MR1', F), (1, 11076.5, 'MR2', F)], (6, 4, 2),
        ('page', 29, (95, 123, 664, 695)), LR, [], [], None, None, None,
        'Trois Ponts above Moro River, the lower row shifted right; each team spawns along both rows\' edges'),
    ('racetorome', 'RaceToRome', 'Race to Rome', sq(('MR1', F), ('MR2', F), ('MO1', F), ('MO2', F)), (6, 4, 2),
        ('page', 28, (40, 268, 420, 840)), [[('L', 0.03, 0.6)], [('R', 0.03, 0.6)]], [],
        [('K', (x - 40) / 380, (y - 268) / 572) for x, y in [(318, 690), (290, 690), (305, 717), (290, 742), (318, 742)]], None, None, None,
        'Moro River above the Monastery; the Spawn Areas are the upper left and right edges'),
    ('stmarie', 'StMarieDuMont', 'St. Marie du Mont', sq(('MO2', T), ('TP1', F), ('TP2', F), ('MO1', T)), (6, 4, 3),
        ('page', 27, (460, 237, 840, 808)), [[('L', 0.03, 0.62)], [('R', 0.03, 0.6)]], [], [], None, None, None,
        'the Monastery and Trois Ponts crosswise; the Spawn Areas are the upper left and right edges'),
    ('cologne5', 'CologneFiveFFA', 'Cologne, 5 Way Free-for-all', sq(('TP1', T), ('TP2', T), ('MO1', F), ('MO2', F)), (5, 4, 3),
        ('page', 31, (40, 268, 420, 840)),
        [pts(*[zoom(x, 78, 0) for x in (145, 218, 292, 365)]), pts(*[zoom(x, y, 0) for x, y in [(878, 78), (915, 138), (878, 318), (915, 378)]]),
         pts(*[zoom(x, y, 0) for x, y in [(878, 912), (915, 970), (878, 1030), (915, 1090)]]), pts(*[zoom(x, 1390, 0) for x in (73, 147, 367, 440)]),
         pts(*[zoom(x, y, 0) for x, y in [(37, 620), (73, 678), (73, 795), (37, 855)]])], [], [], 5, 15, None,
        'five players, each on their own: Trois Ponts turned round above the Monastery'),
    ('hill662', 'HillSixSixTwoFFA', 'Hill 662, 5 Way Free-for-all', sq(('HL1', F), ('HL2', F), ('HL1', F), ('HL2', F)), (3, 4, 5),
        ('page', 31, (465, 268, 840, 840)),
        [pts(*[zoom(x, 78, 12.5) for x in (597, 672, 745, 818)]), pts(*[zoom(x, y, 12.5) for x, y in [(930, 618), (893, 678), (930, 735), (893, 795)]]),
         pts(*[zoom(x, 1390, 12.5) for x in (597, 672, 745, 818)]), pts(*[zoom(x, y, 12.5) for x, y in [(85, 1033), (48, 1093), (85, 1153), (48, 1213)]]),
         pts(*[zoom(x, y, 12.5) for x, y in [(48, 258), (85, 318), (48, 378), (85, 438)]])], [],
        [('K',) + zoom(x, y, 12.5) for x, y in [(487, 377), (452, 437), (523, 437), (487, 1093), (452, 1033)]], 5, 15, None,
        'five players, each on their own: two Hill 331 maps, one above the other'),
    ('riverfar', 'ARiverTooFar', 'A River Too Far', row1('MO2', 'MR1', 'MR2'), (1, 2, 5), ('page', 16, (327, 585, 840, 840)),
        tb([(0, 0.31), (0.665, 1)], [(0.337, 0.717)]), [],
        [('K', (x - 327) / 513, (y - 585) / 255) for x, y in [(405, 708), (390, 688), (420, 688), (390, 732), (420, 732)]], None, None, None,
        'the Monastery and Moro River; Team 1 spawns at both top corners'),
    ('rhine', 'RaceAcrossTheRhine', 'Race Across the Rhine', row1('HL2', 'MR1', 'MR2'), (7, 1, 0), ('page', 17, (327, 315, 840, 570)),
        LR, [], [], None, None, 2, 'Hill 331 and Moro River; the Spawn Areas are the left and right edges; every player starts with 2 Infantry'),
    ('frozenriver', 'FrozenRiver', 'Frozen River', row1('SG1', 'AF2', 'AF1'), (4, 4, 4), ('page', 17, (327, 585, 840, 840)),
        tb([(0, 0.77)], [(0.425, 1)]), [], [], None, None, None, 'Stalingrad and the snowy forest, its frozen river'),
    ('frozenriver4', 'FrozenRiverFour', 'Frozen River (4 panels)', row1('SG2', 'SG1', 'AF2', 'AF1'), (4, 4, 4), ('page', 21, (80, 135, 840, 420)),
        tb([(0.02, 0.25), (0.52, 0.77)], [(0.25, 0.48), (0.72, 1)]), [], [], None, None, None,
        'Stalingrad and the snowy forest; 7 Victory Points on Ice'),
    ('mud', 'Mud', 'Mud', row1('HL2', 'MR1', 'MR2', 'HL1'), (6, 2, 4), ('page', 21, (80, 450, 840, 737)),
        tb([(0, 0.25), (0.737, 1)], [(0, 0.25), (0.737, 1)]), [], [], None, None, None, 'Moro River between two Hill 331 panels; each team spawns at both ends'),
    ('wintersolstice', 'WinterSolstice', 'Winter Solstice', row1('SG1', 'SG2', 'AF1', 'AF2'), (3, 5, 1), ('page', 22, (80, 133, 840, 420)),
        tb([(0, 0.8)], [(0.5, 1)]), [], [], None, None, None, 'Stalingrad and the snowy forest'),
    ('summersolstice', 'SummerSolstice', 'Summer Solstice', row1('MO2', 'MR1', 'MR2', 'MO1'), (2, 3, 4), ('page', 22, (80, 450, 840, 737)),
        tb([(0, 0.8)], [(0, 0.8)]), [],
        [('K', (x - 80) / 760, (y - 450) / 287) for x, y in [(168, 590), (153, 567), (183, 567), (153, 617), (183, 617)]], None, None, None,
        'Moro River between two Monastery panels'),
]
# corrections to the tokens found on a booklet picture, checked against it hex by hex ("row column": resources, "" for none; the rows'
# numbering): a token mostly hidden by its pair's other, or a patch of the art the right colour
FIX = {
    'frozenriver4': {'4 18': ''},
    'volga': {'2 7': 'VM', '7 6': 'MU', '10 10': 'VF', '15 9': 'VF', '23 12': 'VM'},
    'cologne5': {'7 6': ''},
}
W, H = mb.W, mb.H
def bagtokens(name, s):
    b = mb.bag(name); panels = []; toks = []
    for c in b['ContainedObjects']:
        u = ((c.get('CustomImage') or {}).get('ImageURL') or '').rstrip('/').split('/')[-1]; t = c['Transform']
        if c['Name'] == 'Custom_Tile' and t.get('scaleX', 1) > 5:
            if abs(t['posX']) < 60 and abs(t['posZ']) < 60: panels.append((mb.NAME.get(u, u[:6]), round(t.get('rotY', 0)) % 360, t['posX'], t['posZ']))
        elif u in mb.RES: toks.append((mb.RES[u], t['posX'], t['posZ']))
    k = 6000 / W
    ox = min(s * p[3] - W / 2 for p in panels); oy = min(s * p[2] - H / 2 for p in panels)
    spots = [((s * p[3] - W / 2 - ox) * k, (s * p[2] - H / 2 - oy) * k, p[0], (p[1] == 90) != (s == 1)) for p in panels]
    return spots, [(r, (s * z - ox) * k, (s * x - oy) * k) for r, x, z in toks]
def pairs(toks):
    used = set(); out = []
    for i, a in enumerate(toks):
        if i in used: continue
        used.add(i)
        js = [j for j in range(len(toks)) if j not in used and (toks[j][1] - a[1]) ** 2 + (toks[j][2] - a[2]) ** 2 < 600 ** 2]
        js = sorted(js, key=lambda j: (toks[j][1] - a[1]) ** 2 + (toks[j][2] - a[2]) ** 2)[:1]
        used |= set(js); ts = [a] + [toks[j] for j in js]
        out.append((''.join(sorted((t[0] for t in ts), key='VMUF'.index)), sum(t[1] for t in ts) / len(ts), sum(t[2] for t in ts) / len(ts)))
    return out
out = []
for mid, obj, name, lay, stock, cps, spawn, extra, optional, players, trigger, infantry, text in M:
    if ONLY and mid not in ONLY: continue
    rows = max(l[0] for l in lay) + 1
    n = int(-(-(max(l[1] for l in lay) + 6000) // 6000))
    MW, MH = 6000 * n, 9000 * rows
    p0 = [l for l in lay if l[0] == 0 and l[1] == 0][0]
    x0 = 926 if (p0[2] in mb.FIRST) != p0[3] else 464.5
    for r, px, p, t in lay:
        lx = 926 if (p in mb.FIRST) != t else 464.5
        off = (px + lx - x0) % DX
        assert min(off, DX - off) < 40, (mid, p, px, off)
    img = 'map-' + '-'.join(''.join(p.lower() + ('r' if t else '') for r, px, p, t in lay if r == rr) for rr in range(rows))
    full = os.path.join(D, 'mission-' + mid + '.png')
    if not os.path.exists(full) or not os.path.exists(os.path.join(OUT, img + '.webp')):
        m = Image.new('RGB', (MW, MH))
        for r, px, p, t in lay:
            im = Image.open(os.path.join(D, mb.PANEL[p])).convert('RGB')
            if t: im = im.rotate(180)
            m.paste(im, (int(round(px)), 9000 * r))
        if not os.path.exists(full): m.save(full)
        m.resize((MW // 5, MH // 5), Image.LANCZOS).save(os.path.join(OUT, img + '.webp'), 'WEBP', quality=82, method=6)
    ej = os.path.join(D, 'edges-' + mid + '.json')
    if not os.path.exists(ej): subprocess.run([sys.executable, os.path.join(HERE, 'map-edges.py'), full, ej, os.path.join(D, 'overlay-' + mid + '.png'), str(x0)], check=True)
    edges = json.load(open(ej))
    feats = {}; drops = set()
    for r, px, p, t in lay:
        for kind, x, y, e in mb.LOCAL.get(p, []):
            if t: x, y, e = 6000 - x, 9000 - y, None if e is None else (e + 3) % 6
            rr, c = hexat(x + px, y + 9000 * r, x0)
            if e is None: feats['%d,%d' % (rr, c)] = kind
            else: drops.add(('%d,%d' % (rr, c), e))
    for kind, fx, fy in extra:
        rr, c = hexat(fx * MW, fy * MH, x0)
        feats['%d,%d' % (rr, c)] = kind
    if cps[0] == 'bag':
        spots, toks = bagtokens(cps[1], cps[2])
        for sx, sy, p, t in spots:
            m = [l for l in lay if abs(l[1] - sx) < 700 and abs(9000 * l[0] - sy) < 700]
            ok = m and m[0][2] == p and m[0][3] == t
            if not ok: print('  bag panel', p, 'turned' if t else '', 'at', round(sx), round(sy), 'not in the layout:', m and m[0])
        tp = pairs(toks)
    else:
        page, (bx0, by0, bx1, by1) = cps[1], cps[2]
        png = os.path.join(D, 'booklet-%d.png' % page)
        if not os.path.exists(png):
            subprocess.run(['pdftoppm', '-r', '300', '-f', str(page), '-l', str(page), '-png', '-singlefile', PDF, png[:-4]], check=True)
        tj = os.path.join(D, 'tokens-' + mid + '.json')
        subprocess.run([sys.executable, os.path.join(HERE, 'booklet-tokens.py'), png] + [str(v * 3.75) for v in (bx0, by0, bx1, by1)] +
                       [tj, os.path.join(D, 'tokens-' + mid + '.png')], check=True)
        tj = json.load(open(tj))
        # the blobs by the hex their centre is in; a kind's blobs in a hex, taken together, are one token, or two
        byhex = {}
        for q in tj['blobs']:
            x0b, y0b, x1b, y1b = q['box'][0] * MW, q['box'][1] * MH, q['box'][2] * MW, q['box'][3] * MH
            byhex.setdefault(hexat((x0b + x1b) / 2, (y0b + y1b) / 2, x0), []).append((q['k'], x0b, y0b, x1b, y1b, q['px'] * MW * MH))
        dd = tj['d'] * MW
        tp = []
        for (rr, c), qs in byhex.items():
            got = {}
            for k in 'VMUF':
                ks = [q for q in qs if q[0] == k]
                if not ks: continue
                w = max(q[3] for q in ks) - min(q[1] for q in ks); h = max(q[4] for q in ks) - min(q[2] for q in ks)
                px = sum(q[5] for q in ks)
                # not a token: a cover line's pieces or a patch of the art (little of the colour: a token is mostly its colour)
                if px < dd * dd * 0.12 or w < dd * 0.3 or h < dd * 0.3: continue
                big = max(max(q[3] - q[1], q[4] - q[2]) for q in ks)
                # a small token (about 1.2 d) with more of its colour beyond it: the one behind; a big token is round and alone
                two = (w > dd * 1.4 or h > dd * 1.4) and (big < dd * 1.45 or max(w, h) > min(w, h) * 1.15)
                got[k] = (2 if two else 1, px)
            # a point has one or two tokens: two kinds are one of each; a faint patch counts only beside a token
            if len(got) > 1:
                got = {k: (1, px) for k, (n, px) in got.items()}
            elif got and list(got.values())[0][1] < dd * dd * 0.35:
                got = {}
            rs = ''.join(k * got[k][0] for k in 'VMUF' if k in got)
            if rs: tp.append((rs,) + center(rr, c, x0))
    for k, v in FIX.get(mid, {}).items():
        rr, c = map(int, k.split(' '))
        tp = [q for q in tp if hexat(q[1], q[2], x0) != (rr - 1, c - 1)] + ([(v,) + center(rr - 1, c - 1, x0)] if v else [])
    cp = {}
    for rs, x, y in tp:
        rr, c = hexat(x, y, x0)
        k = '%d,%d' % (rr, c)
        if k in cp and len(cp[k] + rs) > 2: print('  more than two resources in', k, cp[k], rs)
        cp[k] = ''.join(sorted(cp.get(k, '') + rs, key='VMUF'.index))
    def inside(x, y, tol=60):
        return any(px - tol <= x <= px + 6000 + tol and 9000 * r - tol <= y <= 9000 * r + 9000 + tol for r, px, p, t in lay)
    def band(y):
        r = min(rows - 1, max(0, int(y // 9000)))
        xs = [px for rr, px, p, t in lay if rr == r]
        return min(xs), max(xs) + 6000
    lines = []; hexes = []
    for r in range(-1, 12 * rows):
        for c in range(-2, int(MW / DX) + 2):
            x, y = center(r, c, x0)
            if not inside(x, y): continue
            k = '%d,%d' % (r, c)
            kind = feats.get(k, '.')
            e = [None if (k, i) in drops else v for i, v in enumerate(edges.get(k, [None] * 6))]
            s = ''.join({'red': 'X', 'green': 'H', 'yellow': 'L', None: '.', 'purple': '.'}[v] for v in e)
            if kind in 'BCDK': s = '......'
            t = 'B' if kind == 'K' else (kind if kind in 'WBCDI' else '.')
            ex = ('K' if kind == 'K' else '') + ('T' if kind == 'T' else '') + cp.get(k, '')
            lines.append('"%d %d %s %s %s"' % (r + 1, c + 1, t, s, ex or '-'))
            hexes.append((r, c, x, y, t, ex))
    keys = set('%d,%d' % (h[0], h[1]) for h in hexes)
    lost = [k for k in cp if k not in keys]
    def area(parts):
        hs = []
        for p in parts:
            for (r, c, x, y, t, ex) in hexes:
                if t == 'W': continue
                lo, hi = band(y)
                ok = (p[0] == 'T' and r == -1 and p[1] * MW <= x <= p[2] * MW) or \
                     (p[0] == 'B' and r == 12 * rows - 1 and p[1] * MW <= x <= p[2] * MW) or \
                     (p[0] == 'L' and x < lo + DX * 0.75 and p[1] * MH <= y <= p[2] * MH) or \
                     (p[0] == 'R' and x > hi - DX * 0.75 and p[1] * MH <= y <= p[2] * MH) or \
                     (p[0] == 'P' and (r, c) == hexat(p[1] * MW, p[2] * MH, x0))
                if ok and (c, r) not in hs: hs.append((c, r))
        return hs
    areas = [area(a) for a in spawn]
    opt = []
    for kind, fx, fy in optional:
        rr, c = hexat(fx * MW, fy * MH, x0)
        h = [q for q in hexes if (q[0], q[1]) == (rr, c)][0]
        ex = h[5].replace('K', '')
        opt.append('"%d %d %s %s"' % (rr + 1, c + 1, 'B' if kind == 'K' else h[4], ('K' + ex) if kind == 'K' else (ex + 'V').replace('-', '')))
    bs = set((h[0], h[1]) for h in hexes if h[4] in ('B', 'C'))
    def nb(r, c): return [(r, c - 1), (r, c + 1)] + [(r + d, c + e) for d in (-1, 1) for e in ((-1, 0) if r % 2 == 0 else (0, 1))]
    multi = any(q in bs for (r, c) in bs for q in nb(r, c))
    print(mid, img, 'x0', x0, len(lines), 'hexes', len(cp), 'points', sum(len(v) for v in cp.values()), 'resources', 'lost', lost,
          'areas', [len(a) for a in areas], 'optional', opt)
    out.append(dict(mid=mid, obj=obj, name=name, lay=lay, x0=x0, rows=rows, n=n, lines=lines, areas=areas, img=img, stock=stock,
                    players=players, trigger=trigger, infantry=infantry, text=text, multi=multi, opt=opt))
with open(os.path.join(OUT, 'missions-grid.scala.txt'), 'w') as f:
    for o in out:
        args = []
        if o['x0'] != 926: args.append('x0 = 92.9')
        args.append('areas = %sRows.areas' % o['obj'])
        if o['n'] != 2: args.append('panels = %d' % o['n'])
        if o['rows'] != 1: args.append('rows = %d' % o['rows'])
        if o['players']: args.append('players = |(%d)' % o['players'])
        if o['trigger']: args.append('trigger = |(%d)' % o['trigger'])
        if o['infantry']: args.append('infantry = |(%d)' % o['infantry'])
        if o['opt']: args.append('optional = %sRows.optional' % o['obj'])
        f.write('// %s: %s (tools/missions-grid.py "%s")\n' % (o['name'], ' / '.join(', '.join(p + (' turned round' if t else '') for r, px, p, t in o['lay'] if r == rr) for rr in range(o['rows'])), o['mid']))
        st = o['stock']
        f.write('object %s extends MapData("%s", "%s", "%s", (%d, %d, %d), %s, %sRows.rows, %s)\n\n' % (o['obj'], o['mid'], o['name'], o['img'],
            st[0], st[1], st[2], 'true' if o['multi'] else 'false', o['obj'], ', '.join(args)))
        f.write('object %sRows {\n' % o['obj'])
        f.write('    val areas = $(%s)\n\n' % ', '.join('$(' + ', '.join('Hex(%d, %d)' % (c + 1, r + 1) for c, r in a) + ')' for a in o['areas']))
        if o['opt']: f.write('    val optional = $(%s)\n\n' % ', '.join(o['opt']))
        f.write('    val rows = $(\n        %s\n    )\n}\n\n' % ',\n        '.join(o['lines']))
    f.write('// choices\n')
    for o in out:
        lbl = ' / '.join(', '.join(p + ('r' if t else '') for r, px, p, t in o['lay'] if r == rr) for rr in range(o['rows']))
        st = o['stock']
        f.write('case object %sMap extends MapChoice(%s, "%s%s: %s. Starting stockpile %d Manpower, %d Munitions, %d Fuel.")\n' % (o['obj'], o['obj'],
            'Large Map Pack, ' if o['mid'] not in ('rhine',) or True else '', lbl, o['text'][0].upper() + o['text'][1:], st[0], st[1], st[2]))
    f.write('// all: ' + ', '.join(o['obj'] + 'Map' for o in out) + '\n')
