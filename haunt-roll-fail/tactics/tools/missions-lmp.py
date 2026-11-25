#!/usr/bin/env python3
# The Mission Booklet's 2-panel LMP missions (4.1) on the Large Map Pack panels Hill 331 (HL1, HL2), Trois Ponts (TP1, TP2) and
# Stalingrad (SG1, SG2): missions-lmp.py <dir> <out>, <dir> holding the panels (named by their hashes, as the mod's save.json names
# them; the save itself as save.json); <out> gets map-<left><right>.webp and missions-lmp.scala.txt. As missions.py does for the core
# panels: the panels side by side as each mission's bag in the mod lays them (all unrotated), the cover lines read by map-edges.py, the
# Control Points from the resource tokens in the bag. The Buildings, Destroyed Buildings, Water (ringed in red), Ice and the Bunker were
# read by hand off the panels (2026-11-25; Ice: the hexes mostly ice, which the snowflake icons on their edges agree with). A panel's hexes
# line up with X0 926 or 464.5 when it lies on the left, so each mission pairs one of each. The Spawn Areas follow the booklet's spawn
# boxes (pages 10 to 14): the half hexes of the top and bottom rows under the box, or the left and right edges.
import sys, os, json, subprocess
from PIL import Image
Image.MAX_IMAGE_PIXELS = None
D, OUT = sys.argv[1], sys.argv[2]
HERE = os.path.dirname(os.path.abspath(__file__))
PANEL = {'HL1': '43EC122398E918BD6063DC597F153B5595003E35', 'HL2': '05913A192FB9B46BDCC51CB41526E456944A4F96',
         'TP1': '32F7897D7910788E4C3AD6C35F47BFF1C4162410', 'TP2': '09C63468D71609D36360BF6288D23A1BCB1ACACB',
         'SG1': '4CBB5917D1188AB3A82F68D63FA36737612B72AB', 'SG2': '943F8DBA40970061747CF47323BCC53002DDD8A0'}
FIRST = {'HL1', 'TP2', 'SG2'}  # the panels whose hexes line up with X0 926 when on the left
Y0, DX, S = 741, 923.0, 501.0
DY = 1.5 * S
def center(r, c, x0): return (x0 + c * DX + (r % 2) * DX / 2, Y0 + r * DY)
def hexat(x, y, x0):
    r = round((y - Y0) / DY)
    return (r, round((x - x0 - (r % 2) * DX / 2) / DX))
def x0of(left): return 926 if left in FIRST else 464.5
# hand-read features in a base layout: (left panel, right panel, X0, {kind: hexes}); K is a Bunker
BASE = [
    ('HL1', 'HL2', 926, {'B': {'1,2', '1,8'}, 'K': {'5,5'}, 'D': {'8,3', '9,5', '8,8'}}),
    ('TP1', 'TP2', 464.5, {'B': {'4,5', '2,11', '4,7', '8,1', '8,5', '7,7'}, 'D': {'3,2', '8,10'},
                           'W': {'2,7', '3,6', '7,6', '8,7', '10,8', '11,8'}}),
    ('SG1', 'SG2', 464.5, {'B': {'3,2', '3,5', '3,6', '6,1', '6,2', '6,8', '6,9', '5,11'}, 'D': {'1,2', '3,3', '0,10', '10,2', '8,7', '8,10'},
                           'I': {'0,12', '1,10', '1,11', '2,10', '2,11', '3,8', '3,10', '4,3', '4,7', '4,8', '4,9', '5,0', '5,1', '5,2', '5,3',
                                 '5,4', '5,5', '5,7', '5,8', '6,5', '7,4', '8,1', '8,4', '8,5', '9,0', '9,1', '9,2', '9,3', '9,4'}}),
]
# id, bag, left, right, spawn: 'sides', or the booklet's boxes in its picture's pixels (the map 632 to 1155 wide), top and bottom;
# extra Buildings (the booklet's "A hexes are multi-hex Buildings, not illustrated"), as booklet pixels (the map 645 to 1040 high)
MISSIONS = [
    ('hill331', 'Components - 5VP Hill 331 - 2 Panels  Stockpiles 6/4/2', 'HL1', 'HL2', 'sides', []),
    ('ploiesti', 'Components - 5VP - Ploiesti Outskirts - 2 Panels Stockpiles 4/4/4', 'HL2', 'HL1', ([(715, 1032)], [(755, 1075)]),
        [(815, 808), (795, 843), (1055, 808), (1075, 843)]),
    ('verrieres', 'Components - 6VP Verrieres Ridge - 2 Panels  Stockpiles 4/2/4', 'HL1', 'HL2', ([(632, 1155)], [(632, 1155)]), []),
    ('troisponts', 'Components - 5VP Trois Ponts - 2 Panels Stockpiles 4/4/3', 'TP1', 'TP2', ([(632, 1032)], [(752, 1155)]), []),
    ('caen', 'Components - 6VP Caen - 2 Panels Stockpiles 2/6/0', 'TP2', 'TP1', ([(775, 1015)], [(815, 975)]), []),
    ('carentan', 'Components - 6 VP Carentan - 2 Panels Stockpiles 6/4/4', 'TP1', 'TP2', 'sides', []),
    ('stalingrad', 'Components - 5VP Stalingrad - 2 Panels Stockpiles 2/4/0', 'SG1', 'SG2', ([(632, 905)], [(850, 1155)]), []),
    ('bulge', 'Components - 6VP Bulge - 2 Panels Stockpiles 4/1/6', 'SG2', 'SG1', ([(850, 1155)], [(650, 975)]), []),
    ('leningrad', 'Components - 6VP Leningrad - 2 Panels Stockpiles 4/6/1', 'SG1', 'SG2', 'sides', []),
    ('mongolian', 'Components - 7VP Mongolian Train Station - 2 Panels Stockpiles 3/2/2', 'SG2', 'SG1', ([(775, 1015)], [(655, 810), (975, 1130)]), []),
]
save = json.load(open(os.path.join(D, 'save.json')))
def bag(name):
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
def points(name, left, right, x0):
    b = bag(name); panels = []; toks = []
    for c in b['ContainedObjects']:
        u = ((c.get('CustomImage') or {}).get('ImageURL') or '').rstrip('/').split('/')[-1]; t = c['Transform']
        if c['Name'] == 'Custom_Tile' and t.get('scaleX', 1) > 5: panels.append((u, t['posX'], t['posZ']))
        elif u in RES: toks.append((RES[u], t['posX'], t['posZ']))
    lp = min(panels, key=lambda p: p[1])
    assert lp[0] == PANEL[left] and max(panels, key=lambda p: p[1])[0] == PANEL[right], (name, [p[0][:6] for p in panels])
    W = 26.35; H = W * 1.5
    used = set(); out = {}
    for i, a in enumerate(toks):
        if i in used: continue
        j = min((j for j in range(len(toks)) if j != i and j not in used), key=lambda j: (toks[j][1] - a[1]) ** 2 + (toks[j][2] - a[2]) ** 2)
        used |= {i, j}; b2 = toks[j]
        mx, mz = (a[1] + b2[1]) / 2, (a[2] + b2[2]) / 2
        r, c = hexat((mx - (lp[1] - W / 2)) / W * 6000, (lp[2] + H / 2 - mz) / H * 9000, x0)
        out['%d,%d' % (r, c)] = ''.join(sorted(a[0] + b2[0], key='VMUF'.index))
    return out
def carried(left, right, x0):
    f = {}
    for bl, br, bx0, feats in BASE:
        for kind, hs in feats.items():
            for k in hs:
                r, c = map(int, k.split(','))
                x, y = center(r, c, bx0)
                panel, local = (bl, x) if x < 6000 else (br, x - 6000)
                for i, p in enumerate([left, right]):
                    if p != panel: continue
                    nr, nc = hexat(local + 6000 * i, y, x0)
                    f['%d,%d' % (nr, nc)] = kind
    return f
out = []
for mid, bagname, left, right, spawn, extra in MISSIONS:
    x0 = x0of(left)
    assert x0of(right) != x0, mid
    full = os.path.join(D, 'mission-' + mid + '.png')
    img = 'map-' + left.lower() + right.lower()
    if not os.path.exists(full) or not os.path.exists(os.path.join(OUT, img + '.webp')):
        m = Image.new('RGB', (12000, 9000))
        m.paste(Image.open(os.path.join(D, PANEL[left])).convert('RGB'), (0, 0))
        m.paste(Image.open(os.path.join(D, PANEL[right])).convert('RGB'), (6000, 0))
        if not os.path.exists(full): m.save(full)
        m.resize((2400, 1800), Image.LANCZOS).save(os.path.join(OUT, img + '.webp'), 'WEBP', quality=82, method=6)
    ej = os.path.join(D, 'edges-' + mid + '.json')
    if not os.path.exists(ej): subprocess.run([sys.executable, os.path.join(HERE, 'map-edges.py'), full, ej, os.path.join(D, 'overlay-' + mid + '.png'), str(x0)], check=True)
    edges = json.load(open(ej))
    feats = carried(left, right, x0)
    for bx, by in extra:
        r, c = hexat((bx - 632) / 523 * 12000, (by - 645) / 395 * 9000, x0)
        feats['%d,%d' % (r, c)] = 'B'
    pts = points(bagname, left, right, x0)
    rows = []
    hexes = []
    for r in range(-1, 12):
        for c in range(-2, 14):
            x, y = center(r, c, x0)
            if not (-50 < x < 12050): continue
            k = '%d,%d' % (r, c)
            kind = feats.get(k, '.')
            s = ''.join({'red': 'X', 'green': 'H', 'yellow': 'L', None: '.', 'purple': '.'}[e] for e in edges.get(k, [None] * 6))
            if kind in 'BCDK': s = '......'
            t = 'B' if kind == 'K' else (kind if kind in 'WBCDI' else '.')
            extra_s = ('K' if kind == 'K' else '') + pts.get(k, '')
            rows.append('"%d %d %s %s %s"' % (r + 1, c + 1, t, s, extra_s or '-'))
            hexes.append((r, c, x, t))
    areas = None
    if spawn != 'sides':
        def inbox(boxes, r):
            return ['Hex(%d, %d)' % (c + 1, r + 1) for (rr, c, x, t) in hexes if rr == r and t != 'W' and any((a - 632) / 523 * 12000 <= x <= (b - 632) / 523 * 12000 for a, b in boxes)]
        areas = (inbox(spawn[0], -1), inbox(spawn[1], 11))
    print(mid, left, right, x0, len(rows), 'rows', len(pts), 'points', pts, 'areas', areas and (len(areas[0]), len(areas[1])))
    out.append((mid, left, right, x0, rows, areas, img))
with open(os.path.join(OUT, 'missions-lmp.scala.txt'), 'w') as f:
    for mid, left, right, x0, rows, areas, img in out:
        f.write('// %s: %s | %s, X0 %s, %s\n' % (mid, left, right, x0, img))
        if areas: f.write('    val %sAreas = $($(%s), $(%s))\n' % (mid, ', '.join(areas[0]), ', '.join(areas[1])))
        f.write('    val %s = $(\n        %s\n    )\n\n' % (mid, ',\n        '.join(rows)))
