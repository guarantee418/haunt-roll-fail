#!/usr/bin/env python3
# The Mission Booklet's two-panel core missions (tactics/board.scala `Missions`), built from the four core panels: missions.py <dir> <out>.
# <dir> holds the panels as tactics-art.py fetch names them (their hashes) and the mod's save as save.json; <out> gets map-<id>.webp
# and missions.scala.txt (the rows to paste into board.scala). For each mission: the panels side by side as its bag in the mod lays
# them (every panel unrotated; when the second panel of a pair is on the left, its hexes sit half a hex further left, X0 464.5), the
# features read by hand on Moro River (map-data.py) and Ardennes Forest (ardennes-data.py) carried over with their panels, the cover
# lines read again by map-edges.py, and the Control Points from the resource tokens in the mission's bag.
import sys, os, json, subprocess, math
from PIL import Image
Image.MAX_IMAGE_PIXELS = None
D, OUT = sys.argv[1], sys.argv[2]
HERE = os.path.dirname(os.path.abspath(__file__))
PANEL = {'MR1': 'DFF22E047EEC2B3D162B3A1A384815F403ED3C05', 'MR2': '9CA2558A6336BADA801A44D1AA70518520F19CDE',
         'AF1': 'BA5B22330444EFD4368DFF43D78742B9213D8882', 'AF2': 'A02EBD6944A47B3931A2F89115F1D7B5F2FD349E'}
Y0, DX, S = 741, 923.0, 501.0
DY = 1.5 * S
def center(r, c, x0): return (x0 + c * DX + (r % 2) * DX / 2, Y0 + r * DY)
def hexat(x, y, x0):
    r = round((y - Y0) / DY)
    return (r, round((x - x0 - (r % 2) * DX / 2) / DX))
# hand-read features in their base layouts: (left panel, right panel, X0, {kind: hexes})
BASE = [
    ('MR1', 'MR2', 926, {'W': {'-1,2', '0,2', '4,4', '5,5', '6,7', '10,10', '11,11'}, 'B': {'4,3', '5,3', '5,0', '5,7', '6,8', '5,10'},
                         'D': {'6,5', '4,6'}, 'T': {'1,4', '6,2', '4,5', '4,9', '6,6', '9,6'},
                         'drop': {'1,-1/3', '8,0/3', '3,7/5', '-1,10/4', '11,-1/1'}}),
    ('AF2', 'AF1', 926, {'B': {'3,-1', '3,11', '5,0', '5,10', '6,2', '7,1', '7,8', '7,9'}, 'C': {'2,8', '2,9', '3,7', '3,8', '4,9'},
                         'D': {'1,10', '4,2', '5,7', '7,3', '7,11', '9,1'}, 'I': {'4,5', '4,7', '5,3', '5,4', '5,5', '5,6', '5,8', '6,4', '6,6', '7,4'}}),
]
# id, name, bag, left, right, stockpile, sides (spawn on the left and right edges)
MISSIONS = [
    ('moro', 'Moro River', 'Components - 6VP Moro River- 2 Panels Stockpiles 4/2/3', 'MR1', 'MR2', (4, 2, 3), False),
    ('don', 'Across the Don', 'Components - 6VP Across the Don- 2 Panels Stockpiles 5/3/3', 'MR1', 'MR2', (5, 3, 3), True),
    ('rivers', 'Between Two Rivers', 'Components - 6VP Between Two Rivers- 2 Panels Stockpiles 4/4/4', 'MR2', 'MR1', (4, 4, 4), False),
    ('ardennes', 'Ardennes Forest', 'Components - 6VP Ardennes Forest- 2 Panels Stockpiles 6/2/3', 'AF1', 'AF2', (6, 2, 3), False),
    ('frozen', 'Frozen Lake', 'Components - 6VP Frozen Lake- 2 Panels Stockpiles 6/4/3', 'AF2', 'AF1', (6, 4, 3), False),
    ('moscow', 'Road to Moscow', 'Components - 6VP Road to Moscow- 2 Panels Stockpiles 6/4/4', 'AF1', 'AF2', (6, 4, 4), True),
]
FIRST = {'MR1', 'AF2'}  # the panels whose hexes line up with X0 926 when on the left
def x0of(left): return 926 if left in FIRST else 464.5
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
def points(name, left, x0):
    b = bag(name); panels = []; toks = []
    for c in b['ContainedObjects']:
        u = ((c.get('CustomImage') or {}).get('ImageURL') or '').rstrip('/').split('/')[-1]; t = c['Transform']
        if c['Name'] == 'Custom_Tile' and t.get('scaleX', 1) > 5: panels.append((u, t['posX'], t['posZ']))
        elif u in RES: toks.append((RES[u], t['posX'], t['posZ']))
    lp = [p for p in panels if p[0] == PANEL[left]][0]
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
                k, e = (k.split('/') + [None])[:2]
                r, c = map(int, k.split(','))
                x, y = center(r, c, bx0)
                panel, local = (bl, x) if x < 6000 else (br, x - 6000)
                if panel == left: nx = local
                elif panel == right: nx = local + 6000
                else: continue
                nr, nc = hexat(nx, y, x0)
                if e is None: f['%d,%d' % (nr, nc)] = kind
                else: f.setdefault('drop', set()).add(('%d,%d' % (nr, nc), int(e)))
    return f
rows_out = []
for mid, name, bagname, left, right, stock, sides in MISSIONS:
    x0 = x0of(left)
    m = Image.new('RGB', (12000, 9000))
    m.paste(Image.open(os.path.join(D, PANEL[left])).convert('RGB'), (0, 0))
    m.paste(Image.open(os.path.join(D, PANEL[right])).convert('RGB'), (6000, 0))
    full = os.path.join(D, 'mission-' + mid + '.png')
    m.save(full)
    small = m.resize((2400, 1800), Image.LANCZOS)
    small.save(os.path.join(OUT, 'map-' + left.lower() + right.lower() + '.webp'), 'WEBP', quality=82, method=6)
    ej = os.path.join(D, 'edges-' + mid + '.json')
    if not os.path.exists(ej): subprocess.run([sys.executable, os.path.join(HERE, 'map-edges.py'), full, ej, os.path.join(D, 'overlay-' + mid + '.png'), str(x0)], check=True)
    edges = json.load(open(ej))
    feats = carried(left, right, x0)
    pts = points(bagname, left, x0)
    rows = []
    for r in range(-1, 12):
        for c in range(-2, 14):
            x, y = center(r, c, x0)
            if not (-50 < x < 12050): continue
            k = '%d,%d' % (r, c)
            kind = feats.get(k, '.') if isinstance(feats.get(k, '.'), str) else '.'
            e = [None if (k, i) in feats.get('drop', set()) else x for i, x in enumerate(edges.get(k, [None] * 6))]
            s = ''.join({'red': 'X', 'green': 'H', 'yellow': 'L', None: '.', 'purple': '.'}[x] for x in e)
            if kind in 'BCD': s = '......'
            t = kind if kind in 'WBCDI' else '.'
            extra = ('T' if kind == 'T' else '') + pts.get(k, '')
            rows.append('"%d %d %s %s %s"' % (r + 1, c + 1, t, s, extra or '-'))
    rows_out.append((mid, name, left, right, x0, stock, sides, rows, pts))
    print(mid, len(rows), 'rows', len(pts), 'points', pts)
with open(os.path.join(OUT, 'missions.scala.txt'), 'w') as f:
    for mid, name, left, right, x0, stock, sides, rows, pts in rows_out:
        f.write('// %s: %s | %s, X0 %s, stockpile %s, %s spawns\n' % (name, left, right, x0, stock, 'side' if sides else 'top and bottom'))
        f.write('    val %s = $(\n        %s\n    )\n\n' % (mid, ',\n        '.join(rows)))
