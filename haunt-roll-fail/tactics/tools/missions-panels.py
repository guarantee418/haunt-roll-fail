#!/usr/bin/env python3
# The Mission Booklet's missions on one or three of the core panels (3.0 Skirmish Core Missions: The River on MR1, Frozen Pond on
# AF2; 5.1 3 Panel Core (x2) Missions: Between Two Rivers, MR2 | MR1 | MR2, and Ardennes Forest, AF2 | AF1 | AF2), built as
# missions.py builds the two-panel ones: missions-panels.py <dir> <out>, <dir> holding the four panels as tactics-art.py fetch names
# them; <out> gets map-<panels>.webp (1200 px a panel) and missions-panels.scala.txt. The features read by hand on Moro River and
# Ardennes Forest are carried over with their panels, the cover lines read again by map-edges.py. The mod has no bags for these
# missions, so their Control Points were read off the booklet's pictures (pages 6 and 18), each token's centre mapped to its hex.
import sys, os, json, subprocess
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
# the same hand-read features as missions.py: (left panel, right panel, X0, {kind: hexes})
BASE = [
    ('MR1', 'MR2', 926, {'W': {'-1,2', '0,2', '4,4', '5,5', '6,7', '10,10', '11,11'}, 'B': {'4,3', '5,3', '5,0', '5,7', '6,8', '5,10'},
                         'D': {'6,5', '4,6'}, 'T': {'1,4', '6,2', '4,5', '4,9', '6,6', '9,6'},
                         'drop': {'1,-1/3', '8,0/3', '3,7/5', '-1,10/4', '11,-1/1'}}),
    ('AF2', 'AF1', 926, {'B': {'3,-1', '3,11', '5,0', '5,10', '6,2', '7,1', '7,8', '7,9'}, 'C': {'2,8', '2,9', '3,7', '3,8', '4,9'},
                         'D': {'1,10', '4,2', '5,7', '7,3', '7,11', '9,1'}, 'I': {'4,5', '4,7', '5,3', '5,4', '5,5', '5,6', '5,8', '6,4', '6,6', '7,4'}}),
]
# id, panels left to right, Control Points ("row col" as in the rows: resources)
MISSIONS = [
    ('river', ['MR1'], {'4 0': 'MU', '4 5': 'VM', '5 4': 'MF', '6 1': 'VF', '6 4': 'VV', '7 6': 'MF', '8 0': 'MU', '8 4': 'VM'}),
    ('pond', ['AF2'], {'4 0': 'MF', '4 5': 'MF', '5 3': 'VU', '6 1': 'VF', '7 6': 'VU', '8 0': 'MU', '8 4': 'MU', '9 2': 'VF'}),
    ('rivers3', ['MR2', 'MR1', 'MR2'], {'1 7': 'MM', '2 4': 'V', '2 10': 'VF', '3 7': 'VU', '4 9': 'VU', '5 7': 'M', '6 4': 'V', '8 6': 'F',
        '9 9': 'VF', '10 6': 'VM', '11 4': 'MU', '1 13': 'F', '1 16': 'MM', '3 13': 'U', '3 16': 'MF', '5 16': 'MU', '7 11': 'UU',
        '7 14': 'VU', '9 16': 'VM', '11 11': 'V', '11 14': 'FF'}),
    ('ardennes3', ['AF2', 'AF1', 'AF2'], {'4 1': 'U', '4 3': 'F', '4 5': 'VM', '3 10': 'V', '4 8': 'V', '5 10': 'VV', '6 0': 'M', '6 7': 'VF',
        '7 4': 'M', '8 1': 'MU', '8 6': 'MF', '8 9': 'VU', '4 12': 'VM', '4 14': 'F', '4 18': 'U', '6 12': 'VF', '6 16': 'M', '8 11': 'VU',
        '8 14': 'MF', '8 17': 'MU'}),
]
FIRST = {'MR1', 'AF2'}
def x0of(left): return 926 if left in FIRST else 464.5
def carried(panels, x0):
    f = {}
    for bl, br, bx0, feats in BASE:
        for kind, hs in feats.items():
            for k in hs:
                k, e = (k.split('/') + [None])[:2]
                r, c = map(int, k.split(','))
                x, y = center(r, c, bx0)
                panel, local = (bl, x) if x < 6000 else (br, x - 6000)
                for i, p in enumerate(panels):
                    if p != panel: continue
                    nr, nc = hexat(local + 6000 * i, y, x0)
                    if e is None: f['%d,%d' % (nr, nc)] = kind
                    else: f.setdefault('drop', set()).add(('%d,%d' % (nr, nc), int(e)))
    return f
out = []
for mid, panels, pts in MISSIONS:
    x0 = x0of(panels[0])
    W = 6000 * len(panels)
    m = Image.new('RGB', (W, 9000))
    for i, p in enumerate(panels): m.paste(Image.open(os.path.join(D, PANEL[p])).convert('RGB'), (6000 * i, 0))
    full = os.path.join(D, 'mission-' + mid + '.png')
    if not os.path.exists(full): m.save(full)
    m.resize((W // 5, 1800), Image.LANCZOS).save(os.path.join(OUT, 'map-' + ''.join(p.lower() for p in panels) + '.webp'), 'WEBP', quality=82, method=6)
    ej = os.path.join(D, 'edges-' + mid + '.json')
    if not os.path.exists(ej): subprocess.run([sys.executable, os.path.join(HERE, 'map-edges.py'), full, ej, os.path.join(D, 'overlay-' + mid + '.png'), str(x0)], check=True)
    edges = json.load(open(ej))
    feats = carried(panels, x0)
    rows = []
    for r in range(-1, 12):
        for c in range(-2, int(W / DX) + 2):
            x, y = center(r, c, x0)
            if not (-50 < x < W + 50): continue
            k = '%d,%d' % (r, c)
            kind = feats.get(k, '.') if isinstance(feats.get(k, '.'), str) else '.'
            e = [None if (k, i) in feats.get('drop', set()) else x for i, x in enumerate(edges.get(k, [None] * 6))]
            s = ''.join({'red': 'X', 'green': 'H', 'yellow': 'L', None: '.', 'purple': '.'}[x] for x in e)
            if kind in 'BCD': s = '......'
            t = kind if kind in 'WBCDI' else '.'
            key = '%d %d' % (r + 1, c + 1)
            extra = ('T' if kind == 'T' else '') + pts.get(key, '')
            rows.append('"%s %s %s %s"' % (key, t, s, extra or '-'))
    missing = [k for k in pts if not any(r.startswith('"' + k + ' ') for r in rows)]
    print(mid, len(rows), 'rows', len(pts), 'points', 'missing', missing)
    out.append((mid, panels, x0, rows))
with open(os.path.join(OUT, 'missions-panels.scala.txt'), 'w') as f:
    for mid, panels, x0, rows in out:
        f.write('// %s: %s, X0 %s\n    val %s = $(\n        %s\n    )\n\n' % (mid, ' | '.join(panels), x0, mid, ',\n        '.join(rows)))
