#!/usr/bin/env python3
# Ardennes Forest (2E, panels AF2 + AF1, as the Mission Booklet's picture lays them: AF2 on the left) map data for tactics/board.scala,
# from the edges json map-edges.py writes for the joined panels: ardennes-data.py <edges.json>. The coordinates here are the original grid,
# (row - 1, column - 1) of Hex. The Buildings, Destroyed Buildings and ice icons were found by the colours of their outlines and icons
# (dark grey, purple, the cyan ice ring inside the hex it marks) and checked by eye; the Cathedral is the cross of five hexes; the
# Control Points are where the Mission Booklet's picture puts them.
import json, sys
d = json.load(open(sys.argv[1]))
building = {'3,-1', '3,11', '5,0', '5,10', '6,2', '7,1', '7,8', '7,9'}
cathedral = {'2,8', '2,9', '3,7', '3,8', '4,9'}
ruin = {'1,10', '4,2', '5,7', '7,3', '7,11', '9,1'}
ice = {'4,5', '4,7', '5,3', '5,4', '5,5', '5,6', '5,8', '6,4', '6,6', '7,4'}
points = {'1,7': 'VU', '2,10': 'UU', '3,0': 'MM', '3,5': 'FF', '4,4': 'FV', '4,11': 'MV', '6,0': 'MV', '6,7': 'FV', '7,5': 'FF',
          '7,10': 'MM', '8,1': 'UU', '9,3': 'VU'}
def valid(r, c):
    if r < -1 or r > 11: return False
    return -1 <= c <= (11 if r % 2 else 12)
rows = []
for r in range(-1, 12):
    for c in range(-1, 13):
        if not valid(r, c): continue
        k = '%d,%d' % (r, c)
        e = d.get(k, [None] * 6)
        # the cover lines of a Building's own outline aren't cover lines
        s = ''.join({'red': 'X', 'green': 'H', 'yellow': 'L', None: '.', 'purple': '.'}[x] for x in e)
        if k in building or k in cathedral or k in ruin: s = '......'
        t = 'B' if k in building else 'C' if k in cathedral else 'D' if k in ruin else 'I' if k in ice else '.'
        rows.append('"%d %d %s %s %s"' % (r + 1, c + 1, t, s, points.get(k, '') or '-'))
print(',\n'.join(rows))
