#!/usr/bin/env python3
# Moro River (2E, panels MR1 + MR2) map data for tactics/board.scala, from edges.json (map-edges.py): map-data.py <edges.json>; the coordinates here are the original grid, (row - 1, column - 1) of Hex
# with the corrections and features read by eye.
import json, sys
d=json.load(open(sys.argv[1]))
drop={('1,-1',3),('8,0',3),('3,7',5),('-1,10',4),('11,-1',1)}
water={'-1,2','0,2','4,4','5,5','6,7','10,10','11,11'}
building={'4,3','5,3','5,0','5,7','6,8','5,10'}
ruin={'6,5','4,6'}
traps={'1,4','6,2','4,5','4,9','6,6','9,6'}
points={'2,1':'VM','3,3':'MF','4,0':'VF','2,7':'FU','3,10':'UM','5,4':'VU','5,6':'VU','6,11':'VF','7,0':'UM','7,3':'FU','7,7':'MF','8,10':'VM'}
def valid(r,c):
    if r<-1 or r>11: return False
    return -1<=c<=(11 if r%2 else 12)
rows=[]
for r in range(-1,12):
    for c in range(-1,13):
        if not valid(r,c): continue
        k='%d,%d'%(r,c)
        e=d.get(k,[None]*6)
        s=''
        for i in range(6):
            x=e[i]
            if (k,i) in drop: x=None
            s+={'red':'X','green':'H','yellow':'L',None:'.','purple':'.'}[x]
        t='W' if k in water else 'B' if k in building else 'D' if k in ruin else '.'
        extra=('T' if k in traps else '')+points.get(k,'')
        rows.append('"%d %d %s %s %s"'%(r+1,c+1,t,s,extra or '-'))
print(',\n'.join(rows))
