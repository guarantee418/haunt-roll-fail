#!/usr/bin/env python3
# Reads the colored lines along each hex edge of the Moro River map (two panels side by side, 12000x9000, as
# tactics-art.py joins them before scaling): map-edges.py <map.png> <edges.json> <overlay.png>. The overlay marks what was
# found, to check by eye; map-data.py then applies the corrections and writes the rows of Board.data.
import sys, json, math, numpy as np
from PIL import Image, ImageDraw
Image.MAX_IMAGE_PIXELS=None
img=Image.open(sys.argv[1]).convert('RGB')
im=np.asarray(img).astype(np.int32)
H,W,_=im.shape
# X0: the first column's center in even rows (926 for Moro River's MR1 + MR2; 464.5 when the panels lie the other way, as their
# hexes then sit half a hex further left); map-edges.py <map.png> <edges.json> <overlay.png> [X0]
X0=float(sys.argv[4]) if len(sys.argv) > 4 else 926
Y0,DX,S=741,923.0,501.0
DY=1.5*S
def center(r,c): return (X0+c*DX+(r%2)*DX/2, Y0+r*DY)
def corners(cx,cy):
    return [(cx,cy-S),(cx+DX/2,cy-S/2),(cx+DX/2,cy+S/2),(cx,cy+S),(cx-DX/2,cy+S/2),(cx-DX/2,cy-S/2)]
def cls(p):
    r,g,b=p
    if r>g+55 and r>b+55 and g<110: return 'red'
    if g>r+35 and g>b+55: return 'green'
    if r>105 and g>105 and b<min(r,g)-60 and abs(r-g)<40: return 'yellow'
    if b>g+25 and r>g+25: return 'purple'
    return None
out={}
for r in range(-1,13):
    for c in range(-1,int(W/DX)+2):
        cx,cy=center(r,c)
        if not (-S*0.3<cx<W+S*0.3 and -S*0.3<cy<H+S*0.3): continue
        cs=corners(cx,cy); res=[]
        for i in range(6):
            (ax,ay),(bx,by)=cs[i],cs[(i+1)%6]
            mx,my=(ax+bx)/2,(ay+by)/2
            nx,ny=cx-mx,cy-my; L=math.hypot(nx,ny); nx/=L; ny/=L
            def frac(ds):
                counts={}; tot=0
                for d in ds:
                    for t in np.linspace(0.15,0.85,15):
                        x=ax+(bx-ax)*t+nx*d; y=ay+(by-ay)*t+ny*d
                        xi,yi=int(x),int(y)
                        if 0<=xi<W and 0<=yi<H:
                            tot+=1
                            k=cls(im[yi,xi])
                            if k: counts[k]=counts.get(k,0)+1
                return {k:v/tot for k,v in counts.items()} if tot else {}
            near=frac((14,24,34,44)); far=frac((85,100,115))
            best=None; bv=0
            for k,v in near.items():
                dv=v-far.get(k,0)
                if v>=0.3 and dv>=0.25 and dv>bv: best=k; bv=dv
            res.append(best)
        out['%d,%d'%(r,c)]=res
json.dump(out,open(sys.argv[2],'w'))
# overlay
sc=0.2
ov=img.resize((int(W*sc),int(H*sc)))
d=ImageDraw.Draw(ov)
col={'red':(255,0,0),'green':(0,255,0),'yellow':(255,255,0),'purple':(255,0,255)}
for k,res in out.items():
    r,c=map(int,k.split(',')); cx,cy=center(r,c); cs=corners(cx,cy)
    for i,e in enumerate(res):
        if e:
            (ax,ay),(bx,by)=cs[i],cs[(i+1)%6]
            mx,my=(cx*0.25+(ax+bx)/2*0.75),(cy*0.25+(ay+by)/2*0.75)
            d.ellipse((mx*sc-7,my*sc-7,mx*sc+7,my*sc+7),fill=col[e],outline=(0,0,0))
    d.text((cx*sc-12,cy*sc-6),k,fill=(255,255,255))
ov.save(sys.argv[3])
