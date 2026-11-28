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
# the 3rd printing's beach panels (Twin Beaches; map-edges.py ... <X0> beach): the brown shorelines are read too, a red line is only the dark
# red ring of a water hex or an impassable line (the whole edge), and the red barbed wire symbols across the middle of an edge (permanent barbed wire) are read apart, into
# <edges.json>.wire.json, as the cover line under them is kept
BEACH=len(sys.argv) > 5 and sys.argv[5] == 'beach'
Y0,DX,S=741,923.0,501.0
DY=1.5*S
def center(r,c): return (X0+c*DX+(r%2)*DX/2, Y0+r*DY)
def corners(cx,cy):
    return [(cx,cy-S),(cx+DX/2,cy-S/2),(cx+DX/2,cy+S/2),(cx,cy+S),(cx-DX/2,cy+S/2),(cx-DX/2,cy-S/2)]
def cls(p):
    r,g,b=p
    if r>g+55 and r>b+55 and g<110: return 'red'
    if BEACH and 55<r<120 and 40<g<100 and b<g and r-b>=22 and 5<=r-g<=30: return 'brown'
    if g>r+35 and g>b+55: return 'green'
    if r>105 and g>105 and b<min(r,g)-60 and abs(r-g)<40: return 'yellow'
    if b>g+25 and r>g+25: return 'purple'
    return None
out={}
for r in range(-1,int(H/DY)+2):
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
            # beach panels: a red line runs the whole edge, the barbed wire symbols sit only in its middle
            if BEACH and near.get('red',0)>=0.3:
                ends=0; n=0
                for t in (0.15,0.2,0.25,0.75,0.8,0.85):
                    for d in (14,24,34):
                        x=ax+(bx-ax)*t+nx*d; y=ay+(by-ay)*t+ny*d
                        xi,yi=int(x),int(y)
                        if 0<=xi<W and 0<=yi<H:
                            n+=1; ends+=cls(im[yi,xi])=='red'
                if n and ends/n<0.3: near['red']=0
            best=None; bv=0
            for k,v in near.items():
                dv=v-far.get(k,0)
                if v>=0.3 and dv>=0.25 and dv>bv: best=k; bv=dv
            res.append(best)
        out['%d,%d'%(r,c)]=res
json.dump(out,open(sys.argv[2],'w'))
if BEACH:
    def wirecol(p):
        r,g,b=p
        return 110<r<175 and 35<g<85 and b<60 and r>g+50
    wires={}
    for k in out:
        r,c=map(int,k.split(',')); cx,cy=center(r,c); cs=corners(cx,cy); ws=[]
        for i in range(6):
            (ax,ay),(bx,by)=cs[i],cs[(i+1)%6]
            mx,my=(ax+bx)/2,(ay+by)/2
            nx,ny=cx-mx,cy-my; L=math.hypot(nx,ny); nx/=L; ny/=L
            n=tot=0
            for t in np.linspace(0.38,0.62,9):
                for dd in (-25,-10,10,25):
                    x=int(ax+(bx-ax)*t+nx*dd); y=int(ay+(by-ay)*t+ny*dd)
                    if 0<=x<W and 0<=y<H:
                        tot+=1; n+=wirecol(im[y,x])
            ws.append(bool(tot>0 and n/tot>=0.25))
        wires[k]=ws
    json.dump(wires,open(sys.argv[2]+'.wire.json','w'))
# overlay
sc=0.2
ov=img.resize((int(W*sc),int(H*sc)))
d=ImageDraw.Draw(ov)
col={'red':(255,0,0),'green':(0,255,0),'yellow':(255,255,0),'purple':(255,0,255),'brown':(140,90,40)}
for k,res in out.items():
    r,c=map(int,k.split(',')); cx,cy=center(r,c); cs=corners(cx,cy)
    for i,e in enumerate(res):
        if e:
            (ax,ay),(bx,by)=cs[i],cs[(i+1)%6]
            mx,my=(cx*0.25+(ax+bx)/2*0.75),(cy*0.25+(ay+by)/2*0.75)
            d.ellipse((mx*sc-7,my*sc-7,mx*sc+7,my*sc+7),fill=col[e],outline=(0,0,0))
    d.text((cx*sc-12,cy*sc-6),k,fill=(255,255,255))
ov.save(sys.argv[3])
