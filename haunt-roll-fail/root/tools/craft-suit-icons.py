# Composes the missing multi-suit craft icons from the existing ones (glyphs from the 4-suit icon, frame from the frog icon).
from PIL import Image
import numpy as np, colorsys, itertools
R={'fox':(0,15),'mouse':(15,42),'rabbit':(42,75),'frog':(140,230)}
def hues(a):
    h=np.full(a.shape[:2],-1.0)
    for y in range(a.shape[0]):
        for x in range(a.shape[1]):
            r,g,b,al=a[y,x]
            if al>0:
                hh,s,v=colorsys.rgb_to_hsv(r/255,g/255,b/255)
                if s>0.2: h[y,x]=hh*360
    return h
four=np.array(Image.open('craft-suit-fox-rabbit-mouse-frog.webp')).astype(np.uint8)
h=hues(four)
glyph={}
for n,(lo,hi) in R.items():
    m=(h>=lo)&(h<hi)
    g=four.copy(); g[~m]=0
    ys,xs=np.nonzero(m)
    glyph[n]=Image.fromarray(g).crop((xs.min(),ys.min(),xs.max()+1,ys.max()+1))
frog=np.array(Image.open('craft-suit-frog.webp')).astype(np.uint8)
hf=hues(frog); fr=frog.copy(); fr[hf>=0]=0
frame=Image.fromarray(fr)
def put(im,n,cx,cy):
    g=glyph[n]; im.alpha_composite(g,(int(round(cx-g.width/2)),int(round(cy-g.height/2))))
order=['fox','rabbit','mouse','frog']
two=[(69,99),(133,101)]
three=[(73,122),(132,122),(99,62)]
import os
for k in (2,3):
    for combo in itertools.combinations(order,k):
        name='craft-suit-'+'-'.join(combo)+'.webp'
        if os.path.exists(name): continue
        im=frame.copy()
        for n,(x,y) in zip(combo,two if k==2 else three): put(im,n,x,y)
        im.save(name,'WEBP',lossless=True); print('made',name)
