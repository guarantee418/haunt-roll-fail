# Renders a Tabletop Simulator OBJ miniature to a transparent PNG (numpy z-buffer, one directional light, dark outline).
# python3 render-mesh.py model.obj out.png r g b yaw pitch   (colors 0-1; the creature figures used yaw 30, Draugr 210, pitch 15)
import numpy as np, sys, math
from PIL import Image, ImageFilter

def load(fn):
    V=[];F=[]
    for line in open(fn, errors='ignore'):
        if line.startswith('v '):
            V.append([float(x) for x in line.split()[1:4]])
        elif line.startswith('f '):
            idx=[int(p.split('/')[0]) for p in line.split()[1:]]
            idx=[i-1 if i>0 else len(V)+i for i in idx]
            for k in range(1,len(idx)-1): F.append((idx[0],idx[k],idx[k+1]))
    return np.array(V), np.array(F)

def render(fn, color, out, yaw=30, pitch=20, size=512, ss=2, flipx=False):
    V,F=load(fn)
    V=V-(V.min(0)+V.max(0))/2
    if flipx: V[:,0]*=-1
    a=math.radians(yaw); b=math.radians(pitch)
    Ry=np.array([[math.cos(a),0,math.sin(a)],[0,1,0],[-math.sin(a),0,math.cos(a)]])
    Rx=np.array([[1,0,0],[0,math.cos(b),-math.sin(b)],[0,math.sin(b),math.cos(b)]])
    P=V@Ry.T@Rx.T
    S=size*ss
    span=max(np.ptp(P[:,0]),np.ptp(P[:,1]))
    sc=S*0.92/span
    X=(P[:,0]-(P[:,0].min()+P[:,0].max())/2)*sc+S/2
    Y=S/2-(P[:,1]-(P[:,1].min()+P[:,1].max())/2)*sc
    Z=P[:,2]
    zb=np.full((S,S),np.inf); img=np.zeros((S,S,3)); alpha=np.zeros((S,S))
    light=np.array([-0.4,0.7,-0.6]); light/=np.linalg.norm(light)
    col=np.array(color)
    for f in F:
        p=np.stack([X[f],Y[f],Z[f]],1)
        v0=V[f[1]]-V[f[0]]; v1=V[f[2]]-V[f[0]]
        n=np.cross((P[f[1]]-P[f[0]]),(P[f[2]]-P[f[0]]))
        nn=np.linalg.norm(n)
        if nn==0: continue
        n/=nn
        if n[2]>0: n=-n
        sh=0.35+0.75*max(0,np.dot(n,np.array([light[0],light[1],light[2]])*np.array([1,1,1])))
        xmin=int(max(0,math.floor(p[:,0].min()))); xmax=int(min(S-1,math.ceil(p[:,0].max())))
        ymin=int(max(0,math.floor(p[:,1].min()))); ymax=int(min(S-1,math.ceil(p[:,1].max())))
        if xmin>xmax or ymin>ymax: continue
        xs,ys=np.meshgrid(np.arange(xmin,xmax+1)+0.5,np.arange(ymin,ymax+1)+0.5)
        (x0,y0,z0),(x1,y1,z1),(x2,y2,z2)=p
        d=(y1-y2)*(x0-x2)+(x2-x1)*(y0-y2)
        if abs(d)<1e-9: continue
        l0=((y1-y2)*(xs-x2)+(x2-x1)*(ys-y2))/d
        l1=((y2-y0)*(xs-x2)+(x0-x2)*(ys-y2))/d
        l2=1-l0-l1
        m=(l0>=0)&(l1>=0)&(l2>=0)
        if not m.any(): continue
        z=l0*z0+l1*z1+l2*z2
        sub=zb[ymin:ymax+1,xmin:xmax+1]
        w=m&(z<sub)
        sub[w]=z[w]
        img[ymin:ymax+1,xmin:xmax+1][w]=np.clip(col*sh,0,1)
        alpha[ymin:ymax+1,xmin:xmax+1][w]=1
    rgba=np.dstack([img*255,alpha*255]).astype(np.uint8)
    im=Image.fromarray(rgba,'RGBA')
    # dark outline
    a=im.split()[3]
    edge=a.filter(ImageFilter.MaxFilter(7*ss//2*2+1))
    outline=Image.new('RGBA',im.size,(25,20,15,0)); outline.putalpha(edge)
    outline.alpha_composite(im)
    outline=outline.resize((size,size),Image.LANCZOS)
    outline.save(out)

if __name__=='__main__':
    fn,out,r,g,b,yaw,pitch=sys.argv[1:8]
    render(fn,(float(r),float(g),float(b)),out,float(yaw),float(pitch))
