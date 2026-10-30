#!/usr/bin/env python3
# Makes the Root maps' water masks (webp2/root/images/<map>/map-water.webp), which tell the map where a tap is on water
# (`waterAt` in root/ui.scala). Each mask is a quarter of the map's size: red where the water is a river or lake the
# rules use, blue where it is only scenery (the sea on some maps). The water is traced by hand below, as lines with a
# width and as polygons, in the coordinates of the map art shrunk to 800x740; maps with clear blue water refine the
# traced shapes with the art's colors (pixels near the tracing that look like the water; traced polygons there only
# outline where to look).
#
#   python3 root/tools/water-masks.py [map ...] [--preview <dir>]
#
# --preview writes each map with its mask drawn over it, to check the tracing.

import sys, os
from PIL import Image, ImageDraw, ImageFilter
import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__))
IMAGES = os.path.join(HERE, '..', '..', 'webp2', 'root', 'images')

ART = {
    'autumn': 'autumn/map-bright-new-expand',
    'winter': 'winter/map-bright',
    'lake': 'lake/map-bright',
    'mountain': 'mountain/map-bright',
    'tundra': 'tundra/map-bright',
    'gloom': 'gloom/map-bright',
    'gorge': 'gorge/map-art',
    'marsh': 'marsh/map-art',
}

# (kind, width, points) for lines, (kind, None, points) for polygons; kind 'river' (used by the rules) or 'scenery'
WATER = {
    'autumn': [
        ('river', 24, [(468,0),(462,30),(445,65),(425,100),(395,125),(355,140),(328,165),(330,195),(358,215),(400,235),(440,262),(478,282),(505,305),(518,335),(510,370),(488,400),(462,425),(452,455),(462,490),(480,520),(472,553),(445,572),(415,562),(385,545),(335,540),(285,545),(240,545),(195,553),(160,567),(115,590),(75,610),(40,620),(0,612)]),
        ('scenery', None, [(0,395),(25,400),(35,440),(30,480),(12,520),(0,540)]),
        ('scenery', None, [(0,605),(50,615),(100,635),(150,660),(210,672),(260,680),(300,700),(320,740),(0,740)]),
    ],
    'winter': [
        ('river', 34, [(0,305),(40,310),(90,330),(140,350),(190,362),(240,358),(290,350),(340,335),(390,322),(430,325),(470,345),(510,375),(560,398),(610,412),(650,420),(700,410),(750,395),(800,385)]),
    ],
    'mountain': [
        ('river', 16, [(700,112),(660,128),(620,152),(590,178),(560,210),(530,245),(505,285),(475,315),(440,320),(400,308),(360,318),(320,340),(265,372),(215,402),(170,432),(140,465),(127,520),(120,580),(108,630),(78,672),(40,692),(0,700)]),
    ],
    'tundra': [
        ('river', 26, [(800,365),(750,355),(700,340),(650,322),(600,300),(560,282),(520,268),(480,270),(455,290),(440,320)]),
        ('river', None, [(320,350),(360,335),(410,330),(450,335),(465,370),(455,410),(410,425),(360,420),(320,405),(310,380)]),
        ('river', 22, [(320,385),(280,400),(240,420),(200,440),(170,470),(150,510),(120,550),(80,580),(40,600),(0,610)]),
        ('scenery', None, [(745,0),(800,0),(800,330),(775,320),(762,250),(772,150),(752,60)]),
    ],
    'gloom': [
        ('river', 44, [(310,0),(350,40),(400,70),(440,95),(430,135),(422,175),(452,212),(492,242),(510,282),(500,322),(470,352),(430,382),(380,412),(340,452),(310,490)]),
        ('river', None, [(220,500),(330,470),(420,470),(520,520),(600,560),(660,600),(620,650),(560,690),(520,740),(100,740),(130,680),(180,620),(230,560)]),
    ],
    'gorge': [
        ('river', 16, [(45,375),(52,335),(70,300),(100,270),(130,250),(160,222),(200,202),(230,188),(262,202),(300,222),(340,242),(370,265)]),
        ('river', 16, [(800,188),(760,185),(730,170),(700,150),(660,130),(622,125),(582,150),(542,175),(502,190),(462,202),(432,230),(400,250),(370,265)]),
        ('river', 18, [(370,265),(362,300),(345,340),(322,380),(306,420),(300,470),(305,520),(320,560),(338,600),(345,640)]),
        ('river', None, [(318,630),(370,630),(380,680),(390,740),(300,740),(310,690)]),
    ],
    'marsh': [
        ('river', 26, [(620,0),(615,30),(590,60),(560,80),(520,95),(490,115),(470,150),(450,180),(435,210),(425,240),(410,270),(390,290),(350,305),(320,320),(305,345),(285,370),(255,395),(230,420),(210,450),(190,480),(170,510),(150,525),(120,540),(100,570),(90,600),(70,625)]),
        ('river', 22, [(300,365),(330,400),(355,430),(370,470),(380,510),(395,550),(405,590),(418,630),(420,665)]),
        ('scenery', None, [(0,680),(60,665),(120,650),(180,640),(220,650),(280,670),(340,670),(400,662),(470,680),(520,700),(560,720),(560,740),(0,740)]),
    ],
    'lake': [
        ('river', None, [(140,440),(200,330),(215,240),(300,230),(400,240),(395,320),(470,330),(560,330),(575,430),(610,510),(700,560),(670,615),(540,610),(440,560),(300,495),(160,490)]),
    ],
}

# maps whose water is clearly blue in the art: the tracing is widened and cut to the pixels of that color
COLOR = {'lake', 'gloom', 'marsh', 'gorge'}

def mask(name):
    art = Image.open(os.path.join(IMAGES, ART[name] + '.webp')).convert('RGB')
    w, h = art.width // 4, art.height // 4
    sx, sy = w / 800, h / 740

    def layer(kind, grow, lines=True, polygons=True):
        m = Image.new('L', (w, h), 0)
        d = ImageDraw.Draw(m)
        for k, width, pts in WATER[name]:
            if k != kind or not (lines if width else polygons):
                continue
            p = [(x * sx, y * sy) for x, y in pts]
            if width is None:
                d.polygon(p, fill=255)
                if grow:
                    d.line(p + p[:1], fill=255, width=int(grow * 2 * sx), joint='curve')
            else:
                d.line(p, fill=255, width=int((width + grow * 2) * sx), joint='curve')
                for x, y in p:
                    r = (width / 2 + grow) * sx
                    d.ellipse([x - r, y - r, x + r, y + r], fill=255)
        return np.asarray(m) > 0

    result = np.zeros((h, w, 3), dtype=np.uint8)
    small = art.resize((w, h), Image.BILINEAR).filter(ImageFilter.MedianFilter(3))
    hsv = np.asarray(small.convert('HSV')).astype(int)
    rgb = np.asarray(small).astype(int)
    blue = (hsv[..., 0] * 360 / 255 > 165) & (hsv[..., 0] * 360 / 255 < 225) & (hsv[..., 1] > 50) & (rgb[..., 2] > rgb[..., 0] + 20)

    for kind, channel in (('scenery', 2), ('river', 0)):
        m = layer(kind, 0)
        if name in COLOR:
            # polygons only where the art is blue; lines also along their middle, where a bridge or a shadow may cross
            m = (layer(kind, 12) & blue) | layer(kind, -6, polygons=False)
        result[m] = 0
        result[m, channel] = 255

    return art, Image.fromarray(result)

def main():
    args = sys.argv[1:]
    preview = None
    if '--preview' in args:
        i = args.index('--preview')
        preview = args[i + 1]
        del args[i:i + 2]
    for name in args or list(WATER):
        art, m = mask(name)
        m.save(os.path.join(IMAGES, name, 'map-water.webp'), lossless=True)
        if preview:
            a = art.resize(m.size).convert('RGBA')
            o = m.convert('RGBA')
            o.putalpha(Image.fromarray((np.asarray(m).max(axis=2) > 0).astype(np.uint8) * 140))
            a.alpha_composite(o)
            a.convert('RGB').save(os.path.join(preview, name + '.png'))
        print(name, m.size, os.path.getsize(os.path.join(IMAGES, name, 'map-water.webp')), 'bytes')

main()
