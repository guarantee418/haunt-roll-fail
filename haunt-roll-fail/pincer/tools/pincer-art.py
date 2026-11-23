#!/usr/bin/env python3
# Makes the Pincer images in webp2/pincer/images/ from the Tabletop Simulator mod
# "Pincer: A Gentleman's War (Updated for First Draft)" (Steam workshop 3778939778).
#
# Get the mod's save: POST itemcount=1&publishedfileids[0]=3778939778 to
# https://api.steampowered.com/ISteamRemoteStorage/GetPublishedFileDetails/v1/, download its file_url
# (BSON), decode it (pymongo's bson) to JSON, and download every URL in it into one directory,
# numbered in sorted URL order, with an index.txt of "<n> <file type> <url>" lines (as
# `pincer-art.py fetch <save.json> <dir>` does). Then `pincer-art.py make <dir> <out>`, where <out>
# is webp2/pincer/images.
#
# The board is the mod's Custom_Board image. The pieces are its 3D meshes (OBJ), rendered here from
# the front and above, flat shaded in each player color, with a dark outline. Roads and bridges are
# rendered lying along the three edge directions of a pointy-top hex.

import sys, os, re, json, math
import numpy as np
from PIL import Image, ImageFilter

COLORS = {
    'red':    (178, 38, 34),
    'green':  (38, 128, 52),
    'blue':   (44, 72, 186),
    'yellow': (222, 184, 36),
}

# image files in the mod, by the unique part of their Steam URL
URLS = {
    'board': '98AF02B9BA786538B0A2A25B29086B07549A3746',
}

# a part of each piece mesh's URL
MESHES = {
    'colony': '2DF8E45FAB53CB6BF8856C1C9F44F2FAE7A6EBD7',
    'fort': '7C1FF922823EC5E87CD0DC3C121D8E62BD31E862',
    'cannon': 'CB49AE46B77F9F5863A2B690FDA482B4CAE183AA',
    'cavalry': 'B09085467DE7B2B8AA9829C8E257078B20B71CED',
    'musketeer': '7F6EDE2C9AEB32483247F5BD180A8A705C4F2C70',
    'port': '346D507D0E124B41019CC537D8DB30AB46F51850',
    'road': '6064CB5B6582A6E580339486594B9F0532A7D930',
    'frigate': '49E3D821F92E60B59DD379BC406ED3A53747B84C',
    'bridge': '4B1E9D8136221ADA1884C1EF80F79D3D5F0E78ED',
}

# cards, seals and war cards: the image's position in the mod (index.txt numbering) and the name here
CARDS = {
    'wheat': 11, 'fish': 65, 'mustang': 9, 'lumber': 35, 'stone': 34, 'iron': 23,
    'retreat': 20, 'broadside': 21, 'fortify': 16, 'ambush': 8, 'outflank': 26, 'hideout': 38,
    'card-back': 28,
}

SEALS = {
    'founders-oath': 64, 'maritime-exchange': 6, 'stonewall-society': 13, 'crossroads-guild': 32,
    'golden-grain': 15, 'neptunes-bounty': 31, 'colonial-equine': 37, 'northern-axe': 29, 'crown-and-chisel': 39, 'iron-mark': 22,
    'royal-musket': 7, 'mounted-vanguard': 30, 'black-powder-volley': 19, 'naval-supremacy': 14,
}

WAR = {
    'gentleman-blue': 4, 'boar-red': 10, 'gentleman-red': 12, 'boar-blue': 17,
    'gentleman-green': 18, 'boar-green': 33, 'boar-yellow': 36, 'gentleman-yellow': 40,
}


def fetch(save, out):
    s = open(save).read()
    urls = sorted(set(re.findall(r'https?://[^"\\ ]+', s)))
    import urllib.request, subprocess
    os.makedirs(out, exist_ok=True)
    lines = []
    for i, u in enumerate(urls, 1):
        p = os.path.join(out, '%d.bin' % i)
        subprocess.run(['curl', '-sL', '--max-time', '60', '-o', p, u])
        t = subprocess.run(['file', '-b', p], capture_output=True, text=True).stdout.strip()
        lines.append('%d %s %s' % (i, t[:60], u))
    open(os.path.join(out, 'index.txt'), 'w').write('\n'.join(lines) + '\n')


def find(src, key):
    for line in open(os.path.join(src, 'index.txt')):
        if key in line:
            return os.path.join(src, line.split()[0] + '.bin')
    raise Exception('not found ' + key)


def load_obj(path):
    vs, fs = [], []
    for line in open(path):
        if line.startswith('v '):
            vs.append(list(map(float, line.split()[1:4])))
        elif line.startswith('f '):
            idx = [int(p.split('/')[0]) for p in line.split()[1:]]
            idx = [i - 1 if i > 0 else len(vs) + i for i in idx]
            for k in range(1, len(idx) - 1):
                fs.append((idx[0], idx[k], idx[k + 1]))
    return np.array(vs), np.array(fs)


def rot_y(a):
    c, s = math.cos(a), math.sin(a)
    return np.array([[c, 0, s], [0, 1, 0], [-s, 0, c]])


def rot_x(a):
    c, s = math.cos(a), math.sin(a)
    return np.array([[1, 0, 0], [0, c, -s], [0, s, c]])


def render(vs, fs, color, size, yaw, pitch, flat=False, outline=True):
    """Orthographic render, y up, camera looking along -z after rotation. Returns RGBA image of size."""
    v = vs - (vs.max(0) + vs.min(0)) / 2
    v = v @ rot_y(yaw).T @ rot_x(pitch).T
    ss = 3
    S = size * ss
    span = max(v[:, 0].max() - v[:, 0].min(), v[:, 1].max() - v[:, 1].min())
    k = (S * 0.92) / span
    px = (v[:, 0] - (v[:, 0].max() + v[:, 0].min()) / 2) * k + S / 2
    py = -(v[:, 1] - (v[:, 1].max() + v[:, 1].min()) / 2) * k + S / 2
    pz = v[:, 2]
    zbuf = np.full((S, S), -1e9)
    shade = np.zeros((S, S))
    mask = np.zeros((S, S), bool)
    light = np.array([-0.45, 0.65, 0.62])
    light /= np.linalg.norm(light)
    for a, b, c in fs:
        x0, y0, x1, y1, x2, y2 = px[a], py[a], px[b], py[b], px[c], py[c]
        n = np.cross(v[b] - v[a], v[c] - v[a])
        nn = np.linalg.norm(n)
        if nn == 0:
            continue
        n = n / nn
        lam = abs(n @ light)
        xmin, xmax = int(max(0, math.floor(min(x0, x1, x2)))), int(min(S - 1, math.ceil(max(x0, x1, x2))))
        ymin, ymax = int(max(0, math.floor(min(y0, y1, y2)))), int(min(S - 1, math.ceil(max(y0, y1, y2))))
        if xmax < xmin or ymax < ymin:
            continue
        det = (y1 - y2) * (x0 - x2) + (x2 - x1) * (y0 - y2)
        if abs(det) < 1e-12:
            continue
        xs, ys = np.meshgrid(np.arange(xmin, xmax + 1) + 0.5, np.arange(ymin, ymax + 1) + 0.5)
        l0 = ((y1 - y2) * (xs - x2) + (x2 - x1) * (ys - y2)) / det
        l1 = ((y2 - y0) * (xs - x2) + (x0 - x2) * (ys - y2)) / det
        l2 = 1 - l0 - l1
        inside = (l0 >= -1e-6) & (l1 >= -1e-6) & (l2 >= -1e-6)
        if not inside.any():
            continue
        z = l0 * pz[a] + l1 * pz[b] + l2 * pz[c]
        sub = zbuf[ymin:ymax + 1, xmin:xmax + 1]
        upd = inside & (z > sub)
        sub[upd] = z[upd]
        shade[ymin:ymax + 1, xmin:xmax + 1][upd] = lam
        mask[ymin:ymax + 1, xmin:xmax + 1][upd] = True
    col = np.array(color, float)
    lum = 0.42 + 0.68 * shade
    rgb = np.clip(col[None, None, :] * lum[:, :, None] + 18 * shade[:, :, None] ** 6, 0, 255)
    alpha = mask.astype(float) * 255
    img = Image.fromarray(np.dstack([rgb, alpha]).astype(np.uint8), 'RGBA')
    if outline:
        a = img.split()[3].filter(ImageFilter.MaxFilter(2 * ss + 1))
        base = Image.new('RGBA', img.size, (20, 14, 10, 0))
        base.putalpha(a)
        base.alpha_composite(img)
        img = base
    return img.resize((size, size), Image.LANCZOS)


def trim(img, pad=2):
    bb = img.getbbox()
    if bb is None:
        return img
    l, t, r, b = bb
    return img.crop((max(0, l - pad), max(0, t - pad), min(img.width, r + pad), min(img.height, b + pad)))


def save(img, path, lossless=False):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    img.save(path, 'WEBP', quality=88, lossless=lossless, method=6)


def hexes(out):
    # overlays drawn over a hex of the board: a pointy-top hex 119.5 wide
    from PIL import ImageDraw
    ss = 4
    w, r = 120, 69
    for name, line, fill in [('hex-select', (255, 214, 90, 255), (255, 230, 140, 70)), ('hex-target', (230, 40, 30, 255), (230, 60, 40, 60))]:
        im = Image.new('RGBA', (w * ss, 2 * r * ss), (0, 0, 0, 0))
        d = ImageDraw.Draw(im)
        pts = [(w * ss / 2 + (r - 3) * ss * math.cos(math.radians(a)), r * ss + (r - 3) * ss * math.sin(math.radians(a))) for a in range(-90, 270, 60)]
        d.polygon(pts, fill=fill)
        d.line(pts + [pts[0]], fill=line, width=5 * ss, joint='curve')
        save(im.resize((w, 2 * r), Image.LANCZOS), os.path.join(out, name + '.webp'), lossless=True)


def make(src, out):
    hexes(out)
    board = Image.open(find(src, URLS['board'])).convert('RGB')
    save(board, os.path.join(out, 'map.webp'))

    for name, n in CARDS.items():
        im = Image.open(os.path.join(src, '%d.bin' % n)).convert('RGB')
        if name == 'card-back':
            im = im.resize((240, 336), Image.LANCZOS)
        save(im, os.path.join(out, 'card', name + '.webp'))

    for name, n in SEALS.items():
        im = Image.open(os.path.join(src, '%d.bin' % n)).convert('RGBA')
        im = trim(im, 0).resize((240, 240), Image.LANCZOS)
        save(im, os.path.join(out, 'seal', name + '.webp'))

    for name, n in WAR.items():
        im = Image.open(os.path.join(src, '%d.bin' % n)).convert('RGB')
        save(im.resize((262, 186), Image.LANCZOS), os.path.join(out, 'war', name + '.webp'))

    # the six resource die faces, a 3x2 sheet of 2000x2000
    sheet = Image.open(os.path.join(src, '60.bin')).convert('RGBA')
    faces = ['fish', 'iron', 'lumber', 'wheat', 'stone', 'mustang']
    for i, name in enumerate(faces):
        cx = [330, 1000, 1670][i % 3]
        cy = [1000, 1680][i // 3] - 130
        im = trim(sheet.crop((cx - 300, cy - 300, cx + 300, cy + 300)), 4).resize((160, 160), Image.LANCZOS)
        save(im, os.path.join(out, 'die', name + '.webp'))

    meshes = {k: load_obj(find(src, v)) for k, v in MESHES.items()}

    views = {
        # name: (yaw, pitch, size)
        'colony': (-0.5, 0.35, 256),
        'fort': (-0.3, 0.75, 256),
        'cannon': (-0.9, 0.3, 200),
        'cavalry': (-0.6, 0.15, 200),
        'musketeer': (-0.4, 0.12, 200),
        'port': (-0.6, 0.45, 220),
        'frigate': (-0.5, 0.25, 220),
    }
    for color, rgb in COLORS.items():
        for name, (yaw, pitch, size) in views.items():
            vs, fs = meshes[name]
            img = trim(render(vs, fs, rgb, size, yaw, pitch))
            save(img, os.path.join(out, 'piece', '%s-%s.webp' % (name, color)))
        # bridges seen from above, along the three directions across a pointy-top hex
        # a road is a plank lying on the edge between two hexes, 66 long (a hex edge is 69)
        from PIL import ImageDraw
        ss = 4
        plank = Image.new('RGBA', (74 * ss, 22 * ss), (0, 0, 0, 0))
        d = ImageDraw.Draw(plank)
        d.rounded_rectangle((3 * ss, 4 * ss, 71 * ss, 18 * ss), radius=4 * ss, fill=(20, 14, 10, 255))
        d.rounded_rectangle((5 * ss, 6 * ss, 69 * ss, 16 * ss), radius=3 * ss, fill=rgb + (255,))
        d.rounded_rectangle((7 * ss, 7 * ss, 67 * ss, 10 * ss), radius=2 * ss, fill=tuple(min(255, int(c * 1.35) + 30) for c in rgb) + (255,))
        plank = plank.resize((74, 22), Image.LANCZOS)
        for dname, deg in {'e': 90, 'ne': 150, 'se': 30}.items():
            save(trim(plank.rotate(deg, resample=Image.BICUBIC, expand=True)), os.path.join(out, 'piece', 'road-%s-%s.webp' % (color, dname)))
        for name in ['bridge']:
            vs, fs = meshes[name]
            ext = vs.max(0) - vs.min(0)
            # turn the long side along x
            if ext[2] > ext[0]:
                vs = vs @ rot_y(math.pi / 2).T
            top = render(vs, fs, rgb, 300, 0, math.pi / 2 - 0.35)
            # named by the direction from the first land hex to the second, over the water hex between them
            turns = {'e': 0, 'ne': 60, 'se': 300}
            for d, deg in turns.items():
                img = trim(top.rotate(deg, resample=Image.BICUBIC, expand=True))
                save(img, os.path.join(out, 'piece', '%s-%s-%s.webp' % (name, color, d)))


if __name__ == '__main__':
    if sys.argv[1] == 'fetch':
        fetch(sys.argv[2], sys.argv[3])
    else:
        make(sys.argv[2], sys.argv[3])
