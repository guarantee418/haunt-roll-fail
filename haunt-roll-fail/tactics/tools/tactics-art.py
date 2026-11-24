#!/usr/bin/env python3
# Makes the Company of Heroes ("tactics") images in webp2/tactics/images/ from the Tabletop Simulator mod
# "Company of Heroes - 2nd Edition" (Steam workshop 2314664989).
#
#   tactics-art.py fetch <dir>          downloads the mod's save (BSON) and the images used here into <dir>, named by
#                                       the hash part of their Steam URL
#   tactics-art.py make <dir> <out>     writes the images into <out> (webp2/tactics/images)
#
# The map is the two Moro River panels (MR1 and MR2, 6000x9000 each) side by side at a fifth of their size. The unit
# tokens are the portraits on the Building Boards in a ring of the faction's color (a circle for Infantry, a rounded square
# for Light Vehicles, a square for Heavy Vehicles). The damage, die and status icons are cut from the dice textures.

import sys, os, json, math, subprocess
from PIL import Image, ImageDraw, ImageFilter, ImageOps

Image.MAX_IMAGE_PIXELS = None

FILES = {
    'mr1': 'DFF22E047EEC2B3D162B3A1A384815F403ED3C05',
    'mr2': '9CA2558A6336BADA801A44D1AA70518520F19CDE',
    'us-1': '3961DCD195A7A7AC3F2A97E8A669E60C33EEB00F',
    'us-2': '91D1559730CC0E39B2B1504A960E5BAA7FEFEF90',
    'us-3': '4C8CCB14A5C72EDE1A34008803F573C6EDC51294',
    'wehrmacht-1': 'C4E7F9A31FAE610FEDD3E7654533A51A9E64CE9D',
    'wehrmacht-2': 'AE37813EFFDCCDE4CF4DAAFAD462ED32F0E8C119',
    'wehrmacht-3': 'AC792703B43C9B650A4498E751E3DBCBE57B6289',
    'fuel': 'D460F15B813E4215F438753ECD8BF18997A0027A',
    'manpower': 'FE68E027387D8B0CE1CB90CD6C54985FB2359B5A',
    'munitions': '84AA70409E802FC5F6C55B00AC2CA09C6E4BA4D5',
    'vp': 'F656625625C74E8EFDD086FBAFDB1AABC921B481',
    'status-a': 'D3729401FF851DE0635B70E7DDB6853D900D0368',
    'status-b': '8D1EAD98080546BF5BB7D8E679E286B6FDD2FFEE',
    'damage': 'CAB8FFB6A4AB01BF4032B4C709870E5136B0C4B7',
    'ops': '451C2F9A1352DD299B69AEC1FB30C16EB68F4149',
    'ops-back': '6E7EAE4C462BECAE0B39302F20024E468A260674',
}

COLORS = {'us': (111, 154, 62), 'wehrmacht': (201, 66, 59)}

# the units on each Building Board, top to bottom, with their kind
BOARDS = {
    'us-1': [('riflemen', 'G'), ('us-mortar', 'G'), ('jeep', 'Y')],
    'us-2': [('fifty-cal', 'G'), ('at-gun', 'G'), ('greyhound', 'Y')],
    'us-3': [('major', 'G'), ('sherman', 'R'), ('wolverine', 'R')],
    'wehrmacht-1': [('grenadiers', 'G'), ('mg42', 'G'), ('kubelwagen', 'Y')],
    'wehrmacht-2': [('panzer-grenadiers', 'G'), ('wm-mortar', 'G'), ('sdkfz-222', 'Y')],
    'wehrmacht-3': [('ostwind', 'R'), ('panzer-iv', 'R'), ('brummbar', 'R')],
}

# the portraits' top edges on a 1350x1650 board, row by row
ROWS = [300, 725, 1160]

# the six faces of a 2048 die texture: three columns, two rows
def cell(img, col, row):
    w, h = img.size
    xs = [(20, 640), (710, 1330), (1400, 2020)]
    ys = [(710, 1330), (1400, 2020)]
    k = w / 2048
    x0, x1 = xs[col]
    y0, y1 = ys[row]
    return img.crop((int(x0 * k), int(y0 * k), int(x1 * k), int(y1 * k)))

def fetch(d):
    os.makedirs(d, exist_ok=True)
    import urllib.request, urllib.parse
    data = urllib.parse.urlencode({'itemcount': 1, 'publishedfileids[0]': '2314664989'}).encode()
    meta = json.load(urllib.request.urlopen('https://api.steampowered.com/ISteamRemoteStorage/GetPublishedFileDetails/v1/', data))
    url = meta['response']['publishedfiledetails'][0]['file_url']
    save = os.path.join(d, 'save.bin')
    subprocess.run(['curl', '-sL', '-o', save, url], check=True)
    import bson
    text = json.dumps(bson.decode(open(save, 'rb').read()), default=str)
    import re
    urls = set(re.findall(r'https?://[^"\\ ]+', text))
    for name, h in FILES.items():
        u = [x for x in urls if h in x][0]
        subprocess.run(['curl', '-sL', '-o', os.path.join(d, h), u], check=True)
        print(name, u)

def webp(img, path, q=88, lossless=False):
    img.save(path, 'WEBP', quality=q, lossless=lossless, method=6)

def hexagon(w, h, outline, fill, width):
    img = Image.new('RGBA', (int(w) + 2, int(h) + 2), (0, 0, 0, 0))
    d = ImageDraw.Draw(img)
    cx, cy = img.size[0] / 2, img.size[1] / 2
    pts = [(cx, cy - h / 2), (cx + w / 2, cy - h / 4), (cx + w / 2, cy + h / 4), (cx, cy + h / 2), (cx - w / 2, cy + h / 4), (cx - w / 2, cy - h / 4)]
    if fill:
        d.polygon(pts, fill=fill)
    if outline:
        d.line(pts + [pts[0]], fill=outline, width=width, joint='curve')
    return img

def token(portrait, kind, color):
    size = 240
    p = portrait.convert('RGB').resize((size, size), Image.LANCZOS)
    mask = Image.new('L', (size, size), 0)
    d = ImageDraw.Draw(mask)
    ring = Image.new('RGBA', (size, size), (0, 0, 0, 0))
    r = ImageDraw.Draw(ring)
    if kind == 'G':
        d.ellipse((8, 8, size - 8, size - 8), fill=255)
        r.ellipse((4, 4, size - 4, size - 4), outline=color + (255,), width=18)
        r.ellipse((0, 0, size - 1, size - 1), outline=(20, 20, 20, 255), width=4)
    else:
        rad = {'Y': 60, 'P': 36}.get(kind, 14)
        d.rounded_rectangle((8, 8, size - 8, size - 8), radius=rad, fill=255)
        r.rounded_rectangle((4, 4, size - 4, size - 4), radius=rad, outline=color + (255,), width=18)
        r.rounded_rectangle((0, 0, size - 1, size - 1), radius=rad, outline=(20, 20, 20, 255), width=4)
        heart = {'Y': (245, 196, 30), 'R': (210, 40, 40), 'P': (140, 70, 170)}[kind]
        r.rounded_rectangle((size // 2 - 40, size - 26, size // 2 + 40, size - 10), radius=6, fill=heart + (255,), outline=(20, 20, 20, 255), width=2)
    out = Image.new('RGBA', (size, size), (0, 0, 0, 0))
    out.paste(p, (0, 0), mask)
    out.alpha_composite(ring)
    return out.resize((160, 160), Image.LANCZOS)

def white_to_alpha(img):
    img = img.convert('RGBA')
    px = img.load()
    for y in range(img.size[1]):
        for x in range(img.size[0]):
            r, g, b, a = px[x, y]
            m = min(r, g, b)
            if m > 150:
                px[x, y] = (r, g, b, max(0, int(255 * (255 - m) / 105)))
    return img

def make(d, out):
    os.makedirs(out, exist_ok=True)
    def src(name):
        return Image.open(os.path.join(d, FILES[name]))

    # the map
    a = src('mr1').convert('RGB')
    b = src('mr2').convert('RGB')
    m = Image.new('RGB', (12000, 9000))
    m.paste(a, (0, 0))
    m.paste(b, (6000, 0))
    webp(m.resize((2400, 1800), Image.LANCZOS), os.path.join(out, 'map.webp'), q=82)

    # hex overlays, the size of the map's hexes (184.6 x 200.4)
    W, H = 184.6, 200.4
    webp(hexagon(W - 6, H - 6, (255, 244, 200, 255), (255, 244, 200, 60), 6), os.path.join(out, 'hex-select.webp'), lossless=True)
    webp(hexagon(W - 6, H - 6, (255, 60, 40, 255), (255, 60, 40, 50), 6), os.path.join(out, 'hex-target.webp'), lossless=True)
    for f, c in COLORS.items():
        webp(hexagon(W - 2, H - 2, c + (150,), c + (55,), 3), os.path.join(out, 'hex-spawn-' + f + '.webp'), lossless=True)

    # tokens and the boards
    os.makedirs(os.path.join(out, 'token'), exist_ok=True)
    os.makedirs(os.path.join(out, 'board'), exist_ok=True)
    os.makedirs(os.path.join(out, 'emblem'), exist_ok=True)
    for board, units in BOARDS.items():
        img = src(board).convert('RGB')
        k = img.size[0] / 1350
        f = board.rsplit('-', 1)[0]
        webp(img.resize((900, 1100), Image.LANCZOS), os.path.join(out, 'board', board + '.webp'), q=85)
        for (unit, kind), top in zip(units, ROWS):
            p = img.crop((int(122 * k), int((top + 12) * k), int(316 * k), int((top + 206) * k)))
            webp(token(p, kind, COLORS[f]), os.path.join(out, 'token', unit + '-' + f + '.webp'), lossless=False, q=90)
        if board.endswith('-1'):
            e = src(board).convert('RGBA').crop((int(62 * k), int(62 * k), int(340 * k), int(300 * k)))
            webp(e.resize((160, 160), Image.LANCZOS), os.path.join(out, 'emblem', f + '.webp'), q=90)

    # resources: the mod's tokens, and a white cube for Experience
    os.makedirs(os.path.join(out, 'res'), exist_ok=True)
    for r in ['fuel', 'manpower', 'munitions', 'vp']:
        img = src(r).convert('RGBA')
        w, h = img.size
        mask = Image.new('L', img.size, 0)
        ImageDraw.Draw(mask).ellipse((w * 0.09, h * 0.09, w * 0.91, h * 0.91), fill=255)
        img.putalpha(mask)
        img = img.crop((int(w * 0.09), int(h * 0.09), int(w * 0.91), int(h * 0.91)))
        webp(img.resize((96, 96), Image.LANCZOS), os.path.join(out, 'res', r + '.webp'), q=90)
    cube = Image.new('RGBA', (96, 96), (0, 0, 0, 0))
    c = ImageDraw.Draw(cube)
    c.polygon([(48, 10), (84, 28), (48, 46), (12, 28)], fill=(250, 250, 250, 255), outline=(60, 60, 60, 255))
    c.polygon([(12, 28), (48, 46), (48, 88), (12, 70)], fill=(205, 205, 205, 255), outline=(60, 60, 60, 255))
    c.polygon([(84, 28), (48, 46), (48, 88), (84, 70)], fill=(170, 170, 170, 255), outline=(60, 60, 60, 255))
    webp(cube, os.path.join(out, 'res', 'xp.webp'), lossless=True)

    # the Damage Die's faces, and the damage icons without their white ground
    dmg = src('damage').convert('RGB')
    faces = [('faceai', 0, 0), ('faceap', 1, 0), ('facehe', 2, 0), ('faceflame', 0, 1), ('facespeed', 1, 1), ('facespent', 2, 1)]
    os.makedirs(os.path.join(out, 'face'), exist_ok=True)
    os.makedirs(os.path.join(out, 'dmg'), exist_ok=True)
    for name, x, y in faces:
        f = cell(dmg, x, y).resize((96, 96), Image.LANCZOS)
        g = Image.new('RGB', (104, 104), (60, 60, 60))
        g.paste(f, (4, 4))
        webp(g, os.path.join(out, 'face', name + '.webp'), q=90)
    for name, x, y in [('ai', 0, 0), ('ap', 1, 0), ('he', 2, 0), ('flame', 0, 1)]:
        c0 = cell(dmg, x, y)
        w0, h0 = c0.size
        f = white_to_alpha(c0.crop((int(w0 * 0.08), int(h0 * 0.08), int(w0 * 0.92), int(h0 * 0.92))).resize((96, 96), Image.LANCZOS))
        if name in ('ai', 'ap'):
            # black on the dark interface: drawn light
            px = f.load()
            for yy in range(96):
                for xx in range(96):
                    r, g, b, a = px[xx, yy]
                    px[xx, yy] = (235, 228, 210, a)
        else:
            # green and red lightened for the dark interface
            px = f.load()
            for yy in range(96):
                for xx in range(96):
                    r, g, b, a = px[xx, yy]
                    px[xx, yy] = (int(r + (255 - r) * 0.35), int(g + (255 - g) * 0.35), int(b + (255 - b) * 0.35), a)
        webp(f, os.path.join(out, 'dmg', name + '.webp'), lossless=True)

    # status icons: pinned, slow, no fire, retreat (the white flag) and HE Splash from the status die, set up drawn here; the control flags
    os.makedirs(os.path.join(out, 'status'), exist_ok=True)
    st = src('status-b').convert('RGB')
    for name, x, y in [('pinned', 0, 0), ('slow', 1, 0), ('nofire', 0, 1), ('retreat', 1, 1), ('splash', 2, 1)]:
        webp(cell(st, x, y).resize((80, 80), Image.LANCZOS), os.path.join(out, 'status', name + '.webp'), q=90)
    s = Image.new('RGBA', (80, 80), (0, 0, 0, 0))
    d = ImageDraw.Draw(s)
    d.rounded_rectangle((2, 2, 78, 78), radius=10, fill=(40, 120, 60, 255), outline=(20, 20, 20, 255), width=3)
    d.line([(18, 42), (34, 58), (62, 22)], fill=(255, 255, 255, 255), width=10, joint='curve')
    webp(s, os.path.join(out, 'status', 'setup.webp'), lossless=True)
    for f, c in COLORS.items():
        fl = Image.new('RGBA', (80, 96), (0, 0, 0, 0))
        d = ImageDraw.Draw(fl)
        d.rectangle((14, 8, 20, 92), fill=(60, 50, 40, 255))
        d.polygon([(20, 10), (74, 24), (20, 46)], fill=c + (255,), outline=(20, 20, 20, 255))
        webp(fl, os.path.join(out, 'status', 'flag-' + f + '.webp'), lossless=True)

    # the Operations deck (Full Game Rules): its seven cards, a 4x2 sheet of 600x825 cards, and its back
    os.makedirs(os.path.join(out, 'ops'), exist_ok=True)
    ops = src('ops').convert('RGB')
    for i in range(7):
        c = ops.crop(((i % 4) * 600, (i // 4) * 825, (i % 4 + 1) * 600, (i // 4 + 1) * 825))
        webp(c.resize((300, 412), Image.LANCZOS), os.path.join(out, 'ops', 'ops-%d.webp' % i), q=85)
    webp(src('ops-back').convert('RGB').resize((300, 412), Image.LANCZOS), os.path.join(out, 'ops', 'ops-back.webp'), q=85)

if __name__ == '__main__':
    if sys.argv[1] == 'fetch':
        fetch(sys.argv[2])
    else:
        make(sys.argv[2], sys.argv[3])
