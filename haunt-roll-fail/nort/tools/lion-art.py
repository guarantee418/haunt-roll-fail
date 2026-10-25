#!/usr/bin/env python3
# The Lion Kingdom's art (fan-made clan): emblem, clan cards, board, Lord token, Chapel, alternative starting cards.
# Built from the existing Northgard images and the video game's Lion emblem, Hildegard and Lord (expansion/lion/): python3 -I lion-art.py <webp2/nort/images> <out dir for previews>
# [<card> ...] (only those clan cards, e.g. lion-ost, leaving the rest as they are)
import sys, random
import numpy as np
from PIL import Image, ImageDraw, ImageFont, ImageFilter, ImageOps

IMG = sys.argv[1]
PRE = sys.argv[2]
random.seed(7)
np.random.seed(7)

F = '/usr/share/fonts/opentype/inter/'
def font(w, s): return ImageFont.truetype(F + w + '.otf', s)

def load(p): return Image.open(IMG + '/' + p).convert('RGBA')

def save(im, p, lossless=False):
    im.save(IMG + '/' + p, 'WEBP', quality=88, method=6, lossless=lossless)
    im.save(PRE + '/' + p.replace('/', '_').replace('.webp', '.png'))

# --- emblem: the video game's Lion clan emblem (its round icon from the Northgard wiki, expansion/lion/lion-round.webp),
# the white chalk lion taken off its gray stone
def emblem(size):
    src = load('expansion/lion/lion-round.webp')
    g = np.array(ImageOps.grayscale(src.convert('RGB'))).astype(float)
    al = np.clip((g - 112) / 95, 0, 1) * (np.array(src.split()[3]).astype(float) / 255)
    out = np.zeros(g.shape + (4,), np.uint8)
    out[..., :3] = 238
    out[..., 3] = (al * 255).astype(np.uint8)
    e = Image.fromarray(out)
    e = e.crop(e.split()[3].point(lambda v: 255 if v > 60 else 0).getbbox())
    return e.resize((size, size * e.height // e.width), Image.LANCZOS)

# Hildegard, the video game's Lion warchief (her unit icon from the Northgard wiki, expansion/lion/hildegard.webp)
def hildegard(height):
    return figure('expansion/lion/hildegard.webp', height)

# A unit icon from the wiki, cropped and scaled to height (Hildegard; the Lord, expansion/lion/lord.webp)
def figure(path, height):
    h = load(path)
    h = h.crop(h.split()[3].getbbox())
    return h.resize((height * h.width // h.height, height), Image.LANCZOS)

# Fills box with random patches of the card's own background taken from the sources
def patch_fill(im, box, sources, p=36):
    x0, y0, x1, y1 = box
    mask = Image.new('L', (p, p), 0)
    ImageDraw.Draw(mask).ellipse((2, 2, p - 3, p - 3), 255)
    mask = mask.filter(ImageFilter.GaussianBlur(5))
    for _ in range(int((x1 - x0) * (y1 - y0) / (p * p) * 6)):
        sx0, sy0, sx1, sy1 = random.choice(sources)
        sx = random.randint(sx0, sx1 - p); sy = random.randint(sy0, sy1 - p)
        tile = im.crop((sx, sy, sx + p, sy + p))
        if random.random() < 0.5: tile = ImageOps.mirror(tile)
        if random.random() < 0.5: tile = ImageOps.flip(tile)
        im.paste(tile, (random.randint(x0 - p // 2, x1 - p // 2), random.randint(y0 - p // 2, y1 - p // 2)), mask)

def spaced(d, xy, text, f, fill, spacing, anchor='mm'):
    w = sum(d.textlength(c, font=f) for c in text) + spacing * (len(text) - 1)
    x = xy[0] - w / 2 if anchor == 'mm' else xy[0]
    for c in text:
        d.text((x, xy[1]), c, font=f, fill=fill, anchor='lm')
        x += d.textlength(c, font=f) + spacing
    return w

def wrap(d, text, f, width):
    lines, cur = [], ''
    for w in text.split():
        t = (cur + ' ' + w).strip()
        if d.textlength(t, font=f) <= width: cur = t
        else: lines.append(cur); cur = w
    lines.append(cur)
    return lines

def title_banner(im, d, cx, cy, text, h=27, f=None):
    f = f or font('Inter-Bold', 15)
    w = sum(d.textlength(c, font=f) for c in text) + 2.2 * (len(text) - 1)
    d.rectangle((cx - w / 2 - 14, cy - h / 2, cx + w / 2 + 14, cy + h / 2), fill=(14, 30, 40, 255))
    spaced(d, (cx, cy), text, f, (222, 230, 234), 2.2)

def text_box(im, box, header, text):
    x0, y0, x1, y1 = box
    patch_fill(im, (x0 + 4, y0 + 4, x1 - 4, y1 - 4), [(4, 150, 40, 450), (372, 150, 408, 450)], 30)
    patch_fill(im, (x0 + 4, y0 + 4, x1 - 4, y1 - 4), [(4, 150, 40, 450), (372, 150, 408, 450)], 30)
    sub = im.crop(box)
    sub = Image.blend(sub, Image.new('RGBA', sub.size, (16, 34, 46, 255)), 0.55)
    im.paste(sub, box[:2])
    d = ImageDraw.Draw(im)
    d.rectangle(box, outline=(196, 206, 212), width=2)
    d.rectangle((x0 + 5, y0 + 5, x1 - 5, y1 - 5), outline=(150, 165, 172), width=1)
    fb = font('Inter-Bold', 15); ft = font('Inter-Medium', 14)
    lines = wrap(d, text, ft, x1 - x0 - 34)
    lh = 19
    total = len(lines) * lh + (header is not None) * 22
    y = (y0 + y1) / 2 - total / 2 + 9
    if header:
        d.text(((x0 + x1) / 2, y), header, font=fb, fill=(240, 240, 240), anchor='mm'); y += 22
    for l in lines:
        d.text(((x0 + x1) / 2, y), l, font=ft, fill=(236, 238, 238), anchor='mm'); y += lh

def clan_card(base, name, header, text, icon=None, fame=False, figure=None):
    im = load(base)
    if base.endswith('squirrel.webp'):
        emb, banner, tbox = (40, 150, 372, 448), (205, 129), (22, 455, 390, 605)
    else:
        emb, banner, tbox = (40, 145, 372, 448), (205, 126), (22, 455, 390, 602)
    patch_fill(im, emb, [(4, 150, 40, 450), (372, 150, 408, 450)])
    patch_fill(im, (60, 112, 352, 145), [(4, 150, 40, 450), (372, 150, 408, 450)], 24)
    if icon is not None:
        # the bar's own empty part covers its icon, then the new icon goes in the middle
        # each row of the bar takes the median color of its empty left part, with a little grain, blended in at the ends
        a = np.array(im).astype(float)
        rows = np.median(a[26:102, 40:120], axis=1)
        fill = np.repeat(rows[:, None], 240, axis=1) + (np.random.default_rng(3).random((76, 240, 1)) - 0.5) * 6
        ramp = np.clip(np.minimum(np.arange(240), 239 - np.arange(240)) / 20, 0, 1)[None, :, None]
        a[26:102, 86:326] = a[26:102, 86:326] * (1 - ramp) + fill * ramp
        im = Image.fromarray(np.clip(a, 0, 255).astype(np.uint8))
        x = 206 - sum(i.width for i in icon) // 2 - 12 * (len(icon) - 1)
        for i in icon:
            im.alpha_composite(i, (x, 64 - i.height // 2)); x += i.width + 24
    e = emblem(270)
    if figure is not None:
        # a warchief's card: the warchief standing on the text box, a faint emblem behind
        e.putalpha(e.split()[3].point(lambda v: v * 0.16))
    im.alpha_composite(e, (206 - e.width // 2, (emb[1] + emb[3]) // 2 - e.height // 2 + 6))
    if figure is not None:
        im.alpha_composite(figure, (206 - figure.width // 2, emb[3] + 14 - figure.height))
    d = ImageDraw.Draw(im)
    title_banner(im, d, *banner, name.upper())
    text_box(im, tbox, header, text)
    return im.convert('RGB')

# --- the four clan cards
# an icon taken off a card's bar: alpha by the distance from the bar's color (the crop's border), the bar unmixed from the colors
def bar_icon(im, box, soft=70):
    c = np.array(load(im).crop(box)).astype(float)[..., :3]
    border = np.concatenate([c[0], c[-1], c[:, 0], c[:, -1]])
    bg = np.median(border, axis=0)
    al = np.clip((np.sqrt(((c - bg) ** 2).sum(-1)) - 14) / soft, 0, 1)
    rgb = (c - bg * (1 - al[..., None])) / np.maximum(al[..., None], 0.05)
    out = np.dstack([np.clip(rgb, 0, 255), al * 255]).astype(np.uint8)
    i = Image.fromarray(out)
    return i.crop(i.split()[3].point(lambda v: 255 if v > 40 else 0).getbbox())
vow = bar_icon('card/clan/goat-halvards-craft.webp', (172, 36, 244, 96))
# A piece of a card's bar as it is, its edges faded into the bar it goes on (icons the color keying leaves a halo around)
def bar_strip(im, box, fade=12):
    c = load(im).crop(box)
    w, h = c.size
    x = np.minimum(np.arange(w), w - 1 - np.arange(w))[None, :]
    y = np.minimum(np.arange(h), h - 1 - np.arange(h))[:, None]
    c.putalpha(Image.fromarray((np.clip(np.minimum(x, y) / fade, 0, 1) * 255).astype(np.uint8)))
    return c
# The squirrel clan card's Flash and Recruit icons, as one piece of its bar
flash_recruit = bar_strip('card/clan/squirrel.webp', (100, 30, 296, 100))
# Cooking Mastery's Build icon (a hammer)
build = bar_icon('card/clan/squirrel-cooking-mastery.webp', (172, 30, 244, 100))
# Forged for War's combat icon with its question mark ("+? combat points")
axe = bar_icon('card/dev/forged-for-war.webp', (222, 30, 300, 104))
cards = {
    'lion': ('card/clan/squirrel.webp', 'Lion Kingdom', None, 'Recruit 1. Then you may pay up to 2 lore: recruit 1 more unit in that territory for each lore paid.', None),
    'lion-rise-of-the-twelve-peers': ('card/clan/squirrel.webp', 'Rise of the Twelve Peers', 'CLAN UPGRADE', 'Recruit 1 unit in up to 2 territories you control with a Defense Tower or Training Camp. Then you may put a Lord from your reserve on a Defense Tower you control that has none.', None),
    # Recruiting in every territory with a Chapel, in new games (LionChapelPeers, the owner, 2026-10-25)
    'lion-rise-of-the-twelve-peers-chapels': ('card/clan/squirrel.webp', 'Rise of the Twelve Peers', 'CLAN UPGRADE', 'Recruit 1 unit in each territory you control with a Chapel. Then you may put a Lord from your reserve on a Defense Tower you control that has none.', [flash_recruit, load('token/lord.webp').resize((64, 64), Image.LANCZOS)]),
    'lion-abundance': ('card/clan/squirrel-cooking-mastery.webp', 'Abundance', 'CLAN UPGRADE', 'Build. Then, if that territory has 3 or more buildings, collect its resources as if it were the Harvest phase.', None),
    # Ost replaces Abundance in new games (LionOst, the owner, 2026-10-25), after the video game's Faith tree entry
    'lion-ost': ('card/clan/squirrel-cooking-mastery.webp', 'Ost', 'CLAN UPGRADE', 'Build 1 Defense Tower for free. Until the end of the year, your Lords next to a combat you start join it: gain 2 combat points for each. Then they return to their towers.', [build, load('token/lord.webp').resize((64, 64), Image.LANCZOS)]),
    'lion-hildegards-vow': ('card/clan/squirrel-cooking-mastery.webp', "Hildegard's Vow", 'CLAN UPGRADE', 'Move 2. In each combat triggered by this card, gain 2 combat points for each of your Lords in a territory adjacent to the fight.', [vow, axe], hildegard(330)),
}
only = sys.argv[3:]
for k, (base, name, header, text, icon, *fig) in cards.items():
    if only and k not in only:
        continue
    save(clan_card(base, name, header, text, icon, figure=(fig or [None])[0]), 'card/clan/' + k + '.webp')
if only:
    print('done')
    sys.exit(0)

# --- the clan emblem token (clan picker, panels)
tok = load('clan/squirrel.webp')
a = np.array(tok)
yy, xx = np.mgrid[0:160, 0:160]
inner = (xx - 80) ** 2 + (yy - 80) ** 2 < 64 ** 2
bg = Image.new('RGBA', (160, 160), (24, 44, 58, 255))
nz = (np.random.rand(160, 160) * 18).astype(np.uint8)
bga = np.array(bg); bga[..., :3] = np.clip(bga[..., :3].astype(int) + nz[..., None] - 9, 0, 255)
a[inner] = bga[inner]
tok = Image.fromarray(a)
e = emblem(104)
tok.alpha_composite(e, (80 - e.width // 2, 80 - e.height // 2 + 2))
save(tok, 'clan/lion.webp')

# --- the board: the squirrel board's frame and Discard Pile strip, new text and emblem, no portrait
b = load('expansion/board/squirrel.webp')
W, H = b.size
grad = Image.new('RGBA', (W, H))
ga = np.zeros((H, W, 4), np.uint8)
t = (yy2 := np.mgrid[0:H, 0:W][0]) / H
ga[..., 0] = (30 - 10 * t).astype(np.uint8); ga[..., 1] = (56 - 16 * t).astype(np.uint8); ga[..., 2] = (76 - 20 * t).astype(np.uint8); ga[..., 3] = 255
ga[..., :3] = np.clip(ga[..., :3].astype(int) + (np.array(Image.fromarray((np.random.rand(H, W) * 255).astype(np.uint8)).filter(ImageFilter.GaussianBlur(1.2))).astype(int)[..., None] - 128) // 10, 0, 255)
grad = Image.fromarray(ga)
strip_x = 540
board = grad.copy()
board.paste(b.crop((strip_x, 0, W, H)), (strip_x, 0))
# Hildegard where the warchief's portrait is, behind the name
hil = hildegard(420)
hil.putalpha(hil.split()[3].point(lambda v: v * 0.35))
board.alpha_composite(hil, (strip_x - hil.width - 4, 120))
d = ImageDraw.Draw(board)
gold = (240, 176, 48)
cx, cy, r = 130, 300, 104
d.line((cx, 232, strip_x - 10, 232), fill=gold, width=3)
d.line((cx, 368, strip_x - 10, 368), fill=gold, width=3)
d.ellipse((cx - r - 8, cy - r - 8, cx + r + 8, cy + r + 8), fill=(20, 38, 50))
d.ellipse((cx - r, cy - r, cx + r, cy + r), outline=gold, width=7)
e = emblem(150)
board.alpha_composite(e, (cx - e.width // 2, cy - e.height // 2 + 2))
fb = font('InterDisplay-ExtraBold', 38)
spaced(d, (382, 280), 'LION', fb, gold, 4)
spaced(d, (382, 326), 'KINGDOM', fb, gold, 4)
spaced(d, (370, 406), 'HILDEGARD', font('InterDisplay-MediumItalic', 22), (196, 150, 70), 3)
fh = font('Inter-Bold', 17); ft = font('Inter-Medium', 15)
def para(y, head, text, width, cxp):
    d.text((cxp, y), head, font=fh, fill=gold, anchor='mm'); y += 24
    for l in wrap(d, text, ft, width):
        d.text((cxp, y), l, font=ft, fill=(240, 240, 240), anchor='mm'); y += 20
para(26, 'CLAN POWER: FORTS AND LORDS', 'You have 3 Lords. When you build a Defense Tower, put a Lord from your reserve on it. Each Lord adds 2 combat points when you defend its territory. Lords never move and never take casualties.', 330, 300)
para(462, 'WARCHIEF POWER:', 'After a combat involving Hildegard, collect 1 lore for each casualty the enemy suffered.', 300, 300)
d.text((24, H - 14), 'Fan-made', font=font('Inter-MediumItalic', 12), fill=(170, 180, 186), anchor='lm')
save(board.convert('RGB'), 'expansion/board/lion.webp')

# --- the Lord token: a gold ring around the video game's Lord unit (its icon from the Northgard wiki, expansion/lion/lord.webp)
lord = Image.new('RGBA', (300, 300), (0, 0, 0, 0))
d = ImageDraw.Draw(lord)
d.ellipse((4, 4, 295, 295), fill=(36, 26, 8, 255))
d.ellipse((12, 12, 287, 287), fill=(217, 166, 43, 255))
d.ellipse((30, 30, 269, 269), fill=(30, 52, 70, 255))
fig = load('expansion/lion/lord.webp')
fig = fig.crop(fig.split()[3].getbbox())
fig = fig.resize((262 * fig.width // fig.height, 262), Image.LANCZOS)
inside = Image.new('RGBA', (300, 300), (0, 0, 0, 0))
inside.alpha_composite(fig, (150 - fig.width // 2 + 6, 286 - fig.height))
field = Image.new('L', (300, 300), 0)
ImageDraw.Draw(field).ellipse((30, 30, 269, 269), fill=255)
inside.putalpha(Image.fromarray(np.minimum(np.array(inside.split()[3]), np.array(field))))
lord.alpha_composite(inside)
save(lord, 'token/lord.webp', lossless=True)

# --- the Chapel: the Carved Stone's grass, a small stone chapel with a bell gable
ch = load('token/building/carved-stone.webp')
cw, chh = ch.size
grass = ch.crop((0, chh - 70, cw, chh))
for y in range(0, chh, 60):
    g2 = grass if (y // 60) % 2 == 0 else ImageOps.flip(grass)
    ch.paste(g2, (0, y))
ch = ch.filter(ImageFilter.GaussianBlur(1.2))
d = ImageDraw.Draw(ch)
stone, shade, roof, dark = (178, 170, 156), (140, 132, 120), (92, 58, 40), (40, 30, 26)
d.ellipse((50, 230, 270, 285), fill=(40, 60, 30, 110))
# nave
d.polygon([(70, 150), (200, 150), (200, 262), (70, 262)], fill=stone, outline=dark)
d.polygon([(200, 160), (252, 140), (252, 240), (200, 262)], fill=shade, outline=dark)
d.polygon([(60, 156), (135, 92), (210, 156)], fill=roof, outline=dark)
d.polygon([(135, 92), (210, 156), (262, 138), (190, 80)], fill=(70, 44, 30), outline=dark)
# bell gable and cross
d.rectangle((118, 46, 152, 100), fill=stone, outline=dark)
d.polygon([(112, 50), (135, 26), (158, 50)], fill=roof, outline=dark)
d.rectangle((131, 6, 139, 30), fill=(230, 196, 90), outline=dark)
d.rectangle((124, 12, 146, 19), fill=(230, 196, 90), outline=dark)
d.ellipse((126, 58, 144, 78), fill=dark)
# door and windows
d.rectangle((118, 206, 152, 262), fill=(70, 46, 30), outline=dark)
d.pieslice((118, 190, 152, 222), 180, 360, fill=(70, 46, 30), outline=dark)
for x in (86, 172):
    d.rectangle((x - 6, 182, x + 6, 210), fill=(60, 90, 130), outline=dark)
for x in (218, 238):
    d.polygon([(x - 5, 192), (x + 5, 188), (x + 5, 212), (x - 5, 216)], fill=(50, 72, 104), outline=dark)
for y in range(170, 262, 18):
    d.line((70, y, 200, y), fill=(160, 152, 140))
save(ch.convert('RGB'), 'token/building/chapel.webp')

# --- the alternative starting cards: two actions with OR on the bar, a new label, the art of the matching core card
team = load('card/clan/goat-teamwork.webp')
ic = {
    'recruit': team.crop((36, 22, 116, 92)),
    'or': team.crop((116, 40, 142, 64)),
    'move': team.crop((138, 22, 202, 92)),
    'explore': team.crop((220, 22, 298, 92)),
    'build': team.crop((316, 22, 372, 92)),
}
def res_icon(p, s=46):
    t = load(p); t = t.crop(t.getbbox()); t.thumbnail((s, s), Image.LANCZOS); return t
ic['food'] = res_icon('token/food.webp'); ic['wood'] = res_icon('token/wood.webp'); ic['fame'] = res_icon('token/fame.webp')
alts = {'adventure': ('explore', ['explore', 'or', 'move']), 'toil': ('build', ['build', 'or', 'food', 'or', 'wood']), 'prey': ('recruit', ['recruit', 'or', 'fame'])}
for color in ('blue', 'red', 'yellow', 'purple', 'green'):
    for name, (art, row) in alts.items():
        im = load('card/start/%s-%s.webp' % (color, art))
        strip = im.crop((34, 16, 120, 86))
        for x in range(34, 380, 80): im.paste(strip, (x, 16))
        icons = [ic[k] for k in row]
        w = sum(i.width for i in icons) + 6 * (len(icons) - 1)
        x = 206 - w // 2
        for i in icons:
            im.alpha_composite(i, (x, 52 - i.height // 2)); x += i.width + 6
        d = ImageDraw.Draw(im)
        # the label: dark plate, ribbon left as printed
        f = font('Inter-Bold', 15)
        tw = sum(d.textlength(c, font=f) for c in name.upper()) + 2.2 * (len(name) - 1)
        d.rectangle((150, 96, max(150 + tw + 28, 276), 124), fill=(14, 30, 40, 255))
        spaced(d, (164, 110), name.upper(), f, (222, 230, 234), 2.2, anchor='lm')
        save(im.convert('RGB'), 'card/start/%s-%s.webp' % (color, name))
# --- Hildegard on the map: outlined in the player's color like the New Blood warchiefs (warchief-pawns.py)
colors = {'blue': (79, 120, 188), 'red': (234, 65, 13), 'yellow': (255, 255, 0), 'purple': (165, 108, 166), 'green': (79, 159, 59), 'orange': (255, 138, 20)}
size, band, rim = 512, 15, 5
fig = hildegard(370)
canvas = Image.new('RGBA', (size, size), (0, 0, 0, 0))
canvas.alpha_composite(fig, ((size - fig.width) // 2, size - fig.height - band - rim - 8 - (350 - fig.height) // 2 if fig.height < 350 else size - fig.height - band - rim - 8))
alpha = canvas.split()[3].point(lambda a: 255 if a > 96 else 0)
def grow(a, r): return a.filter(ImageFilter.GaussianBlur(r / 2)).point(lambda v: 255 if v > 6 else 0)
inner, outer = grow(alpha, band), grow(alpha, band + rim)
for color, rgb in colors.items():
    out = Image.new('RGBA', (size, size), (255, 255, 255, 0))
    out.paste(Image.new('RGBA', (size, size), (255, 255, 255, 255)), (0, 0), outer.filter(ImageFilter.GaussianBlur(0.8)))
    out.paste(Image.new('RGBA', (size, size), rgb + (255,)), (0, 0), inner.filter(ImageFilter.GaussianBlur(0.8)))
    out.alpha_composite(canvas)
    save(out, 'token/unit/chief-lion-%s.webp' % color)
print('done')
