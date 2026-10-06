# Makes the player panel icons in webp2/nort/images/ui/ (run from haunt-roll-fail/):
# unit.webp, the unit figure from the Active Area board's Winter costs (the Recruit card's figure, without its + sign);
# card-hand.webp, card-draw.webp, card-discard.webp, the start card back outlined green, yellow and red;
# first-player.webp, the first player marker without its background.
from collections import deque
from PIL import Image, ImageDraw, ImageFilter

IMAGES = 'webp2/nort/images/'

# The pixels connected to the image's edge through pixels for which isbg is true
def background(img, isbg):
    w, h = img.size
    px = img.load()
    bg = bytearray(w * h)
    q = deque([(x, y) for x in range(w) for y in (0, h - 1)] + [(x, y) for y in range(h) for x in (0, w - 1)])
    while q:
        x, y = q.popleft()
        if x < 0 or y < 0 or x >= w or y >= h or bg[y * w + x] or not isbg(px[x, y]):
            continue
        bg[y * w + x] = 1
        q.extend([(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)])
    return Image.frombytes('L', (w, h), bytes(0 if v else 255 for v in bg))

# Cuts the image out with the mask and centers it on a 64x64 icon
def icon(img, mask, out):
    r = img.convert('RGBA')
    r.putalpha(mask)
    r = r.crop(mask.point(lambda v: 255 if v > 20 else 0).getbbox())
    s = 64 / max(r.size)
    r = r.resize((round(r.size[0] * s), round(r.size[1] * s)), Image.LANCZOS)
    c = Image.new('RGBA', (64, 64))
    c.paste(r, ((64 - r.size[0]) // 2, (64 - r.size[1]) // 2))
    c.save(IMAGES + 'ui/' + out, quality=90)

# The figure has a white outline: everything outside it is background
unit = Image.open(IMAGES + 'expansion/board/active-area.webp').convert('RGB').crop((395, 870, 480, 945))
icon(unit, background(unit, lambda p: min(p) <= 150).filter(ImageFilter.GaussianBlur(0.6)), 'unit.webp')

# The marker sits on a flat gray background with a bluish rim
marker = Image.open(IMAGES + 'token/first-player.webp').convert('RGB')
icon(marker, background(marker, lambda p: max(p) < 60 and max(p) - min(p) < 8).filter(ImageFilter.MinFilter(3)).filter(ImageFilter.GaussianBlur(1.5)), 'first-player.webp')

# A card tilted like the card icons printed on the cards, drawn 8 times larger then scaled down
K = 8
W, H = 40 * K, 56 * K
art = Image.open(IMAGES + 'card/back/start.webp').convert('RGBA').resize((W, H), Image.LANCZOS)
for name, color in [('hand', (110, 200, 80)), ('draw', (240, 200, 60)), ('discard', (220, 60, 50))]:
    shape = Image.new('L', (W, H))
    ImageDraw.Draw(shape).rounded_rectangle((0, 0, W - 1, H - 1), radius=5 * K, fill=255)
    card = Image.new('RGBA', (W, H))
    card.paste(art, (0, 0), shape)
    ImageDraw.Draw(card).rounded_rectangle((0, 0, W - 1, H - 1), radius=5 * K, outline=color + (255,), width=4 * K)
    c = Image.new('RGBA', (64 * K, 64 * K))
    c.paste(card, ((64 * K - W) // 2, (64 * K - H) // 2), card)
    c.rotate(-12, resample=Image.BICUBIC).resize((64, 64), Image.LANCZOS).save(IMAGES + 'ui/card-' + name + '.webp', quality=90)
