# Makes the combat icons in webp2/nort/images/ui/ (run from haunt-roll-fail/): axe.webp (a combat point) and
# skull.webp (a casualty), cut from the battle die faces in token/die.webp (the die texture of the Tabletop
# Simulator mod 2838546142, black on white) and drawn light with a dark rim, to show on the dark interface.
from PIL import Image, ImageFilter

IMAGES = 'webp2/nort/images/'

die = Image.open(IMAGES + 'token/die.webp').convert('L')

def icon(box, out):
    # Ink is dark: its darkness is the shape
    shape = die.crop(box).point(lambda v: 0 if v > 200 else 255 if v < 80 else (200 - v) * 255 // 120)
    shape = shape.crop(shape.point(lambda v: 255 if v > 40 else 0).getbbox())
    K = 4
    s = (64 * K - 8 * K) / max(shape.size)
    shape = shape.resize((round(shape.size[0] * s), round(shape.size[1] * s)), Image.LANCZOS)
    big = Image.new('L', (64 * K, 64 * K))
    big.paste(shape, ((64 * K - shape.size[0]) // 2, (64 * K - shape.size[1]) // 2))
    rim = big.filter(ImageFilter.MaxFilter(2 * K + 1)).filter(ImageFilter.GaussianBlur(K / 2))
    c = Image.new('RGBA', (64 * K, 64 * K), (20, 16, 12, 0))
    c.putalpha(rim)
    c.paste(Image.new('RGBA', c.size, (236, 228, 214, 255)), (0, 0), big)
    c.resize((64, 64), Image.LANCZOS).save(IMAGES + 'ui/' + out, quality=90)

# The face with a single axe (top left) and the top skull of the face with two skulls (top right)
icon((25, 290, 145, 404), 'axe.webp')
icon((540, 290, 635, 395), 'skull.webp')
