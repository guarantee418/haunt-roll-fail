# Makes the round warchief tokens token/unit/chief-<clan>-<color>.webp for the New Blood clans (and
# chief-brok-<color> for Horse's Brok): each warchief's head cut from the art of their clan card
# (card/clan/*.webp), in a ring of the player's color with a thin white rim. The core clans' warchiefs
# are portraits instead (warchief-portraits.py).
#
#   python3 warchief-heads.py <webp2/nort/images>

import sys
from PIL import Image, ImageDraw, ImageFilter

# Card, and the centre and width of the square around the head, in card pixels (412x635)
heads = {
    "dragon": ("dragon-tenacious-grudge", 180, 212, 130),
    "horse": ("horse-eitria-and-broks-precision", 345, 252, 130),
    "brok": ("horse-eitria-and-broks-precision", 110, 218, 110),
    "kraken": ("kraken-howl-from-the-sea", 188, 225, 160),
    "lynx": ("lynx-the-wise-one", 182, 245, 140),
    "ox": ("ox-the-true-hero", 262, 235, 145),
    "rat": ("rat-blood-ties", 193, 230, 150),
    "squirrel": ("squirrel-eldrich", 178, 222, 140),
}

# The middle of each color's unit figure (token/unit/unit-<color>.webp)
colors = {
    "blue": (79, 120, 188),
    "red": (234, 65, 13),
    "yellow": (247, 156, 0),
    "purple": (165, 108, 166),
    "green": (79, 159, 59),
    "orange": (255, 138, 20),
}

size = 256
ring = 22       # colored ring, in output pixels
rim = 6         # white rim outside it
k = 4           # drawn at k times the size, then scaled down for smooth edges

images = sys.argv[1]

def disc(r):
    m = Image.new("L", (size * k, size * k), 0)
    c = size * k / 2
    ImageDraw.Draw(m).ellipse((c - r * k, c - r * k, c + r * k, c + r * k), fill = 255)
    return m

for name, (card, x, y, w) in heads.items():
    art = Image.open(images + "/card/clan/" + card + ".webp").convert("RGBA")
    inner = size / 2 - rim - ring
    face = art.crop((round(x - w / 2), round(y - w / 2), round(x + w / 2), round(y + w / 2)))
    face = face.resize((round(inner * 2 * k), round(inner * 2 * k)), Image.LANCZOS)

    for color, rgb in colors.items():
        out = Image.new("RGBA", (size * k, size * k), (0, 0, 0, 0))
        out.paste(Image.new("RGBA", out.size, (255, 255, 255, 255)), (0, 0), disc(size / 2))
        out.paste(Image.new("RGBA", out.size, rgb + (255,)), (0, 0), disc(size / 2 - rim))
        o = round((size / 2 - inner) * k)
        layer = Image.new("RGBA", out.size, (0, 0, 0, 0))
        layer.paste(face, (o, o))
        out.paste(layer, (0, 0), disc(inner))
        out.resize((size, size), Image.LANCZOS).save(images + "/token/unit/chief-" + name + "-" + color + ".webp", quality = 88, method = 6)
