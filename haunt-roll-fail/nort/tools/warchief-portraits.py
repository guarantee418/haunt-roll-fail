# Makes the warchief figures token/unit/chief-<clan>-<color>.webp from the seven warchief standees
# of the Tabletop Simulator mod 2847156187 (Figurine_Custom, 259x432 PNGs; see "Warchief portraits"
# in nort/HANDOFF.md): each portrait on a 512x512 canvas, scaled and placed like the generic
# warchief figure, with an outline in the player's color and a thin white rim around that.
#
#   python3 warchief-portraits.py <directory with wolf.png, bear.png, ...> <webp2/nort/images/token/unit>

import sys
from PIL import Image, ImageFilter

clans = ["bear", "boar", "goat", "raven", "snake", "stag", "wolf"]

# The middle of each color's unit figure (token/unit/unit-<color>.webp)
colors = {
    "blue": (79, 120, 188),
    "red": (234, 65, 13),
    "yellow": (247, 156, 0),
    "purple": (165, 108, 166),
    "green": (79, 159, 59),
    "orange": (255, 138, 20),
}

size = 512
scale = 1.18    # a little taller than the generic warchief (about 404 px), as the art is busier
top = -8        # leaves room for the outline under the feet
band = 15       # colored outline, in output pixels
rim = 5         # white rim outside it

def grow(alpha, r):
    # Grows the shape by about r pixels with round corners: blur, then keep everything the blur reached
    return alpha.filter(ImageFilter.GaussianBlur(r / 2)).point(lambda a: 255 if a > 6 else 0)

src, out = sys.argv[1], sys.argv[2]

for clan in clans:
    fig = Image.open(src + "/" + clan + ".png").convert("RGBA")
    fig = fig.resize((round(fig.width * scale), round(fig.height * scale)), Image.LANCZOS)
    canvas = Image.new("RGBA", (size, size), (0, 0, 0, 0))
    canvas.paste(fig, ((size - fig.width) // 2, top), fig)

    alpha = canvas.split()[3].point(lambda a: 255 if a > 96 else 0)
    inner = grow(alpha, band).filter(ImageFilter.GaussianBlur(0.8))
    outer = grow(alpha, band + rim).filter(ImageFilter.GaussianBlur(0.8))

    for name, rgb in colors.items():
        result = Image.new("RGBA", (size, size), (255, 255, 255, 0))
        result.paste(Image.new("RGBA", (size, size), (255, 255, 255, 255)), (0, 0), outer)
        result.paste(Image.new("RGBA", (size, size), rgb + (255,)), (0, 0), inner)
        result.alpha_composite(canvas)
        result.save(out + "/chief-" + clan + "-" + name + ".webp", quality = 88, method = 6)
