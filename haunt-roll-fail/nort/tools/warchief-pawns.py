# Makes the warchief pawns token/unit/chief-<clan>-<color>.webp for the New Blood clans (and chief-brok-<color>
# for Horse's Brok) from the video game's unit renders of those warchiefs on the Northgard wiki (northgard.fandom.com:
# Surtr-Icon, Eitria icon, Brok icon, Kàra-icon, Mielikki model, Torfin icon, Eir icon, Andhrimnir-model; 101 to 150
# pixels wide), kept in expansion/warchief/<name>.webp, like the Lion's Hildegard (lion-art.py). The render's ground
# shadow is left out, and the figure is scaled and outlined like Hildegard's: an outline in the player's color and a
# thin white rim. Until 2026-10-25 the pawns were cut out of the clan cards' art (the owner preferred the renders).
#
#   python3 warchief-pawns.py <webp2/nort/images>

import sys
import numpy
from scipy import ndimage
from PIL import Image, ImageFilter

pawns = ["dragon", "horse", "brok", "kraken", "lynx", "ox", "rat", "squirrel"]

# The middle of each color's unit figure (token/unit/unit-<color>.webp)
colors = {
    "blue": (79, 120, 188),
    "red": (234, 65, 13),
    "yellow": (255, 255, 0),
    "purple": (165, 108, 166),
    "green": (79, 159, 59),
    "orange": (255, 138, 20),
}

size = 512
height = 370    # as Hildegard's
width = 470     # the widest (Kàra's blades) as wide as the canvas allows
band = 15       # colored outline, in output pixels
rim = 5         # white rim outside it

def grow(alpha, r):
    # Grows the shape by about r pixels with round corners: blur, then keep everything the blur reached
    return alpha.filter(ImageFilter.GaussianBlur(r / 2)).point(lambda a: 255 if a > 6 else 0)

def unshadowed(fig):
    # The figure is opaque and its shadow on the ground half transparent: keeps the opaque part and its soft edge
    a = numpy.array(fig.split()[3])
    near = ndimage.binary_dilation(a >= 200, iterations = 2)
    fig.putalpha(Image.fromarray((a * near).astype(numpy.uint8)))
    return fig.crop(fig.split()[3].point(lambda v: 255 if v > 96 else 0).getbbox())

images = sys.argv[1]

for name in pawns:
    fig = unshadowed(Image.open(images + "/expansion/warchief/" + name + ".webp").convert("RGBA"))
    k = min(height / fig.height, width / fig.width)
    fig = fig.resize((round(fig.width * k), round(fig.height * k)), Image.LANCZOS)
    canvas = Image.new("RGBA", (size, size), (0, 0, 0, 0))
    canvas.alpha_composite(fig, ((size - fig.width) // 2, size - fig.height - band - rim - 8 - max(0, (350 - fig.height) // 2)))

    alpha = canvas.split()[3].point(lambda a: 255 if a > 96 else 0)
    inner, outer = grow(alpha, band), grow(alpha, band + rim)

    for color, rgb in colors.items():
        result = Image.new("RGBA", (size, size), (255, 255, 255, 0))
        result.paste(Image.new("RGBA", (size, size), (255, 255, 255, 255)), (0, 0), outer.filter(ImageFilter.GaussianBlur(0.8)))
        result.paste(Image.new("RGBA", (size, size), rgb + (255,)), (0, 0), inner.filter(ImageFilter.GaussianBlur(0.8)))
        result.alpha_composite(canvas)
        result.save(images + "/token/unit/chief-" + name + "-" + color + ".webp", quality = 88, method = 6)
