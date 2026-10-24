# Makes the warchief pawns token/unit/chief-<clan>-<color>.webp for the New Blood clans (and chief-brok-<color>
# for Horse's Brok): each warchief cut out of the art of their clan card (card/clan/*.webp), which shows them down
# to the waist or the knees, above the card's text box, so the pawn stands on that straight edge. The figure is
# placed and outlined like the core clans' portraits (warchief-portraits.py): an outline in the player's color and a
# thin white rim. The figures are cut from the art with the BiRefNet model of rembg (pip install rembg), one
# process for each, as the model runs out of memory over several images in one process.
#
#   python3 warchief-pawns.py <webp2/nort/images>

import sys, os, subprocess, tempfile
import numpy
from scipy import ndimage
from PIL import Image, ImageDraw, ImageFilter

# Card, and the box of the art cut out, in card pixels (412x635): above the text box, and below the clan icon and
# the card's name where the figure doesn't reach them
pawns = {
    "dragon": ("dragon-tenacious-grudge", (0, 150, 412, 480)),
    "horse": ("horse-eitria-and-broks-precision", (200, 150, 412, 470)),
    "brok": ("horse-eitria-and-broks-precision", (0, 150, 240, 470)),
    "kraken": ("kraken-howl-from-the-sea", (0, 150, 412, 455)),
    "lynx": ("lynx-the-wise-one", (0, 150, 412, 500)),
    "ox": ("ox-the-true-hero", (0, 150, 412, 455)),
    "rat": ("rat-blood-ties", (0, 130, 412, 440)),
    "squirrel": ("squirrel-eldrich", (0, 105, 412, 470)),
}

# Parts of the box left out of the figure, in card pixels: the clan icon and the card's name, where the figure
# reaches above them
cuts = {
    "rat": [(80, 0, 360, 146)],
    "squirrel": [(85, 0, 360, 150)],
}

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
width = 380     # the largest figure: the wide ones about as wide as a warrior, the tall ones a little shorter than the portraits
# (the art is a close-up, so they look as big)
height = 350
band = 15       # colored outline, in output pixels
rim = 5         # white rim outside it

def grow(alpha, r):
    # Grows the shape by about r pixels with round corners: blur, then keep everything the blur reached
    return alpha.filter(ImageFilter.GaussianBlur(r / 2)).point(lambda a: 255 if a > 6 else 0)

def solid(mask):
    # Fills the gaps inside the shape (between an arm and the body), so the outline only goes around it
    return Image.fromarray((ndimage.binary_fill_holes(numpy.array(mask) > 127) * 255).astype(numpy.uint8))

def biggest(a):
    # Keeps only the largest piece of the figure, dropping specks like the embers in the art
    parts, n = ndimage.label(numpy.array(a) > 96)
    if n < 2:
        return a
    keep = ndimage.binary_dilation(parts == 1 + numpy.argmax(ndimage.sum(numpy.ones(parts.shape), parts, range(1, n + 1))), iterations = 3)
    return Image.fromarray((numpy.array(a) * keep).astype(numpy.uint8))

def cutout(art, out):
    # Runs in its own process (see above)
    from rembg import remove, new_session
    remove(Image.open(art), session = new_session("birefnet-general")).save(out)

if len(sys.argv) == 4 and sys.argv[1] == "--cut":
    cutout(sys.argv[2], sys.argv[3])
    sys.exit(0)

images = sys.argv[1]
tmp = tempfile.mkdtemp()

for name, (card, box) in pawns.items():
    art = Image.open(images + "/card/clan/" + card + ".webp").convert("RGB").crop(box)
    art.save(tmp + "/art.png")
    subprocess.run([sys.executable, "-I", os.path.abspath(__file__), "--cut", tmp + "/art.png", tmp + "/fig.png"], check = True)
    fig = Image.open(tmp + "/fig.png").convert("RGBA")

    a = fig.split()[3]
    d = ImageDraw.Draw(a)
    for (l, t, r, b) in cuts.get(name, []):
        d.rectangle((l - box[0], t - box[1], r - box[0], b - box[1]), fill = 0)
    # Drops the faint haze the model leaves around the figure
    a = biggest(a.point(lambda v: 0 if v < 40 else v))
    fig.putalpha(a)
    fig = fig.crop(a.point(lambda v: 255 if v > 96 else 0).getbbox())

    k = min(width / fig.width, height / fig.height)
    fig = fig.resize((round(fig.width * k), round(fig.height * k)), Image.LANCZOS)
    canvas = Image.new("RGBA", (size, size), (0, 0, 0, 0))
    # Centred, standing a little above the bottom to leave room for the outline under it
    canvas.paste(fig, ((size - fig.width) // 2, size - fig.height - band - rim - 8 - (height - fig.height) // 2), fig)

    alpha = canvas.split()[3].point(lambda a: 255 if a > 96 else 0)
    inner = solid(grow(alpha, band))
    outer = solid(grow(alpha, band + rim))

    for color, rgb in colors.items():
        result = Image.new("RGBA", (size, size), (255, 255, 255, 0))
        result.paste(Image.new("RGBA", (size, size), (255, 255, 255, 255)), (0, 0), outer.filter(ImageFilter.GaussianBlur(0.8)))
        result.paste(Image.new("RGBA", (size, size), rgb + (255,)), (0, 0), inner.filter(ImageFilter.GaussianBlur(0.8)))
        result.alpha_composite(canvas)
        result.save(images + "/token/unit/chief-" + name + "-" + color + ".webp", quality = 88, method = 6)
