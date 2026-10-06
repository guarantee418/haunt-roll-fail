# Makes the Automa's Leader figures token/unit/leader-<1|2>-<color>.webp (Uncharted Horizons' Solo module):
# the two miniatures are cut out of the components photo on page 2 of the Uncharted Horizons rulebook (in the
# Tabletop Simulator mod 3597126237; Leader 1 is white, Leader 2 black), then placed and outlined like the
# warchief portraits (warchief-portraits.py).
#
#   python3 automa-leaders.py <rulebook.pdf> <webp2/nort/images/token/unit>
#
# Needs pdftoppm (poppler), Pillow, numpy and scipy.

import os, subprocess, sys, tempfile
import numpy as np
from PIL import Image, ImageFilter
from scipy import ndimage

Image.MAX_IMAGE_PIXELS = None

colors = {
    "blue": (79, 120, 188),
    "red": (234, 65, 13),
    "yellow": (247, 156, 0),
    "purple": (165, 108, 166),
    "green": (79, 159, 59),
    "orange": (255, 138, 20),
}

size = 512
height = 440    # the figure's height on the canvas, base included
top = 40
band = 15       # colored outline, in output pixels
rim = 5         # white rim outside it

def grow(alpha, r):
    return alpha.filter(ImageFilter.GaussianBlur(r / 2)).point(lambda a: 255 if a > 6 else 0)

pdf, out = sys.argv[1], sys.argv[2]

with tempfile.TemporaryDirectory() as tmp:
    subprocess.run(["pdftoppm", "-r", "600", "-f", "2", "-l", "2", "-png", pdf, tmp + "/p"], check = True)
    page = Image.open(tmp + "/" + os.listdir(tmp)[0]).convert("RGB")

# The photo, in units of a 662 px wide page
s = page.width / 662
photo = np.array(page.crop((int(230 * s), int(590 * s), int(345 * s), int(650 * s)))).astype(float)

# Everything far enough from the flat blue background, per figure (x ranges in the crop above)
fg = np.sqrt(((photo - np.array([54, 82, 107.0])) ** 2).sum(-1)) > 28

for leader, (x0, x1) in {"2": (95, 470), "1": (455, 830)}.items():
    m = np.zeros_like(fg)
    m[120:565, x0:x1] = fg[120:565, x0:x1]
    m = ndimage.binary_opening(m, iterations = 1)
    labels, n = ndimage.label(m)
    m = labels == (np.argmax(ndimage.sum(m, labels, range(1, n + 1))) + 1)
    # Fill the small holes (noise), keep the big ones (between the arms and the axe)
    holes = ndimage.binary_fill_holes(m) & ~m
    hl, hn = ndimage.label(holes)
    for i, area in enumerate(ndimage.sum(holes, hl, range(1, hn + 1))):
        if area < 150:
            m |= hl == i + 1
    m = ndimage.binary_erosion(m, iterations = 1)

    ys, xs = np.where(m)
    fig = Image.fromarray(photo.astype(np.uint8))
    fig.putalpha(Image.fromarray((m * 255).astype(np.uint8)).filter(ImageFilter.GaussianBlur(0.7)))
    fig = fig.crop((xs.min(), ys.min(), xs.max() + 1, ys.max() + 1))
    fig = fig.resize((round(fig.width * height / fig.height), height), Image.LANCZOS)

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
        result.save(out + "/leader-" + leader + "-" + name + ".webp", quality = 88, method = 6)
