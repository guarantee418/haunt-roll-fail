#!/usr/bin/env python3
# Military paths module (fan content): the MXP icon (ui/mxp.webp), the crossed swords the Northgard video game shows for
# Military Experience. The only copy found is the game wiki's 20x20 one (https://northgard.fandom.com/wiki/File:Military_Experience.png,
# kept here as mxp-source.png); it is scaled up to the 64x64 of the other ui icons, as it is drawn at about 20 pixels (1.4em)
# beside numbers. Run from haunt-roll-fail/: python3 nort/tools/mxp-icon.py
from PIL import Image, ImageFilter

im = Image.open('nort/tools/mxp-source.png').convert('RGBA')
# Scale in two steps (bicubic to 128, then down to 64) and sharpen a little, to keep the blades crisp
big = im.resize((128, 128), Image.BICUBIC).filter(ImageFilter.UnsharpMask(radius=2, percent=60, threshold=2))
big.resize((64, 64), Image.LANCZOS).save('webp2/nort/images/ui/mxp.webp', lossless=True)
