# the wax seal icon, cut out of a Royal Seal's picture: python3 wax-seal.py <seal.png> <out.png>
import sys
import numpy as np
from PIL import Image, ImageDraw, ImageFilter

K = 8
im = Image.open(sys.argv[1]).convert('RGB').crop((2, 2, 56, 56)).resize((54 * K, 54 * K), Image.LANCZOS)
a = np.asarray(im).astype(float)
# the red of the wax, not the parchment, within the seal's circle (the card's dark border touches it at the corner)
red = Image.fromarray(((a[..., 1] < 95) * 255).astype('uint8')).filter(ImageFilter.MaxFilter(9)).filter(ImageFilter.MinFilter(9))
circle = Image.new('L', im.size, 0)
ImageDraw.Draw(circle).ellipse((3.5 * K, 3 * K, 48.5 * K, 49.5 * K), fill=255)
alpha = Image.fromarray(np.minimum(np.asarray(red), np.asarray(circle))).filter(ImageFilter.GaussianBlur(4)).point(lambda v: 255 if v > 127 else 0).filter(ImageFilter.GaussianBlur(2))
# a little brighter, as it is dark on the panels
o = Image.fromarray(np.clip(a * np.array([1.4, 1.1, 1.1]), 0, 255).astype('uint8'))
o.putalpha(alpha)
o = o.crop(alpha.getbbox())
s = max(o.size)
p = Image.new('RGBA', (s, s))
p.paste(o, ((s - o.width) // 2, (s - o.height) // 2))
p.resize((96, 96), Image.LANCZOS).save(sys.argv[2])
