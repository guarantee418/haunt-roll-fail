#!/usr/bin/env python3
# The 3rd printing's plane tokens, from the eighteen "Plane cards" pictures of Bad Crow's "3rd printing balance changes" sheet (a Google
# Doc the owner shared, public: its export?format=zip; pictures 113 to 130 counting the document's pictures in order from 1, 600x825 each),
# each card's picture in tactics-art.py's ring of its faction's colour.
#
#   plane-art.py <dir> <out>   <dir> holds the pictures as 113.img ... 130.img; <out> is webp2/tactics/images

import sys, os, importlib.util
from PIL import Image, ImageDraw, ImageFont

here = os.path.dirname(os.path.abspath(__file__))
spec = importlib.util.spec_from_file_location('art', os.path.join(here, 'tactics-art.py'))
art = importlib.util.module_from_spec(spec)
spec.loader.exec_module(art)

# the cards in the sheet's order: faction, role
CARDS = [('us', 'antitank'), ('us', 'bomber'), ('us', 'recon'), ('wehrmacht', 'antitank'), ('wehrmacht', 'bomber'), ('wehrmacht', 'recon'),
    ('soviet', 'bomber'), ('soviet', 'antitank'), ('soviet', 'recon'), ('okw', 'antitank'), ('okw', 'bomber'), ('okw', 'recon'),
    ('british', 'bomber'), ('british', 'antitank'), ('british', 'recon'), ('dak', 'antitank'), ('dak', 'bomber'), ('dak', 'recon')]

def make(d, out):
    for i, (f, role) in enumerate(CARDS):
        img = Image.open(os.path.join(d, '%d.img' % (113 + i))).convert('RGB')
        t = art.token(img.crop((215, 75, 415, 275)), 'Y', art.COLORS[f])
        # the role on a dark badge at the top left: AT, B or R
        draw = ImageDraw.Draw(t)
        draw.ellipse((14, 14, 84, 84), fill=(20, 20, 20, 235), outline=art.COLORS[f] + (255,), width=4)
        label = {'antitank': 'AT', 'bomber': 'B', 'recon': 'R'}[role]
        font = ImageFont.truetype('/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf', 34 if len(label) > 1 else 40)
        draw.text((49, 49), label, fill=(255, 255, 255, 255), font=font, anchor='mm')
        art.webp(t, os.path.join(out, 'token', 'plane-%s-%s.webp' % (role, f)), q=90)

if __name__ == '__main__':
    make(sys.argv[1], sys.argv[2])
