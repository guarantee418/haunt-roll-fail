#!/usr/bin/env python3
# The Deutsches Afrikakorps' seventeen Lost Brigade cards and the tokens of their new units, from the pictures of Bad Crow's "3rd printing
# balance changes" sheet (a Google Doc the owner shared, public: its export?format=zip; the Lost Brigades' "Dak" pictures, 600x825 each,
# 95 to 111 counting the document's pictures in order from 1).
#
#   dak-brigade-art.py <dir> <out>   <dir> holds the pictures as 95.img ... 111.img; <out> is webp2/tactics/images
#   dak-brigade-art.py preview <dir> <file>   a sheet of the token crops, to check them
#
# Each card is cut to its frame and saved as brigade/<id>.webp, 457 wide like the Faction Guide's; the tokens come from the cards'
# pictures in tactics-art.py's ring of the DAK colour.

import sys, os, importlib.util
from PIL import Image

here = os.path.dirname(os.path.abspath(__file__))
spec = importlib.util.spec_from_file_location('art', os.path.join(here, 'tactics-art.py'))
art = importlib.util.module_from_spec(spec)
spec.loader.exec_module(art)

DAK = art.COLORS['dak']

CARDS = {95: 'lb-dak-howitzer', 96: 'lb-dak-artillery', 97: 'lb-dak-bersaglieri', 98: 'lb-command-panther-dak', 99: 'lb-dak-elefant',
    100: 'lb-dak-guastatori', 101: 'lb-dak-kradschutzen', 102: 'lb-dak-l640', 103: 'lb-dak-marder', 104: 'lb-dak-nebelwerfer',
    105: 'lb-dak-panzerpioneer', 106: 'lb-dak-semovente', 107: 'lb-dak-stormtroopers', 108: 'lb-dak-stug', 109: 'lb-dak-coastal-reserves',
    110: 'lb-dak-flammpanzer', 111: 'lb-dak-tank-busters'}

# the token, the card its picture is on, the square cut from it, its kind
TOKENS = [
    ('command-panther', 98, (195, 80, 395, 280), 'R'),
    ('marder-iii', 103, (185, 85, 385, 285), 'R'),
    ('nebelwerfer', 104, (205, 90, 425, 310), 'G'),
    ('stormtroopers', 107, (210, 80, 425, 295), 'G'),
    ('stug', 108, (200, 80, 400, 280), 'R'),
    ('coastal-reserves', 109, (205, 85, 425, 305), 'G'),
    ('tank-busters', 111, (205, 85, 425, 305), 'G'),
]

def make(d, out):
    os.makedirs(os.path.join(out, 'brigade'), exist_ok=True)
    for n, cid in CARDS.items():
        img = Image.open(os.path.join(d, '%d.img' % n)).convert('RGB').crop((45, 45, 555, 778))
        img = img.resize((457, int(733 * 457 / 510)), Image.LANCZOS)
        art.webp(img, os.path.join(out, 'brigade', cid + '.webp'), q=85)
    for name, n, box, kind in TOKENS:
        img = Image.open(os.path.join(d, '%d.img' % n)).convert('RGB')
        art.webp(art.token(img.crop(box), kind, DAK), os.path.join(out, 'token', name + '-dak.webp'), q=90)

def preview(d, file):
    sheet = Image.new('RGBA', (240 * len(TOKENS), 240), (40, 40, 40, 255))
    for i, (name, n, box, kind) in enumerate(TOKENS):
        img = Image.open(os.path.join(d, '%d.img' % n)).convert('RGB')
        sheet.alpha_composite(art.token(img.crop(box), kind, DAK), (240 * i, 0))
    sheet.save(file)

if __name__ == '__main__':
    if sys.argv[1] == 'preview':
        preview(sys.argv[2], sys.argv[3])
    else:
        make(sys.argv[1], sys.argv[2])
