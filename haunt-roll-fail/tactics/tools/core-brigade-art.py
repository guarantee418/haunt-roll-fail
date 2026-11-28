#!/usr/bin/env python3
# The core factions' 26 new Lost Brigade cards of the 3rd printing and the tokens of their new units, from the pictures of Bad Crow's
# "3rd printing balance changes" sheet (a Google Doc the owner shared, public: its export?format=zip; the "New lost brigades" pictures,
# 600x825 each, 69 to 94 counting the document's pictures in order from 1: US 69-72, Wehrmacht 73-76, Soviet 77-80, OKW 81-85,
# British 86-94).
#
#   core-brigade-art.py <dir> <out>   <dir> holds the pictures as 69.jpg ... 94.jpg (or .img); <out> is webp2/tactics/images
#   core-brigade-art.py preview <dir> <file>   a sheet of the token crops, to check them
#
# Each card is cut to its frame and saved as brigade/<id>.webp, 457 wide like the Faction Guide's; the new units' tokens come from the
# cards' pictures in tactics-art.py's ring of the faction's colour.

import sys, os, importlib.util
from PIL import Image

here = os.path.dirname(os.path.abspath(__file__))
spec = importlib.util.spec_from_file_location('art', os.path.join(here, 'tactics-art.py'))
art = importlib.util.module_from_spec(spec)
spec.loader.exec_module(art)

CARDS = {69: 'lb-artillery-observers', 70: 'lb-m3-lee', 71: 'lb-pathfinders', 72: 'lb-rangers',
    73: 'lb-command-panzer-iv', 74: 'lb-marder-wehrmacht', 75: 'lb-panzerwerfer', 76: 'lb-tiger-ace',
    77: 'lb-ppsh-guards', 78: 'lb-katyusha', 79: 'lb-kv1', 80: 'lb-combat-engineers',
    81: 'lb-fg42', 82: 'lb-precision-lefh', 83: 'lb-mg34', 84: 'lb-coastal-reserves-okw', 85: 'lb-marder-okw',
    86: 'lb-anzac', 87: 'lb-dingo', 88: 'lb-m3-grant', 89: 'lb-matilda', 90: 'lb-polish-lancers', 91: 'lb-stuart-honey',
    92: 'lb-black-prince', 93: 'lb-canadian-shock', 94: 'lb-land-mattress'}

# the token, its faction, the card its picture is on, the square cut from it, its kind
TOKENS = [
    ('tiger-ace', 'wehrmacht', 76, (205, 100, 415, 310), 'R'),
    ('ppsh-guards', 'soviet', 77, (205, 80, 425, 300), 'G'),
    ('kv1', 'soviet', 79, (200, 140, 380, 320), 'R'),
    ('fg42', 'okw', 81, (205, 80, 425, 300), 'G'),
    ('coastal-reserves', 'okw', 84, (205, 80, 425, 300), 'G'),
]

def load(d, n):
    for ext in ('jpg', 'img', 'png'):
        p = os.path.join(d, '%d.%s' % (n, ext))
        if os.path.exists(p):
            return Image.open(p).convert('RGB')
    raise FileNotFoundError(n)

def make(d, out):
    os.makedirs(os.path.join(out, 'brigade'), exist_ok=True)
    for n, cid in CARDS.items():
        img = load(d, n).crop((45, 45, 555, 778))
        img = img.resize((457, int(733 * 457 / 510)), Image.LANCZOS)
        art.webp(img, os.path.join(out, 'brigade', cid + '.webp'), q=85)
    for name, faction, n, box, kind in TOKENS:
        art.webp(art.token(load(d, n).crop(box), kind, art.COLORS[faction]), os.path.join(out, 'token', name + '-' + faction + '.webp'), q=90)

def preview(d, file):
    sheet = Image.new('RGBA', (240 * len(TOKENS), 240), (40, 40, 40, 255))
    for i, (name, faction, n, box, kind) in enumerate(TOKENS):
        sheet.alpha_composite(art.token(load(d, n).crop(box), kind, art.COLORS[faction]), (240 * i, 0))
    sheet.save(file)

if __name__ == '__main__':
    if sys.argv[1] == 'preview':
        preview(sys.argv[2], sys.argv[3])
    else:
        make(sys.argv[1], sys.argv[2])
