#!/usr/bin/env python3
# The 3rd printing's Troop Transport cards and tokens, from the pictures of Bad Crow's "3rd printing balance changes" sheet (its six
# "Troop transport cards", 600x825, 133 to 138 counting the document's pictures from 1, one a faction, told by its background and emblem):
#   boat-art.py <dir> <out>   <dir> holds the pictures as 133.jpg ... 138.jpg; <out> is webp2/tactics/images
# Each card is cut to its frame as brigade/troop-transport-<faction>.webp, 457 wide; the token is the card's boat in tactics-art.py's ring.
import sys, os, importlib.util
from PIL import Image
here = os.path.dirname(os.path.abspath(__file__))
spec = importlib.util.spec_from_file_location('art', os.path.join(here, 'tactics-art.py'))
art = importlib.util.module_from_spec(spec)
spec.loader.exec_module(art)
CARDS = {133: 'us', 134: 'wehrmacht', 135: 'soviet', 136: 'okw', 137: 'british', 138: 'dak'}
def make(d, out):
    for n, f in CARDS.items():
        img = Image.open(os.path.join(d, '%d.jpg' % n)).convert('RGB')
        card = img.crop((45, 45, 555, 778)).resize((457, int(733 * 457 / 510)), Image.LANCZOS)
        art.webp(card, os.path.join(out, 'brigade', 'troop-transport-' + f + '.webp'), q=85)
        art.webp(art.token(img.crop((180, 50, 420, 290)), 'Y', art.COLORS[f]), os.path.join(out, 'token', 'troop-transport-' + f + '.webp'), q=90)
if __name__ == '__main__':
    make(sys.argv[1], sys.argv[2])
