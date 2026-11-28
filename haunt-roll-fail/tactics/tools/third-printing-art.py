#!/usr/bin/env python3
# The 3rd printing's Commander cards and the tokens of their units, from the pictures of Bad Crow's "3rd printing balance changes" sheet (a
# Google Doc the owner shared, public: its export?format=zip; the pictures counted in document order from 1, the Commanders 32 to 68,
# 1500x900 each).
#
#   third-printing-art.py <dir> <out>   <dir> holds the pictures as <n>.img; <out> is webp2/tactics/images
#
# Each card in CARDS is cut to its frame as commander/<id>.webp (1016 wide, like the Faction Guide's); the tokens come from the portraits
# on the cards' panels (2, 3, 4 at x 485, 805, 1130) in tactics-art.py's ring of the faction's colour. ALL names every Commander picture
# of the sheet, the ones not yet played too.

import sys, os, importlib.util
from PIL import Image

here = os.path.dirname(os.path.abspath(__file__))
spec = importlib.util.spec_from_file_location('art', os.path.join(here, 'tactics-art.py'))
art = importlib.util.module_from_spec(spec)
spec.loader.exec_module(art)

# the sheet's Commander pictures: Rocket pack (US, Wehrmacht, Soviet, OKW, British two each, the DAK's two), the universal German ones, the
# Desert Rats', the Stuart Honey pack's and the Stretch Goal box's
ALL = {32: 'Field Ordnance (US)', 33: 'Cavalry Support (US)', 34: 'Luftwaffe Supply Doctrine (WM)', 35: 'Breakthrough Offensive (WM)',
    36: 'Reserve Army (SU)', 37: 'Shock Army (SU)', 38: 'Defense in Depth (OKW)', 39: 'Atlantic Wall (OKW)', 40: 'Rocketry Support (BR)',
    41: 'Army in Exile (BR)', 42: 'Desperate Battle (DAK)', 43: 'Last Stand (DAK)', 44: 'Intel Disruption Doctrine', 45: 'Tank Buster Doctrine',
    46: 'StuG Assault Doctrine (DAK)', 47: 'StuG Tactics (DAK)', 48: 'Polish Carpathian Brigade', 49: 'Special Operations', 50: 'Shock Assault Regiment',
    51: 'Logistical Supremacy', 52: 'Artillery Support', 53: 'Heavy Armor Support', 54: 'Polish Cavalry Regiment', 55: 'Commonwealth Defense',
    56: 'Special Operations (US)', 57: 'Engineering Corps (US)', 58: 'Fortified Armor Doctrine (WM)', 59: 'Jaeger Armor Doctrine (WM)',
    60: 'Shock Motor (SU)', 61: 'Airborne Troops Tactics (SU)', 62: 'Overwatch Doctrine (OKW)', 63: 'Luftwaffe Ground Forces (OKW)',
    64: 'Infantry Assault (BR)', 65: 'Demolition Engineers (BR)', 66: 'Field Support (BR)', 67: 'Italian Infantry (DAK)', 68: 'Stalling Tactics (DAK)'}

# the cards played, by picture (a universal German card is each German faction's Commander)
CARDS = {35: 'breakthrough-offensive', 37: 'shock-army', 46: 'stug-assault-dak', 47: 'stug-tactics-dak',
    44: ['intel-disruption-wehrmacht', 'intel-disruption-okw', 'intel-disruption-dak'], 45: ['tank-buster-wehrmacht', 'tank-buster-okw', 'tank-buster-dak'],
    56: 'special-operations-us', 49: 'special-operations-british',
    57: 'engineering-corps', 38: 'defense-in-depth', 39: 'atlantic-wall', 36: 'reserve-army', 67: 'italian-infantry',
    48: 'polish-carpathian', 50: 'shock-assault', 51: 'logistical-supremacy', 52: 'artillery-support', 53: 'heavy-armor-support',
    54: 'polish-cavalry', 55: 'commonwealth-defense',
    41: 'army-in-exile', 40: 'rocketry-support', 66: 'field-support', 42: 'desperate-battle', 43: 'last-stand', 60: 'shock-motor',
    61: 'airborne-troops', 34: 'luftwaffe-supply'}

PANEL = {2: 485, 3: 805, 4: 1130}

# the token, its faction, the picture and panel its portrait is on, its kind
TOKENS = [('panzerwerfer', 'wehrmacht', 35, 2, 'Y'), ('katyusha', 'soviet', 37, 3, 'Y'),
    ('marder-iii', 'wehrmacht', 44, 2, 'R'), ('marder-iii', 'okw', 44, 2, 'R'), ('m3-lee', 'us', 56, 3, 'R'), ('m3-grant', 'british', 49, 3, 'R'),
    ('ssf-canadians', 'british', 49, 2, 'G'), ('combat-engineers', 'soviet', 36, 3, 'G'),
    ('polish-lancer', 'british', 41, 2, 'G'), ('matilda', 'british', 48, 2, 'R'), ('black-prince', 'british', 48, 4, 'R'),
    ('canadian-shock', 'british', 55, 2, 'G'), ('anzac', 'british', 51, 2, 'G'), ('dingo', 'british', 53, 2, 'Y'), ('stuart', 'british', 54, 2, 'R'),
    ('land-mattress', 'british', 40, 2, 'G'), ('land-mattress-emplacement', 'british', 40, 4, 'P')]

def make(d, out):
    for n, ids in CARDS.items():
        img = Image.open(os.path.join(d, '%d.img' % n)).convert('RGB').crop((75, 65, 1425, 830))
        img = img.resize((1016, int(765 * 1016 / 1350)), Image.LANCZOS)
        for cid in ([ids] if isinstance(ids, str) else ids):
            art.webp(img, os.path.join(out, 'commander', cid + '.webp'), q=85)
    for name, f, n, panel, kind in TOKENS:
        img = Image.open(os.path.join(d, '%d.img' % n)).convert('RGB')
        x = PANEL[panel]
        art.webp(art.token(img.crop((x, 115, x + 225, 340)), kind, art.COLORS[f]), os.path.join(out, 'token', name + '-' + f + '.webp'), q=90)

if __name__ == '__main__':
    make(sys.argv[1], sys.argv[2])
