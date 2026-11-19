# Art for the fan deck "Sorcery of the Enchanted Woods" (Tabletop Simulator workshop 2413287097, uploaded by slugfacekillah,
# "Not Mine": its designer isn't named). The mod's card sheet is a 10x6 grid of 55 cards (4055x3324), its back one card (811x1108)
# and its two tokens 700x700 squares (the Steam save's CustomDeck FaceURL / BackURL and the two Custom_Tile images):
#   python3 sorcery-art.py <sheet.jpg> <back.jpg> <tower.png> <garden.png> <webp2/root/images/card/sorcery>
# The ambushes, items and the four suit dominances are the base deck's cards with the same art, so only the 25 persistent effects
# and the sixth-improvement dominance are cut, at the 512x708 of the other deck cards.
import sys
from PIL import Image, ImageDraw

sheet, back, tower, garden, out = sys.argv[1:6]

cards = {
    (0, 0): "dominance", (0, 1): "garden-of-glory", (0, 2): "enchanted-objects", (0, 3): "living-buildings",
    (0, 4): "mantra-of-frenzy", (0, 5): "favor-of-the-phoenix", (0, 6): "foresight", (0, 7): "knowledge-of-the-fallen",
    (0, 8): "enchanted-gifts", (0, 9): "spell-of-flight", (1, 0): "onyx-cauldron", (1, 1): "misdirection", (1, 2): "hailstorm",
    (1, 3): "scepter-of-majesty", (1, 5): "visions-from-beyond", (1, 6): "treewalking", (1, 7): "wizards-tower",
    (1, 8): "fire-and-lightning", (1, 9): "dispel", (2, 8): "summon-river-beast",
}

im = Image.open(sheet).convert("RGB")
cw, ch = im.size[0] / 10, im.size[1] / 6

for (r, c), name in cards.items():
    box = (int(c * cw) + 3, int(r * ch) + 3, int((c + 1) * cw) - 3, int((r + 1) * ch) - 3)
    im.crop(box).resize((512, 708), Image.LANCZOS).save(f"{out}/sorcery-{name}.webp", quality=88)

# the back has a white border round its art, wider on the right and at the bottom
b = Image.open(back).convert("RGB")
w, h = b.size
b.crop((38, 38, w - 64, h - 78)).resize((512, 708), Image.LANCZOS).save(f"{out}/../card-back-art-se.webp", quality=88)

# the tokens, as rounded squares
def token(path, size, name):
    t = Image.open(path).convert("RGBA").resize((size, size), Image.LANCZOS)
    m = Image.new("L", t.size, 0)
    ImageDraw.Draw(m).rounded_rectangle((0, 0, size - 1, size - 1), radius=size // 8, fill=255)
    t.putalpha(m)
    ImageDraw.Draw(t).rounded_rectangle((1, 1, size - 2, size - 2), radius=size // 8, outline=(40, 25, 60, 255), width=size // 24)
    t.save(f"{out}/{name}.webp", lossless=True)

token(tower, 120, "sorcery-tower")
token(garden, 120, "sorcery-garden")
