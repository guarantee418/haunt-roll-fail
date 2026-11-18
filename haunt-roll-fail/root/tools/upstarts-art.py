# Art for the Outsiders Expansion's Upstarts & Renegades deck (slugfacekillah), from its print-and-play card PNGs
# ("Upstarts-CARDS(63x88mm)", 825x1125 with the bleed):
#   python3 upstarts-art.py <that folder> <webp2/root/images/card/upstarts>
# Cards 01 to 29 are the base deck's (same names, costs and art), so only 30 to 54 are cut, at the 512x708 of the other
# deck cards, with the bleed trimmed. The back, and the map pieces (the Murder Hawk, the Cooperative Housing slot and the
# Trailblazers' trail) cut round from the cards' art.
import sys
from PIL import Image, ImageDraw

src, out = sys.argv[1], sys.argv[2]

cards = {
    "30-32": "sabotage", "33": "prophetic-sight", "34": "cooperative-housing", "35": "birdy-boom-boom", "36": "murder-hawk",
    "37": "fox-escort", "38": "mouse-escort", "39": "rabbit-escort", "40-41": "field-medic", "42": "coordinated-movement",
    "43": "burglar-extraordinaire", "44": "rabbit-hood", "45": "cobbler-commander", "46": "salt-factory", "47-48": "peace-offering",
    "49": "trailblazers", "50-51": "salvagers", "52": "tagalongs", "53": "baron-von-costume", "54": "goon-for-hire",
}

def card(path):
    im = Image.open(path).convert("RGB")
    return im.crop((40, 43, 785, 1082)).resize((512, 708), Image.LANCZOS)

for n, name in cards.items():
    card(f"{src}/Upstarts-CARDfront{n}.png").save(f"{out}/upstarts-{name}.webp", quality=88)

card(f"{src}/Upstarts-CARDback01-54.png").save(f"{out}/../card-back-art-ur.webp", quality=88)

def disc(n, box, size, name, ring):
    im = Image.open(f"{src}/Upstarts-CARDfront{n}.png").convert("RGBA").crop(box).resize((size, size), Image.LANCZOS)
    m = Image.new("L", im.size, 0)
    ImageDraw.Draw(m).ellipse((0, 0, size - 1, size - 1), fill=255)
    im.putalpha(m)
    ImageDraw.Draw(im).ellipse((1, 1, size - 2, size - 2), outline=ring, width=size // 20)
    im.save(f"{out}/{name}.webp", lossless=True)

disc("36", (220, 190, 560, 530), 120, "upstarts-hawk", (60, 40, 20, 255))
disc("34", (50, 520, 350, 820), 100, "upstarts-housing", (240, 240, 240, 255))
disc("49", (180, 250, 700, 770), 100, "upstarts-trail", (90, 60, 30, 255))
