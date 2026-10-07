#!/usr/bin/env python3
# Cuts the French Root deck cards and faction boards for the French display language
# (root/french.scala) from the Tabletop Simulator mod "Root FR" (Steam workshop 1829904481,
# the official Matagot French components) and "ROOT + FANFACTIONS - FR" (2049935670).
#
#   python3 root/tools/french-assets.py <download dir>
#
# Writes webp2/root/images/card/deck/fr/<card id>.webp (512x708, like the English deck cards)
# and webp2/root/images/faction/fr/<faction>-board.webp (the size of the English boards).
import os, sys, urllib.request
from PIL import Image

UGC = "https://cdn.steamusercontent.com/ugc/"

BASE = "771743400535630432/98165E0BC7082D47C056809FE9B40F2EE91CAC48"
EXILES = "771743400535646806/21896B6CC1EC553FCCDAE30E357A572599F5667D"

# sheet position (row * 10 + column on the 10x6 sheet) -> card id
BASE_CARDS = {
    0: "armorers", 2: "woodland-runners", 3: "arms-trader", 4: "bird-crossbow", 5: "sappers", 7: "brutal-tactics", 9: "royal-claim",
    10: "fox-ambush", 11: "gently-used-knapsack", 12: "fox-root-tea", 13: "fox-travel-gear", 14: "protection-racket", 15: "foxfolk-steel", 16: "anvil", 17: "stand-and-deliver", 19: "tax-collector",
    22: "favor-of-the-foxes", 23: "rabbit-ambush", 24: "smugglers-trail", 25: "rabbit-root-tea", 26: "a-visit-to-friends", 27: "bake-sale", 28: "command-warren",
    30: "better-burrow-bank", 32: "cobbler", 34: "favor-of-the-rabbits", 35: "mouse-ambush", 36: "mouse-in-a-sack", 37: "mouse-root-tea", 38: "mouse-travel-gear", 39: "investments",
    40: "sword", 41: "mouse-crossbow", 42: "scouting-party", 44: "bird-ambush", 46: "birdy-bindle", 47: "bird-dominance", 48: "fox-dominance", 49: "codebreakers",
    51: "mouse-dominance", 52: "favor-of-the-mice", 53: "rabbit-dominance",
}

EXILES_CARDS = {
    0: "saboteurs", 1: "soup-kitchens", 2: "boat-builders", 3: "corvid-planners", 4: "eyrie-emigre", 5: "fox-partisans", 6: "propaganda-bureau", 7: "false-orders", 9: "informants",
    11: "rabbit-partisans", 12: "tunnels", 14: "charm-offensive", 15: "coffin-makers", 16: "swap-meet", 18: "mouse-partisans", 19: "league-of-adventurous-mice",
    21: "murine-broker", 22: "master-engravers",
}

BOARDS = {
    "mc": "770616235210786476/4D2CCF0564A06FCCCF74BBB72F91245480E55C1D",
    "ed": "770615928468474898/E2BF4844311675BFE7AC96117908277DB8068050",
    "wa": "773977439692332857/05D2656DCDB8CAC39690BB4E3557092E8AF64177",
    "vb": "770615928461998842/7011ACAAC2933C9759F4ED72CA627AA10D85E161",
    "lc": "770616219900631339/0BFE7B56A116BE73EC66CB2D989110F1851B4256",
    "rf": "770615928471025292/D59986B3924366E04C1B220240EE7EF9AA6CA42C",
    "cc": "771743400535573608/B06570FFE8548C7D89D5C37CC7775FF2718011B0",
    "ud": "770616235209713943/C3A2F3B815AC97EFA59506116D7E038458390C18",
}

ENGLISH_BOARDS = {"mc": "feline", "ed": "aviary", "wa": "insurgent", "vb": "hero", "lc": "fanatic", "rf": "trader", "cc": "mischief", "ud": "underground"}

def fetch(dir, ugc):
    path = os.path.join(dir, ugc.split("/")[0] + ".img")
    if not os.path.exists(path):
        urllib.request.urlretrieve(UGC + ugc + "/", path)
    return Image.open(path).convert("RGB")

def main():
    dir = sys.argv[1]
    images = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "..", "webp2", "root", "images")
    cards = os.path.join(images, "card", "deck", "fr")
    boards = os.path.join(images, "faction", "fr")
    os.makedirs(cards, exist_ok=True)
    os.makedirs(boards, exist_ok=True)

    for ugc, l in [(BASE, BASE_CARDS), (EXILES, EXILES_CARDS)]:
        sheet = fetch(dir, ugc)
        w, h = sheet.size[0] / 10, sheet.size[1] / 6
        for n, id in l.items():
            x, y = n % 10, n // 10
            card = sheet.crop((round(x * w), round(y * h), round((x + 1) * w), round((y + 1) * h))).resize((512, 708), Image.LANCZOS)
            card.save(os.path.join(cards, id + ".webp"), quality=80, method=6)

    for f, ugc in BOARDS.items():
        board = fetch(dir, ugc)
        english = Image.open(os.path.join(images, "faction", ENGLISH_BOARDS[f], f + "-board.webp"))
        board.resize(english.size, Image.LANCZOS).save(os.path.join(boards, f + "-board.webp"), quality=80, method=6)

main()
