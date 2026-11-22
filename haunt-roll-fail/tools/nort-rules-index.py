#!/usr/bin/env python3
# THE NORTHGARD RULE INDEX for the Discord bot's /rule command (good-game/Lookup.scala): the rulebooks have no rule numbers, so a
# rule is cited by its book, page and section heading, and the bot shows the page. The sections are listed here by hand, from the
# headings printed in each book; the pages are rendered from the PDFs (not in the repository: the core rulebook from Hachette's
# product page, Warchiefs, Wilderness, Uncharted Horizons and Training Fields from Tabletopia's Northgard rules, Wastelands and New
# Blood as the owner uploaded them, 2026-11-22). Run it again after adding a book or section:
#   python3 haunt-roll-fail/tools/nort-rules-index.py <directory of the PDFs> haunt-roll-fail/webp2
# It writes webp2/lfg/rules-nort.tsv (book, page, section, image path) and the pages, webp2/lfg/rulebook/nort/<book>-<page>.webp.
import os, subprocess, sys, tempfile
from PIL import Image

BOOKS = {
    # file, the book's name as cited
    "core": ("core-rulebook.pdf", "Core rulebook"),
    "warchiefs": ("warchiefs-rulebook.pdf", "Warchiefs"),
    "wilderness": ("wilderness-rulebook.pdf", "Wilderness"),
    "wastelands": ("wastelands-rulebook.pdf", "Wastelands"),
    "newblood": ("new-blood-rulebook.pdf", "New Blood"),
    "horizons": ("uncharted-horizons-rulebook.pdf", "Uncharted Horizons"),
    "training": ("training-fields-rules.pdf", "Training Fields"),
}

# book, PDF page, section
SECTIONS = """
core 2 Components
core 3 Goal of the Game
core 3 The Map Tiles
core 4 Game Setup
core 5 Deck Building
core 6 Phases of a Year
core 6 Start of Year
core 6 Actions phase
core 6 Wait
core 6 Replace a card
core 6 Remove a card
core 6 Upgrade
core 6 Pass
core 6 Flash cards
core 7 Harvest
core 7 Trade resources
core 7 Winter
core 7 End of Year
core 8 Anatomy of a Card
core 9 Actions
core 9 The cards are always right
core 9 Recruit Action
core 10 Build Action
core 10 Small Buildings
core 10 Large Buildings
core 11 Move Action
core 12 Explore Action
core 13 Draw Actions
core 13 Special Actions
core 14 Combat
core 14 Combat points
core 14 Combat factors
core 14 Dice results
core 14 Combat steps
core 14 Loser retreats
core 15 Combat examples
core 16 End of the Game and Winner
core 16 Variants
core 16 Longer games
core 16 Team play
core 17 Clan Powers
core 17 The Bear token (Kaija)
core 17 The Snake token (Scorched Earth)
core 18 Creatures module
core 19 Creature colors
core 19 Creature card anatomy
core 20 Creatures setup
core 20 Creatures sharing a territory
core 20 General creature rules
core 20 Creature apparition
core 20 Creature effects and activation
core 20 Explore action (creatures)
core 20 Combat against creatures
core 21 Combat resolution (creatures)
core 22 Creature phase
core 22 Creature movement
core 23 Variant: More Creatures!
core 23 Player pass (creatures)
core 24 Creatures details
core 24 Creature activation summary
warchiefs 1 Components
warchiefs 2 New components for the core game
warchiefs 2 Warchiefs module
warchiefs 2 Warchiefs general rules
warchiefs 2 Warchiefs in combat
warchiefs 2 Warchiefs powers
wilderness 1 Components
wilderness 2 New creatures
wilderness 2 Eldthurs
wilderness 3 Hvedrung
wilderness 4 Spectral Warrior
wilderness 4 Wyvern
wilderness 5 Wyvern activation example
wilderness 6 Environment Tiles module
wilderness 6 Environment tiles rules
wilderness 6 Great Lake
wilderness 7 Geysers
wilderness 7 Poisonous Swamp
wilderness 7 High Peaks
wilderness 7 Ruins
wilderness 8 Environment tiles requiring the Creatures module
wilderness 8 Ancestral Graveyard
wilderness 8 Wyvern's Den
wastelands 1 Components
wastelands 2 New creatures
wastelands 2 Rock Golem
wastelands 2 Myrkalfar
wastelands 3 Giant Boar
wastelands 3 Valdemar
wastelands 4 Environments module
wastelands 4 Kobold Camp
wastelands 4 Thor's Wrath
wastelands 5 Vedrfolnir
wastelands 5 Landvidi
wastelands 5 Urdarbrunn
wastelands 6 Jötnar Camp
wastelands 6 Jötunn Blainn
wastelands 7 Central tiles
wastelands 7 Magma Flow
wastelands 7 Yggdrasil
wastelands 7 Relic of the Gods
wastelands 8 Great Lake (central tile)
wastelands 8 Volcano
wastelands 8 Mimirsbrunn
wastelands 9 Hrimgandr's Lair
wastelands 9 Hrimgandr
wastelands 10 Central tiles with the Creatures module
wastelands 10 Wyvern's Den (central tile)
wastelands 11 Gate of Helheim
newblood 1 Components
newblood 2 Rat Clan
newblood 2 Squirrel Clan
newblood 3 Kraken Clan
newblood 3 High Tide tokens
newblood 4 Dragon Clan
newblood 4 Sacrificial Pyre
newblood 5 Lynx Clan
newblood 5 Brundr and Kaelinn
newblood 6 Ox Clan
newblood 6 Ancestral Equipment tokens
newblood 7 Ancestral Equipment token effects
newblood 8 Horse Clan
newblood 8 Eitria and Brok
horizons 2 Components
horizons 3 New clans (first printing)
horizons 6 Creatures module (Horizons)
horizons 7 Environment module (Horizons)
horizons 9 Sea module
horizons 9 Beach tile
horizons 10 Raid cards
horizons 10 Raids rules
horizons 11 Events module
horizons 12 Alternative victory conditions module
horizons 12 End of the game changes
horizons 13 Solo module (Automa)
horizons 13 Compatible modules
horizons 14 Automa's rules
horizons 14 Automa card anatomy
horizons 14 Automa playing priorities
horizons 15 Automa actions
horizons 16 Automa combat rules
horizons 16 Automa priority for drawing
horizons 17 Automa harvest and end of the year
horizons 17 Adjustments to clan upgrade cards
horizons 18 Priority of Automa card choices
horizons 19 Sea module setups
training 1 Training Fields module
training 2 Goal of the game
training 2 Game turns
training 2 Actions
training 2 Recruit
training 3 Building benefits
training 3 Refresh
training 3 Explore
training 3 Special
"""

def main():
    src, webp = sys.argv[1], sys.argv[2]
    out_dir = os.path.join(webp, "lfg", "rulebook", "nort")
    os.makedirs(out_dir, exist_ok=True)
    rows = []
    pages = set()
    for line in SECTIONS.strip().splitlines():
        book, page, section = line.split(" ", 2)
        path = "webp2/lfg/rulebook/nort/%s-%s.webp" % (book, page)
        rows.append((BOOKS[book][1], page, section, path))
        pages.add((book, int(page)))
    with tempfile.TemporaryDirectory() as tmp:
        for book, page in sorted(pages):
            png = os.path.join(tmp, "p")
            subprocess.run(["pdftoppm", "-f", str(page), "-l", str(page), "-r", "150", "-png", "-singlefile",
                            os.path.join(src, BOOKS[book][0]), png], check=True)
            im = Image.open(png + ".png").convert("RGB")
            # Discord shows it small and opens it full size: wide enough to read, light enough to load
            w = 1400 if im.width < im.height else 2400
            if im.width > w:
                im = im.resize((w, round(im.height * w / im.width)), Image.LANCZOS)
            im.save(os.path.join(out_dir, "%s-%d.webp" % (book, page)), "WEBP", quality=72, method=6)
    with open(os.path.join(webp, "lfg", "rules-nort.tsv"), "w", encoding="utf-8") as f:
        f.write("book\tpage\tsection\tpath\n")
        for r in rows:
            f.write("\t".join(r) + "\n")
    print("nort-rules-index:", len(rows), "sections,", len(pages), "pages")

if __name__ == "__main__":
    main()
