package nort
//
//
//
//
import hrf.colmat._
//
//
//
//

import hrf.elem._

import nort.elem._


// The map tiles with a rule of their own (the Wilderness and Wastelands Environment tiles, the Wastelands central tiles
// and The Bridge): the centrepiece of each tile's art (centre and radius, in tile units, on the unturned tile) is left out
// of the territory tints like the resource icons, marked with a small "i", and tapping either opens the tile's rule
// (TileAbilityInfo, asked for by Marzaus and Gman on 2026-11-01)
object TileAbilities {
    case class Feature(x : Double, y : Double, r : Double)

    val features : Map[String, Feature] = Map(
        Waste.jotnar -> Feature(0.40, 0.42, 0.16),
        Waste.kobold -> Feature(0.44, 0.50, 0.15),
        Waste.landvidi -> Feature(0.45, 0.47, 0.14),
        Waste.nastrond -> Feature(0.50, 0.48, 0.16),
        Waste.thor -> Feature(0.52, 0.66, 0.15),
        Waste.urdarbrunn -> Feature(0.63, 0.68, 0.12),
        Waste.vedrfolnir -> Feature(0.50, 0.42, 0.16),
        Wild.lake -> Feature(0.55, 0.45, 0.16),
        "wild-geyser-1" -> Feature(0.62, 0.78, 0.10),
        "wild-geyser-2" -> Feature(0.62, 0.78, 0.10),
        "wild-ruins-1" -> Feature(0.48, 0.58, 0.12),
        "wild-ruins-2" -> Feature(0.47, 0.56, 0.12),
        "wild-swamp" -> Feature(0.50, 0.50, 0.15),
        Wild.poison -> Feature(0.50, 0.55, 0.16),
        "wild-peaks-1" -> Feature(0.50, 0.58, 0.10),
        "wild-peaks-2" -> Feature(0.50, 0.58, 0.10),
        Wild.graveyard -> Feature(0.66, 0.65, 0.12),
        Wild.den -> Feature(0.50, 0.45, 0.15),
        Waste.magma -> Feature(0.50, 0.50, 0.14),
        Waste.yggdrasil -> Feature(0.50, 0.37, 0.14),
        Waste.relic -> Feature(0.45, 0.45, 0.16),
        Waste.lake -> Feature(0.36, 0.48, 0.12),
        Waste.volcano -> Feature(0.50, 0.45, 0.16),
        Waste.mimir -> Feature(0.50, 0.55, 0.12),
        Waste.hrimgandr -> Feature(0.48, 0.52, 0.15),
        Waste.den -> Feature(0.50, 0.45, 0.14),
        Waste.helheim -> Feature(0.52, 0.37, 0.14),
        "horizon-bridge" -> Feature(0.50, 0.48, 0.10),
    )

    def name(tile : String) : String = tile match {
        case Wild.lake => "Great Lake"
        case "wild-geyser-1" | "wild-geyser-2" => "Geyser"
        case "wild-ruins-1" | "wild-ruins-2" => "Ruins"
        case "wild-swamp" => "Swamp"
        case Wild.poison => "Poisonous Swamp"
        case "wild-peaks-1" | "wild-peaks-2" => "High Peaks"
        case Wild.graveyard => "Ancestral Graveyard"
        case Wild.den => "Wyvern's Den"
        case "horizon-bridge" => "The Bridge"
        case t => Waste.name(t)
    }

    def expansion(tile : String) : String =
        if (tile.startsWith("wild-")) "Wilderness Environment tile"
        else if (tile.startsWith("waste-")) "Wastelands Environment tile"
        else if (tile.startsWith("start-")) "Wastelands central tile"
        else "Uncharted Horizons map tile"

    def text(tile : String) : String = tile match {
        case Waste.kobold => "Impassable in the middle. At the Harvest, once for each territory next to it that a player controls, they may exchange 1 food or 1 wood for 1 food, 1 wood or 1 fame."
        case Waste.jotnar => "Impassable in the middle. When it is explored, Jötunn Blainn (a neutral unit worth 2 axes) goes on it. During the Harvest, a player controlling a territory next to the camp may pay 1 food to recruit him into a territory of theirs next to it. Left alone in a territory, or defeated, he goes back to the camp."
        case Waste.nastrond => "Impassable in the middle. When it is revealed, 2 wood go on it; the first player to control a territory next to it takes them."
        case Waste.landvidi => "In a combat in this territory, the defender gets 2 more axes."
        case Waste.thor => "Its controller gets 1 more axe in every combat, anywhere on the map."
        case Waste.urdarbrunn => "In a combat in this territory, the defender ignores 1 skull of the enemy die."
        case Waste.vedrfolnir => "Just before the Harvest, its controller may add the top map tile next to any open territory. This is not an Explore action, so clan and Development card explore effects don't apply."
        case Wild.lake => "Impassable. At each Harvest, the player with the most units and warchiefs in the territories around the lake collects 2 food (1 each when tied). The food is no territory's, so cards and clan powers ignore it."
        case "wild-geyser-1" | "wild-geyser-2" => "At the end of the year, a player controlling a territory with a Geyser may place one unit there (once per Geyser). It doesn't add to a Training Camp."
        case "wild-ruins-1" => "At the Harvest, the controller collects 1 lore and 1 fame, the fame even if the territory isn't closed."
        case "wild-ruins-2" => "At the Harvest, the controller collects 1 lore and 2 fame, the fame even if the territory isn't closed."
        case "wild-swamp" => "Units may move through the Swamp but can't end a move or a retreat there, and units moving through it lose one of them. Creatures may stop there."
        case Wild.poison => "Impassable. At the end of the Harvest, one unit is removed from each territory around it."
        case "wild-peaks-1" | "wild-peaks-2" => "Impassable: nothing crosses the peaks, and the territories on each side are not adjacent."
        case Wild.graveyard => "At the start of the Creature phase, the player controlling the Graveyard territory may place a Spectral Warrior on any territory with a creature's lair (Creatures module)."
        case Wild.den => "When it is explored, the Wyvern goes on it. The Den is a territory of one tile, and gives 2 fame to the player controlling it at the Harvest (Creatures module)."
        case "horizon-bridge" => "Two cliffs behind impassable borders, linked by a bridge that makes them adjacent; the bridge overhangs the valley between them, a third territory."
        case t => CentralChoice.tiles.collectFirst { case c : CentralTileChoice if c.id == t => c.text.capitalize }.|("")
    }

    def has(tile : String) = features.contains(tile)

    // The tapped tile's name, picture and rule
    def view(tile : String) : Elem =
        (name(tile).hlb.div ~ expansion(tile).txt.div(xstyles.smaller85) ~ Image("tile-" + tile, styles.abilityTile).div ~ CombatText(text(tile)).div(styles.abilityText)).div(xlo.flexvcenter)
}

// A tile's centrepiece or its "i" tapped on the map
case class TileAbilityInfo(tile : String)
