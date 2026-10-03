package nort
//
//
//
//
import hrf.colmat._
import hrf.logger._
//
//
//
//

import hrf.elem._

import nort.elem._


// What playing a card does
trait Effect
// Draw n cards; keep, discard and return to the top of the draw pile as many as given
case class DrawEffect(n : Int, keep : Int, discard : Int, back : Int) extends Effect
case class CollectEffect(r : Resource, n : Int) extends Effect
// Take any card from the discard pile, or draw 1
case object NegotiationEffect extends Effect
// Draw 2; may pay any 3 resources to draw 1 more
case object ResourcefulEffect extends Effect
// Needs the map, opponents' cards or combat; not implemented yet
case object MapEffect extends Effect


case class CardInfo(name : String, image : String, fame : Int, flash : Boolean, effect : Effect, text : String)

trait Card extends Elementary with Record {
    def info : CardInfo
    def name = info.name
    def fame : Int = info.fame
    def flash = info.flash
    def effect = info.effect
    def removable : Boolean = true
    def elem : Elem = name.hl
    def img = Image(info.image, styles.card)
}

// Starting cards: Recruit, Move, Explore, Build and two Feasts per player
case class StartCard(id : String) extends Card {
    def info = Cards.start(id)
}

// Clan cards: n = 0 is the initial card, 1 and 2 the upgrades
case class ClanCard(clan : Faction, n : Int) extends Card {
    def info = Cards.clan(clan)(n)
    override def elem = name.styled(clan)
}

case class Development(id : String) extends Card {
    def info = Cards.developments(id)
}

case class Achievement(id : String) extends Card {
    def info = Cards.achievements(id)
    override def elem = name.styled(styles.fame)
}

case object UnrestCard extends Card {
    val info = CardInfo("Unrest", "unrest", 0, false, MapEffect, "This card may not be removed from your deck.")
    override val removable = false
    override def elem = name.styled(xstyles.error)
}


object Cards {
    def card(prefix : String)(id : String, name : String, fame : Int, flash : Boolean, effect : Effect, text : String) =
        id -> CardInfo(name, prefix + id, fame, flash, effect, text)

    val start : Map[String, CardInfo] = $(
        card("card-start-")("recruit", "Recruit", 0, false, MapEffect, "Recruit 1."),
        card("card-start-")("move", "Move", 0, false, MapEffect, "Move 1."),
        card("card-start-")("explore", "Explore", 0, false, MapEffect, "Explore."),
        card("card-start-")("build", "Build", 0, false, MapEffect, "Build."),
        card("card-start-")("feast", "Feast", 0, false, MapEffect, "Recruit 1, Move 1, Explore or Build."),
    ).toMap

    val starting : $[Card] = $("recruit", "move", "explore", "build", "feast", "feast")./(StartCard(_))

    private def clanCards(f : Faction, cards : (String, CardInfo)*) = f -> cards./{ case (id, c) => c.copy(image = "card-clan-" + id) }.$

    val clan : Map[Faction, $[CardInfo]] = $(
        clanCards(Bear,
            "bear" -> CardInfo("Bear Clan", "", 0, true, MapEffect, "Move 1. If Kaija is part of this Move action, before it is resolved collect all food and wood from the territory it leaves."),
            "bear-the-bear-awakens" -> CardInfo("The Bear Awakens", "", 0, true, MapEffect, "Recruit 2. For the rest of this year, Kaija can also move in enemy territories."),
            "bear-protector-of-the-land" -> CardInfo("Protector of the Land", "", 0, false, MapEffect, "Draw 1 card for each of your closed territories of at least 3 tiles in size."),
        ),
        clanCards(Boar,
            "boar" -> CardInfo("Boar Clan", "", 0, true, MapEffect, "Build. You may build for 1 less wood (a small building becomes free)."),
            "boar-osmosis" -> CardInfo("Osmosis", "", 0, false, MapEffect, "Recruit 1 unit in up to 3 of your different territories. Each territory chosen this way must be open or contain wood (excluding buildings)."),
            "boar-lay-of-the-land" -> CardInfo("Lay of the Land", "", 0, false, MapEffect, "Explore. Draw 3 tiles: choose 1 and place the rest at the bottom of the pile. Before placing the chosen tile, collect all resources on it."),
        ),
        clanCards(Goat,
            "goat" -> CardInfo("Goat Clan", "", 0, true, MapEffect, "Build. The new building being placed can be identical to any building already in the same territory."),
            "goat-teamwork" -> CardInfo("Teamwork", "", 0, false, MapEffect, "Choose 2 different actions and resolve them in any order: Recruit, Move, Explore, Build."),
            "goat-resourceful-people" -> CardInfo("Resourceful People", "", 0, false, ResourcefulEffect, "Draw 2 cards. You may pay any 3 resources to draw 1 additional card."),
        ),
        clanCards(Raven,
            "raven" -> CardInfo("Raven Clan", "", 0, true, MapEffect, "Explore. Draw 2 tiles: choose 1, and place the other one on the bottom of the pile."),
            "raven-raiding-party" -> CardInfo("Raiding Party", "", 0, true, MapEffect, "Remove 1 enemy unit and collect 1 resource displayed on its territory."),
            "raven-mercenaries" -> CardInfo("Raven Mercenaries", "", 0, false, MapEffect, "Recruit 2 units in 1 neutral territory. You may spend any 2 resources to recruit 1 additional unit in that same territory."),
        ),
        clanCards(Snake,
            "snake" -> CardInfo("Snake Clan", "", 0, true, MapEffect, "Move 1. After this Move action you may place the Scorched Earth token."),
            "snake-rapacious-exploitation" -> CardInfo("Rapacious Exploitation", "", 0, false, MapEffect, "Look at an opponent's hand and choose 1 card from it. They choose either to discard it or give you any 2 resources of their choice (if they don't have enough resources, they must discard)."),
            "snake-stolen-lore" -> CardInfo("Stolen Lore", "", 0, true, MapEffect, "Copy the effect of any 1 card in the active area belonging to the opponent with the Scorched Earth token."),
        ),
        clanCards(Stag,
            "stag" -> CardInfo("Stag Clan", "", 0, true, MapEffect, "Recruit 1. After this Recruit action, you can move 1 of your units from an adjacent territory into this one (ignoring Rough borders)."),
            "stag-glory-of-the-clan" -> CardInfo("Glory of the Clan", "", 0, false, MapEffect, "Build. Before or after this Build action, you may collect resources in this territory as if it were the Harvest phase."),
            "stag-annexation" -> CardInfo("Annexation", "", 0, true, MapEffect, "Move 1. Before or after this Move action, you may do an Explore action."),
        ),
        clanCards(Wolf,
            "wolf" -> CardInfo("Wolf Clan", "", 0, true, MapEffect, "Move 1. During this Move action, you can cross Rough borders with no penalty (including during retreat)."),
            "wolf-plunder" -> CardInfo("Plunder", "", 0, true, MapEffect, "Remove 1 unit from an adjacent enemy territory to draw 1 card."),
            "wolf-call-to-war" -> CardInfo("Call to War", "", 0, false, MapEffect, "Recruit 1 unit in each of your territories adjacent to an enemy territory."),
        ),
    ).toMap

    private val dev = card("card-dev-") _

    val early : $[(String, CardInfo)] = $(
        dev("negociation", "Negociation", 2, false, NegotiationEffect, "Choose one: take any 1 card from your discard pile OR draw 1 card."),
        dev("scouts", "Scouts", 2, true, MapEffect, "Explore."),
        dev("town-hall", "Town Hall", 1, true, MapEffect, "Recruit 1."),
        dev("merchant", "Merchant", 1, false, DrawEffect(3, 1, 2, 0), "Draw 3 cards; keep 1 and discard 2 of them."),
        dev("militia", "Militia", 0, true, MapEffect, "Move 2."),
        dev("improved-villager-tools", "Improved Villager Tools", 2, true, MapEffect, "Build."),
        dev("recruitment", "Recruitment", 1, false, MapEffect, "Recruit 2. Both units must be placed in the same territory."),
        dev("market-place", "Market Place", 0, false, DrawEffect(2, 2, 0, 0), "Draw 2 cards."),
        dev("mercenary", "Mercenary", 1, false, MapEffect, "Recruit 1. The unit may be placed in a neutral territory."),
        dev("intimidate", "Intimidate", 0, false, MapEffect, "Move 2. Before each combat, you can move 1 defending unit to any adjacent territory that is either neutral, or controlled by the defender."),
        dev("feast", "Feast", 0, false, MapEffect, "Recruit 1, Move 1, Explore or Build."),
        dev("scout-camp", "Scout Camp", 1, false, MapEffect, "Explore. Instead of placing the drawn tile, you may put it on the bottom of the pile and draw 1 new tile."),
        dev("shieldbearers", "Shieldbearers", 0, false, MapEffect, "Move 2. During each combat, cancel 1 casualty inflicted by the defender."),
        dev("bodyguard", "Bodyguard", 0, true, MapEffect, "Move 1, +1 combat point."),
        dev("simple-living", "Simple Living", 1, false, MapEffect, "Build. You can build for 1 less wood (a small building becomes free)."),
        dev("future-sight", "Future Sight", 0, false, MapEffect, "Pick up 1 available Development or Achievement card and place it on top of your draw pile. You will not pick another card this year."),
    )

    val advanced : $[(String, CardInfo)] = $(
        dev("spy", "Spy", 2, false, MapEffect, "Look at an opponent's hand and discard 1 card from it. Draw 1 card."),
        dev("fishing", "Fishing", 2, true, CollectEffect(Food, 3), "Collect 3 food."),
        dev("axe-throwers", "Axe Throwers", 0, false, MapEffect, "Move 2, +1 casualty or +1 combat point. Choose this combat bonus after rolling your die and before the defender's roll."),
        dev("lore-mastery", "Lore Mastery", 2, true, CollectEffect(Lore, 2), "Collect 2 lore."),
        dev("industrious-villagers", "Industrious Villagers", 3, false, MapEffect, "Build. After this Build action, you may replace 1 of your buildings on the map by another from the reserve. It has to be the same size, and can be identical to any building already in the territory."),
        dev("defensive-strategy", "Defensive Strategy", 3, false, MapEffect, "Play this in response to an opponent playing a card on their turn; discard it instead of resolving its effects. They may play another card or choose another action."),
        dev("enemy-secrets", "Enemy Secrets", 0, false, MapEffect, "Choose 1 enemy territory adjacent to yours. Copy the effect of 1 card in its owner's active area."),
        dev("hunters", "Hunters", 1, false, MapEffect, "Recruit 1 unit per food in each of your territories (including buildings)."),
        dev("outpost", "Outpost", 1, false, MapEffect, "Recruit 2. Both units must be placed in the same friendly or neutral territory."),
        dev("hidden-ways", "Hidden Ways", 0, false, MapEffect, "Move any number of units from 1 of your territories to any open territory."),
        dev("carpentry-mastery", "Carpentry Mastery", 2, false, MapEffect, "Build twice, but one of the buildings must be small."),
        dev("conqueror", "Conqueror", 2, true, MapEffect, "For the duration of this year, ignore enemy Defense Towers and Fortresses."),
        dev("huginn-and-muninn", "Huginn & Muninn", 3, false, MapEffect, "Explore. You can explore from any open territory on the map."),
        dev("upgraded-market-place", "Upgraded Market Place", 1, true, DrawEffect(2, 2, 0, 0), "Draw 2 cards."),
        dev("grizzled-warriors", "Grizzled Warriors", 0, false, MapEffect, "Move 2, +2 combat points."),
        dev("upgraded-trading-post", "Upgraded Trading Post", 0, false, DrawEffect(3, 2, 1, 0), "Draw 3 cards; keep 2 and discard 1 of them."),
        dev("ancestral-curse", "Ancestral Curse", 2, false, MapEffect, "All your opponents discard 1 card of their choice. Draw 1 card."),
        dev("rangers", "Rangers", 1, false, MapEffect, "Explore. After the first Explore action, you may do a second Explore action if possible."),
        dev("upgraded-scout-camp", "Upgraded Scout Camp", 0, false, MapEffect, "Explore. Draw 2 tiles; keep 1 to explore, and put the other on the bottom of the pile."),
        dev("greater-trade-routes", "Greater Trade Routes", 0, false, DrawEffect(3, 2, 0, 1), "Draw 3 cards; keep 2 and return 1 to the top of your draw pile."),
        dev("woodcutters", "Woodcutters", 1, false, MapEffect, "Recruit 1 unit per wood in each of your territories (including buildings)."),
        dev("infiltration", "Infiltration", 0, false, MapEffect, "Move 3. You can move through enemy territories without stopping."),
        dev("upgraded-town-hall", "Upgraded Town Hall", 1, true, MapEffect, "Recruit 2."),
        dev("amenities", "Amenities", 2, false, MapEffect, "Build. If you build a small building (including Carved Stone), it doesn't take any space; place it anywhere in the territory."),
        dev("wood-monopoly", "Wood Monopoly", 2, true, CollectEffect(Wood, 3), "Collect 3 wood."),
        dev("bribery", "Bribery", 3, true, MapEffect, "Move up to 2 enemy units from a single territory to an adjacent territory (ignoring Rough borders). If it triggers a combat, resolve it."),
        dev("incursion", "Incursion", 0, false, MapEffect, "Move 4."),
        dev("heroic-charge", "Heroic Charge", 0, false, MapEffect, "Move 3, +1 combat point."),
        dev("trading-post", "Trading Post", 1, false, DrawEffect(3, 1, 1, 1), "Draw 3 cards; keep 1, discard 1, and return 1 to the top of your draw pile."),
        dev("legendary-heroes", "Legendary Heroes", 2, true, MapEffect, "Resolve the effect of a card you played this year."),
        dev("allies-from-the-wild", "Allies from the Wild", 1, false, MapEffect, "Recruit 3. The units must be placed in neutral territories only."),
        dev("loremasters", "Loremasters", 1, false, MapEffect, "Recruit 1 unit per lore in each of your territories (including buildings)."),
        dev("warriors", "Warriors", 0, false, MapEffect, "Move 2, +1 combat point."),
        dev("cunning-merchant", "Cunning Merchant", 1, false, DrawEffect(3, 1, 0, 2), "Draw 3 cards; keep 1 and return 2 to the top of your draw pile."),
        dev("capture", "Capture", 3, true, MapEffect, "Remove 1 enemy unit from a territory adjacent to yours. Add 1 unit to any one of your territories."),
    )

    val developments : Map[String, CardInfo] = (early ++ advanced).toMap

    val earlyCards : $[Card] = early.lefts./(Development(_))
    val advancedCards : $[Card] = advanced.lefts./(Development(_))

    // Achievement fame is worked out at the end of the game
    val achievements : Map[String, CardInfo] = $(
        card("card-achievement-")("builder", "Builder", 0, false, MapEffect, "Gain 1 fame per small building and 3 fame per large building you control."),
        card("card-achievement-")("explorer", "Explorer", 0, false, MapEffect, "Gain 1 fame per empty small space and 3 fame per empty large space in your territories."),
        card("card-achievement-")("food-trader", "Food Trader", 0, false, MapEffect, "Gain 2 fame for each food on your territories (including buildings)."),
        card("card-achievement-")("scholar", "Scholar", 0, false, MapEffect, "Gain 2 fame for each lore on your territories (including buildings)."),
        card("card-achievement-")("trapper", "Trapper", 0, false, MapEffect, "Gain 3 fame per creature's lair in your territories."),
        card("card-achievement-")("warlord", "Warlord", 0, false, MapEffect, "Gain 1 fame per unit in your territories."),
        card("card-achievement-")("wood-trader", "Wood Trader", 0, false, MapEffect, "Gain 2 fame for each wood on your territories (including buildings)."),
    ).toMap

    val achievementCards : $[Card] = achievements.keys.$.sorted./(Achievement(_))

    // Image names for the asset list
    def images : $[String] =
        start.values.$./(_.image) ++ clan.values.$.flatten./(_.image) ++ developments.values.$./(_.image) ++ achievements.values.$./(_.image) :+ UnrestCard.info.image
}
