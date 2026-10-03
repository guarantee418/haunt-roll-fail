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
trait Effect extends Record
// Draw n cards; keep, discard and return to the top of the draw pile as many as given
case class DrawEffect(n : Int, keep : Int, discard : Int, back : Int) extends Effect
case class CollectEffect(r : Resource, n : Int) extends Effect
// Take any card from the discard pile, or draw 1
case object NegotiationEffect extends Effect
// Draw 2; may pay any 3 resources to draw 1 more
case object ResourcefulEffect extends Effect
// Not implemented yet
case object MapEffect extends Effect

trait RecruitMode extends Record
// In controlled territories, or any neutral one with no units on the map
case object RecruitNormal extends RecruitMode
// All in the same territory
case object RecruitSame extends RecruitMode
// May also go in neutral territories
case object RecruitNeutral extends RecruitMode
// Neutral territories only
case object RecruitNeutralOnly extends RecruitMode
// All in the same neutral territory
case object RecruitNeutralSame extends RecruitMode
// All in the same friendly or neutral territory
case object RecruitSameAny extends RecruitMode
// Osmosis: different territories, each open or with wood on its tiles
case object RecruitOsmosis extends RecruitMode

case class RecruitEffect(n : Int, mode : RecruitMode = RecruitNormal) extends Effect
// What a Move card does besides moving
trait MoveSpecial extends Record
case object PlainMove extends MoveSpecial
// Bear Clan: Kaija collects food and wood from the territory it leaves
case object BearMove extends MoveSpecial
// Snake Clan: may place the Scorched Earth token after the move
case object SnakeMove extends MoveSpecial
// Shieldbearers: cancel 1 casualty inflicted by the defender
case object ShieldMove extends MoveSpecial
// Infiltration: units may keep moving out of enemy territories
case object InfiltrateMove extends MoveSpecial
// Axe Throwers: +1 casualty or +1 point, chosen after the attacker's roll
case object AxeMove extends MoveSpecial
// Intimidate: before each combat, may push 1 defending unit out
case object IntimidateMove extends MoveSpecial
// The warchief upgrade cards (Warchiefs box), one per clan:
// Egil's Fury: +1 casualty; before the combats, may remove 1 building from an attacked territory
case object EgilMove extends MoveSpecial
// Brand's Bravery: the winner chooses where the enemy retreats
case object BrandMove extends MoveSpecial
// Borgild's Shield: before moving, Kaija and Borgild may join each other; cancel 1 casualty inflicted by the defender
case object BorgildMove extends MoveSpecial
// Signy's Celerity: may move through the territory with the Scorched Earth token without stopping
case object SignyMove extends MoveSpecial
// Liv's Cunning: may spend wood or lore instead of food in combat
case object LivMove extends MoveSpecial
// Halvard's Craft: a free small building in a territory newly controlled after the move
case object HalvardMove extends MoveSpecial
// Svarn's Menders: friendly casualties come back after the combats, in any of the player's territories
case object SvarnMove extends MoveSpecial

case class MoveEffect(n : Int, bonus : Int = 0, ignoreRough : Boolean = false, special : MoveSpecial = PlainMove) extends Effect
// Draw tiles and keep one; redraw: may put the drawn tile back once; anywhere: from any open territory
// collect: Lay of the Land collects the resources shown on the tile
case class ExploreEffect(draw : Int = 1, times : Int = 1, redraw : Boolean = false, anywhere : Boolean = false, collect : Boolean = false) extends Effect

trait BuildSpecial extends Record
case object PlainBuild extends BuildSpecial
// Carpentry Mastery: one of the two buildings must be small
case object CarpentryBuild extends BuildSpecial
// Amenities: small buildings take no space
case object AmenitiesBuild extends BuildSpecial
// Glory of the Clan: collect the territory's resources
case object GloryBuild extends BuildSpecial
// Industrious Villagers: may replace a building afterwards
case object IndustriousBuild extends BuildSpecial

// discount: wood saved; duplicate: may match a building already in the territory
case class BuildEffect(discount : Int = 0, times : Int = 1, duplicate : Boolean = false, special : BuildSpecial = PlainBuild) extends Effect
// Recruit 1, Move 1, Explore or Build
case object FeastEffect extends Effect
// The Bear Awakens: Recruit 2, and Kaija may enter enemy territories this year
case object AwakenEffect extends Effect
// Protector of the Land: draw 1 per closed territory of 3 or more tiles
case object ProtectorEffect extends Effect


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
    def handImg = Image(info.image, styles.handCard)
}

// Starting cards: Recruit, Move, Explore, Build and two Feasts per player
case class StartCard(color : PlayerColor, id : String) extends Card {
    def info = Cards.start(id).copy(image = "card-start-" + color.id + "-" + id)
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
    val info = CardInfo("Unrest", "card-unrest", 0, false, MapEffect, "This card may not be removed from your deck.")
    override val removable = false
    override def elem = name.styled(xstyles.error)

    // Cards in the box
    val supply = 10
}


object Cards {
    def card(prefix : String)(id : String, name : String, fame : Int, flash : Boolean, effect : Effect, text : String) =
        id -> CardInfo(name, prefix + id, fame, flash, effect, text)

    val start : Map[String, CardInfo] = $(
        card("card-start-")("recruit", "Recruit", 0, false, RecruitEffect(1), "Recruit 1."),
        card("card-start-")("move", "Move", 0, false, MoveEffect(1), "Move 1."),
        card("card-start-")("explore", "Explore", 0, false, ExploreEffect(), "Explore."),
        card("card-start-")("build", "Build", 0, false, BuildEffect(), "Build."),
        card("card-start-")("feast", "Feast", 0, false, FeastEffect, "Recruit 1, Move 1, Explore or Build."),
    ).toMap

    def starting(color : PlayerColor) : $[Card] = $("recruit", "move", "explore", "build", "feast", "feast")./(StartCard(color, _))

    private def clanCards(f : Faction, cards : (String, CardInfo)*) = f -> cards./{ case (id, c) => c.copy(image = "card-clan-" + id) }.$

    val clan : Map[Faction, $[CardInfo]] = $(
        clanCards(Bear,
            "bear" -> CardInfo("Bear Clan", "", 0, true, MoveEffect(1, special = BearMove), "Move 1. If Kaija is part of this Move action, before it is resolved collect all food and wood from the territory it leaves."),
            "bear-the-bear-awakens" -> CardInfo("The Bear Awakens", "", 0, true, AwakenEffect, "Recruit 2. For the rest of this year, Kaija can also move in enemy territories."),
            "bear-protector-of-the-land" -> CardInfo("Protector of the Land", "", 0, false, ProtectorEffect, "Draw 1 card for each of your closed territories of at least 3 tiles in size."),
            "bear-borgilds-shield" -> CardInfo("Borgild's Shield", "", 0, false, MoveEffect(2, special = BorgildMove), "Move 2. Before your Move action, you may place Kaija into Borgild's territory or vice versa. During each combat, cancel 1 casualty inflicted by the defender."),
        ),
        clanCards(Boar,
            "boar" -> CardInfo("Boar Clan", "", 0, true, BuildEffect(discount = 1), "Build. You may build for 1 less wood (a small building becomes free)."),
            "boar-osmosis" -> CardInfo("Osmosis", "", 0, false, OsmosisEffect, "Recruit 1 unit in up to 3 of your different territories. Each territory chosen this way must be open or contain wood (excluding buildings)."),
            "boar-lay-of-the-land" -> CardInfo("Lay of the Land", "", 0, false, ExploreEffect(draw = 3, collect = true), "Explore. Draw 3 tiles: choose 1 and place the rest at the bottom of the pile. Before placing the chosen tile, collect all resources on it."),
            "boar-svarns-menders" -> CardInfo("Svarn's Menders", "", 0, false, MoveEffect(2, special = SvarnMove), "Move 2. During each combat, place friendly casualties on this card, and after all combats are resolved, place them in any number of your territories."),
        ),
        clanCards(Goat,
            "goat" -> CardInfo("Goat Clan", "", 0, true, BuildEffect(duplicate = true), "Build. The new building being placed can be identical to any building already in the same territory."),
            "goat-teamwork" -> CardInfo("Teamwork", "", 0, false, TeamworkEffect, "Choose 2 different actions and resolve them in any order: Recruit, Move, Explore, Build."),
            "goat-resourceful-people" -> CardInfo("Resourceful People", "", 0, false, ResourcefulEffect, "Draw 2 cards. You may pay any 3 resources to draw 1 additional card."),
            "goat-halvards-craft" -> CardInfo("Halvard's Craft", "", 0, false, MoveEffect(2, special = HalvardMove), "Move 2. If you control new territories after this Move action, you may place 1 small building in one of them at no cost."),
        ),
        clanCards(Raven,
            "raven" -> CardInfo("Raven Clan", "", 0, true, ExploreEffect(draw = 2), "Explore. Draw 2 tiles: choose 1, and place the other one on the bottom of the pile."),
            "raven-raiding-party" -> CardInfo("Raiding Party", "", 0, true, RaidEffect, "Remove 1 enemy unit and collect 1 resource displayed on its territory."),
            "raven-mercenaries" -> CardInfo("Raven Mercenaries", "", 0, false, MercenariesEffect, "Recruit 2 units in 1 neutral territory. You may spend any 2 resources to recruit 1 additional unit in that same territory."),
            "raven-livs-cunning" -> CardInfo("Liv's Cunning", "", 0, false, MoveEffect(4, special = LivMove), "Move 4. During combat, you may discard wood or lore instead of food to gain bonus points."),
        ),
        clanCards(Snake,
            "snake" -> CardInfo("Snake Clan", "", 0, true, MoveEffect(1, special = SnakeMove), "Move 1. After this Move action you may place the Scorched Earth token."),
            "snake-rapacious-exploitation" -> CardInfo("Rapacious Exploitation", "", 0, false, RapaciousEffect, "Look at an opponent's hand and choose 1 card from it. They choose either to discard it or give you any 2 resources of their choice (if they don't have enough resources, they must discard)."),
            "snake-stolen-lore" -> CardInfo("Stolen Lore", "", 0, true, StolenLoreEffect, "Copy the effect of any 1 card in the active area belonging to the opponent with the Scorched Earth token."),
            "snake-signys-celerity" -> CardInfo("Signy's Celerity", "", 0, false, MoveEffect(3, special = SignyMove), "Move 3. You may move through the territory with the Scorched Earth token without stopping."),
        ),
        clanCards(Stag,
            "stag" -> CardInfo("Stag Clan", "", 0, true, RecruitEffect(1), "Recruit 1. After this Recruit action, you can move 1 of your units from an adjacent territory into this one (ignoring Rough borders)."),
            "stag-glory-of-the-clan" -> CardInfo("Glory of the Clan", "", 0, false, BuildEffect(special = GloryBuild), "Build. Before or after this Build action, you may collect resources in this territory as if it were the Harvest phase."),
            "stag-annexation" -> CardInfo("Annexation", "", 0, true, AnnexationEffect, "Move 1. Before or after this Move action, you may do an Explore action."),
            "stag-brands-bravery" -> CardInfo("Brand's Bravery", "", 0, false, MoveEffect(2, bonus = 1, special = BrandMove), "Move 2, +1 combat point. For each combat won, you choose which territory the enemy retreats to (the retreat move must be legal)."),
        ),
        clanCards(Wolf,
            "wolf" -> CardInfo("Wolf Clan", "", 0, true, MoveEffect(1, ignoreRough = true), "Move 1. During this Move action, you can cross Rough borders with no penalty (including during retreat)."),
            "wolf-plunder" -> CardInfo("Plunder", "", 0, true, PlunderEffect, "Remove 1 unit from an adjacent enemy territory to draw 1 card."),
            "wolf-call-to-war" -> CardInfo("Call to War", "", 0, false, CallToWarEffect, "Recruit 1 unit in each of your territories adjacent to an enemy territory."),
            "wolf-egils-fury" -> CardInfo("Egil's Fury", "", 0, false, MoveEffect(3, special = EgilMove), "Move 3, +1 casualty. Before combats are resolved, you may remove 1 building from a territory which is being attacked. Place it back in the reserve."),
        ),
    ).toMap

    private val dev = card("card-dev-") _

    val early : $[(String, CardInfo)] = $(
        dev("negociation", "Negociation", 2, false, NegotiationEffect, "Choose one: take any 1 card from your discard pile OR draw 1 card."),
        dev("scouts", "Scouts", 2, true, ExploreEffect(), "Explore."),
        dev("town-hall", "Town Hall", 1, true, RecruitEffect(1), "Recruit 1."),
        dev("merchant", "Merchant", 1, false, DrawEffect(3, 1, 2, 0), "Draw 3 cards; keep 1 and discard 2 of them."),
        dev("militia", "Militia", 0, true, MoveEffect(2), "Move 2."),
        dev("improved-villager-tools", "Improved Villager Tools", 2, true, BuildEffect(), "Build."),
        dev("recruitment", "Recruitment", 1, false, RecruitEffect(2, RecruitSame), "Recruit 2. Both units must be placed in the same territory."),
        dev("market-place", "Market Place", 0, false, DrawEffect(2, 2, 0, 0), "Draw 2 cards."),
        dev("mercenary", "Mercenary", 1, false, RecruitEffect(1, RecruitNeutral), "Recruit 1. The unit may be placed in a neutral territory."),
        dev("intimidate", "Intimidate", 0, false, MoveEffect(2, special = IntimidateMove), "Move 2. Before each combat, you can move 1 defending unit to any adjacent territory that is either neutral, or controlled by the defender."),
        dev("feast", "Feast", 0, false, FeastEffect, "Recruit 1, Move 1, Explore or Build."),
        dev("scout-camp", "Scout Camp", 1, false, ExploreEffect(redraw = true), "Explore. Instead of placing the drawn tile, you may put it on the bottom of the pile and draw 1 new tile."),
        dev("shieldbearers", "Shieldbearers", 0, false, MoveEffect(2, special = ShieldMove), "Move 2. During each combat, cancel 1 casualty inflicted by the defender."),
        dev("bodyguard", "Bodyguard", 0, true, MoveEffect(1, bonus = 1), "Move 1, +1 combat point."),
        dev("simple-living", "Simple Living", 1, false, BuildEffect(discount = 1), "Build. You can build for 1 less wood (a small building becomes free)."),
        dev("future-sight", "Future Sight", 0, false, FutureSightEffect, "Pick up 1 available Development or Achievement card and place it on top of your draw pile. You will not pick another card this year."),
    )

    val advanced : $[(String, CardInfo)] = $(
        dev("spy", "Spy", 2, false, SpyEffect, "Look at an opponent's hand and discard 1 card from it. Draw 1 card."),
        dev("fishing", "Fishing", 2, true, CollectEffect(Food, 3), "Collect 3 food."),
        dev("axe-throwers", "Axe Throwers", 0, false, MoveEffect(2, special = AxeMove), "Move 2, +1 casualty or +1 combat point. Choose this combat bonus after rolling your die and before the defender's roll."),
        dev("lore-mastery", "Lore Mastery", 2, true, CollectEffect(Lore, 2), "Collect 2 lore."),
        dev("industrious-villagers", "Industrious Villagers", 3, false, BuildEffect(special = IndustriousBuild), "Build. After this Build action, you may replace 1 of your buildings on the map by another from the reserve. It has to be the same size, and can be identical to any building already in the territory."),
        dev("defensive-strategy", "Defensive Strategy", 3, false, DefensiveEffect, "Play this in response to an opponent playing a card on their turn; discard it instead of resolving its effects. They may play another card or choose another action."),
        dev("enemy-secrets", "Enemy Secrets", 0, false, SecretsEffect, "Choose 1 enemy territory adjacent to yours. Copy the effect of 1 card in its owner's active area."),
        dev("hunters", "Hunters", 1, false, RecruitPerEffect(Food), "Recruit 1 unit per food in each of your territories (including buildings)."),
        dev("outpost", "Outpost", 1, false, RecruitEffect(2, RecruitSameAny), "Recruit 2. Both units must be placed in the same friendly or neutral territory."),
        dev("hidden-ways", "Hidden Ways", 0, false, HiddenWaysEffect, "Move any number of units from 1 of your territories to any open territory."),
        dev("carpentry-mastery", "Carpentry Mastery", 2, false, BuildEffect(times = 2, special = CarpentryBuild), "Build twice, but one of the buildings must be small."),
        dev("conqueror", "Conqueror", 2, true, ConquerorEffect, "For the duration of this year, ignore enemy Defense Towers and Fortresses."),
        dev("huginn-and-muninn", "Huginn & Muninn", 3, false, ExploreEffect(anywhere = true), "Explore. You can explore from any open territory on the map."),
        dev("upgraded-market-place", "Upgraded Market Place", 1, true, DrawEffect(2, 2, 0, 0), "Draw 2 cards."),
        dev("grizzled-warriors", "Grizzled Warriors", 0, false, MoveEffect(2, bonus = 2), "Move 2, +2 combat points."),
        dev("upgraded-trading-post", "Upgraded Trading Post", 0, false, DrawEffect(3, 2, 1, 0), "Draw 3 cards; keep 2 and discard 1 of them."),
        dev("ancestral-curse", "Ancestral Curse", 2, false, CurseEffect, "All your opponents discard 1 card of their choice. Draw 1 card."),
        dev("rangers", "Rangers", 1, false, ExploreEffect(times = 2), "Explore. After the first Explore action, you may do a second Explore action if possible."),
        dev("upgraded-scout-camp", "Upgraded Scout Camp", 0, false, ExploreEffect(draw = 2), "Explore. Draw 2 tiles; keep 1 to explore, and put the other on the bottom of the pile."),
        dev("greater-trade-routes", "Greater Trade Routes", 0, false, DrawEffect(3, 2, 0, 1), "Draw 3 cards; keep 2 and return 1 to the top of your draw pile."),
        dev("woodcutters", "Woodcutters", 1, false, RecruitPerEffect(Wood), "Recruit 1 unit per wood in each of your territories (including buildings)."),
        dev("infiltration", "Infiltration", 0, false, MoveEffect(3, special = InfiltrateMove), "Move 3. You can move through enemy territories without stopping."),
        dev("upgraded-town-hall", "Upgraded Town Hall", 1, true, RecruitEffect(2), "Recruit 2."),
        dev("amenities", "Amenities", 2, false, BuildEffect(special = AmenitiesBuild), "Build. If you build a small building (including Carved Stone), it doesn't take any space; place it anywhere in the territory."),
        dev("wood-monopoly", "Wood Monopoly", 2, true, CollectEffect(Wood, 3), "Collect 3 wood."),
        dev("bribery", "Bribery", 3, true, BriberyEffect, "Move up to 2 enemy units from a single territory to an adjacent territory (ignoring Rough borders). If it triggers a combat, resolve it."),
        dev("incursion", "Incursion", 0, false, MoveEffect(4), "Move 4."),
        dev("heroic-charge", "Heroic Charge", 0, false, MoveEffect(3, bonus = 1), "Move 3, +1 combat point."),
        dev("trading-post", "Trading Post", 1, false, DrawEffect(3, 1, 1, 1), "Draw 3 cards; keep 1, discard 1, and return 1 to the top of your draw pile."),
        dev("legendary-heroes", "Legendary Heroes", 2, true, HeroesEffect, "Resolve the effect of a card you played this year."),
        dev("allies-from-the-wild", "Allies from the Wild", 1, false, RecruitEffect(3, RecruitNeutralOnly), "Recruit 3. The units must be placed in neutral territories only."),
        dev("loremasters", "Loremasters", 1, false, RecruitPerEffect(Lore), "Recruit 1 unit per lore in each of your territories (including buildings)."),
        dev("warriors", "Warriors", 0, false, MoveEffect(2, bonus = 1), "Move 2, +1 combat point."),
        dev("cunning-merchant", "Cunning Merchant", 1, false, DrawEffect(3, 1, 0, 2), "Draw 3 cards; keep 1 and return 2 to the top of your draw pile."),
        dev("capture", "Capture", 3, true, CaptureEffect, "Remove 1 enemy unit from a territory adjacent to yours. Add 1 unit to any one of your territories."),
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
