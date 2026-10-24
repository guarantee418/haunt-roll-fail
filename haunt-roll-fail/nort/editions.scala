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

import hrf.meta._
import hrf.options._
import hrf.elem._

import nort.elem._


// CARD VERSIONS: a card as another edition (or fan content) prints it, used in the games with that version's option.
// Each version names the card it replaces, the option that turns it on, and its own name, image, effect and text; Card.info
// gives the version in play (CardVersions.apply). Asked for by Robotos on 2026-10-22, so that both editions can be played:
// a later erratum or a fan-made card is one more entry here and, if it needs one, one more option
case class CardVersion(card : Card, option : GameOption, info : CardInfo) {
    def edition : String = option match {
        case o : EditionChoice => o.short
        case PrintedNewBlood => "Printed card"
        case _ => "Fan version"
    }
}

object CardVersions {
    val secondEdition : $[CardVersion] = $(
        CardVersion(ClanCard(Stag, 0), SecondEdition, CardInfo("Stag Clan", "card-clan-stag-2e", 0, true, StagMoveEffect,
            "Recruit 1. After this Recruit action, you can do a Move 1 action with only one of your units (ignoring Rough borders).")),
        CardVersion(ClanCard(Bear, 2), SecondEdition, CardInfo("Protector of the Land", "card-clan-bear-protector-of-the-land-2e", 0, false, ProtectorMaxEffect(3),
            "Draw 1 card for each of your closed territories of at least 3 tiles in size (maximum 3 cards).")),
        CardVersion(ClanCard(Snake, 1), SecondEdition, CardInfo("Rapacious Exploitation", "card-clan-snake-rapacious-exploitation-2e", 0, false, RapaciousDrawEffect,
            "Look at an opponent's hand and choose 1 card from it. They choose either to discard it or give you 2 resources of their choice (if they don't have enough resources, they must discard). Then you may draw 1 card.")),
    )

    // New Blood as printed (Gman, 2026-10-23), in place of the TTS mod's prototype cards: the clan cards in the same slots
    // (images cut from the cards Gman sent)
    val printedNewBlood : $[CardVersion] = $(
        CardVersion(ClanCard(Dragon, 1), PrintedNewBlood, CardInfo("Enslavement", "card-clan-dragon-enslavement", 0, true, EnslavementEffect,
            "Move 1 or 2 enemy units to available spots on the Sacrificial Pyre from different territories adjacent to your territories. (If the pyre was already full, beforehand you may Sacrifice 1 unit to collect 1 food.)")),
        CardVersion(ClanCard(Dragon, 2), PrintedNewBlood, CardInfo("Reluctant Forces", "card-clan-dragon-reluctant-forces", 0, false, ReluctantForcesEffect,
            "Draw 3 cards. Sacrifice 1 unit for each card you want to keep in hand. Then, return the other cards on the top of either the draw or discard pile.")),
        CardVersion(ClanCard(Dragon, 3), PrintedNewBlood, CardInfo("Surtr's Wrath", "card-clan-dragon-surtrs-wrath", 0, false, MoveEffect(3, special = SurtrMove),
            "Move 3. Before each combat triggered by this card, you may Sacrifice units to gain 1 combat point per unit sacrificed.")),
        CardVersion(ClanCard(Horse, 0), PrintedNewBlood, CardInfo("Horse Clan", "card-clan-horse-print", 0, true, BuildEffect(special = HorseBuild),
            "Build. Small buildings do not require a building space to be placed on a territory (includes Carved Stones).")),
        CardVersion(ClanCard(Horse, 2), PrintedNewBlood, CardInfo("Quality of Life", "card-clan-horse-quality-of-life-print", 0, true, QualityEffect,
            "You may build 1 Defense Tower for free. It does not require a building space. Then collect a resource from each territory you control that has a Defense Tower.")),
        CardVersion(ClanCard(Horse, 3), PrintedNewBlood, CardInfo("Eitria and Brok's Precision", "card-clan-horse-eitria-and-broks-precision-print", 0, false, MoveEffect(2, special = PrecisionMove),
            "Move 2. Before these Move actions, you can discard any 1 resource to gain 1 combat point in combats triggered by this card, and add Move 1 to this card.")),
        CardVersion(ClanCard(Kraken, 1), PrintedNewBlood, CardInfo("Endless Tide", "card-clan-kraken-endless-tide-print", 0, true, EndlessTidePrintEffect,
            "Choose to either: Recruit 1 unit in a neutral open territory, or replace 1 enemy unit in an enemy's open territory with 1 of your own from your reserve. If this triggers a combat, resolve it.")),
        CardVersion(ClanCard(Kraken, 3), PrintedNewBlood, CardInfo("Kara's Devotion", "card-clan-kraken-karas-devotion", 0, false, MoveEffect(2, special = HowlMove),
            "Move 2. For each combat triggered by this card and won, add to the territory 1 unit per casualty you inflicted. And for each combat lost, add 1 unit on a territory you retreated to.")),
        CardVersion(ClanCard(Lynx, 0), PrintedNewBlood, CardInfo("Lynx Clan", "card-clan-lynx-print", 0, true, ExploreEffect(redraw = true),
            "Explore. You may forgo your bonus Move (Brundr and Kaelinn) to place the first map tile you draw at the bottom of the pile and draw a new one. You must place the second tile.")),
        CardVersion(ClanCard(Lynx, 1), PrintedNewBlood, CardInfo("Nimble Paws", "card-clan-lynx-nimble-paws", 0, true, NimblePawsEffect,
            "Each Lynx may Move 1 enemy unit to its territory from an adjacent territory, ignoring rough borders. Immediately resolve any combats triggered.")),
        CardVersion(ClanCard(Lynx, 2), PrintedNewBlood, CardInfo("Caring Siblings", "card-clan-lynx-caring-siblings", 0, false, RecruitEffect(2),
            "Recruit 2. During the bonus Move (Brundr and Kaelinn), you may Move the newly recruited units that are with the lynx.")),
        CardVersion(ClanCard(Lynx, 3), PrintedNewBlood, CardInfo("Mielikki's Wisdom", "card-clan-lynx-mielikkis-wisdom", 0, false, MoveEffect(3, special = MielikkiMove),
            "Move 3. If at least 1 casualty was rolled during a combat triggered by this card, collect 1 lore (only once).")),
        CardVersion(ClanCard(Ox, 0), PrintedNewBlood, CardInfo("Ox Clan", "card-clan-ox-print", 0, true, MoveEffect(1),
            "Move 1. You may take an Ancestral Equipment token after playing this card, instead of before.")),
        CardVersion(ClanCard(Ox, 1), PrintedNewBlood, CardInfo("Ancient Battlegrounds", "card-clan-ox-ancient-battlegrounds", 0, false, BattlegroundsEffect,
            "Explore. After the Explore action, if the newly placed tile has a lore anywhere on it, you may collect 1 lore. If you didn't collect 1 lore, you can immediately do another Explore action.")),
        CardVersion(ClanCard(Ox, 2), PrintedNewBlood, CardInfo("City Builder", "card-clan-ox-city-builder-print", 0, false, CityBuilderPrintEffect,
            "Build. If you build a small building, you may draw 1 card. If you build a large building, you collect 1 lore.")),
        CardVersion(ClanCard(Ox, 3), PrintedNewBlood, CardInfo("Torfin's Ferocity", "card-clan-ox-torfins-ferocity", 0, false, MoveEffect(2, special = TrueHeroMove),
            "Move 2. Before starting a combat involving Torfin, you can choose NOT to use his power and instead remove a building in the combat territory.")),
        CardVersion(ClanCard(Rat, 1), PrintedNewBlood, CardInfo("Overwork", "card-clan-rat-overwork-print", 0, true, OverworkEffect,
            "You may remove 1 of your units anywhere to collect all resources from any 1 territory you control, including from buildings. Then, you may draw 1 card.")),
        CardVersion(ClanCard(Squirrel, 1), PrintedNewBlood, CardInfo("Cooking Mastery", "card-clan-squirrel-cooking-mastery-print", 0, false, CookingPrintEffect,
            "Build. After this Build action, you may also build one Food Silo for free, in any territory you control. It does not require a building space.")),
        CardVersion(ClanCard(Squirrel, 2), PrintedNewBlood, CardInfo("Economics", "card-clan-squirrel-economics-print", 0, false, EconomicsPrintEffect,
            "Draw 1 card. You may pay 1 food to draw a second card. You may pay 2 more food to draw a third and final card.")),
    )

    // Robotos's fan-made Dragon (DragonVariant, 2026-10-24): in place of both the TTS and the printed Dragon cards, so it comes first
    // (the warchief upgrade card stays)
    val dragonVariant : $[CardVersion] = $(
        CardVersion(ClanCard(Dragon, 0), ModuleOption(DragonVariant), CardInfo("Dragon Clan", "card-clan-dragon-variant", 0, true, DragonVariantClanEffect,
            "Recruit 1. If the Sacrificial Pyre is full, you may empty it to collect 1 food.")),
        CardVersion(ClanCard(Dragon, 1), ModuleOption(DragonVariant), CardInfo("Enthrall", "card-clan-dragon-enthrall", 0, true, EnthrallEffect,
            "If the Sacrificial Pyre is full, empty it to gain 2 fame. Then take an enemy unit from a territory adjacent to one of your own and put it on the Sacrificial Pyre.")),
        CardVersion(ClanCard(Dragon, 2), ModuleOption(DragonVariant), CardInfo("Reluctant Workforce", "card-clan-dragon-reluctant-workforce-variant", 0, false, WorkforceVariantEffect,
            "Draw 2 cards. If the Sacrificial Pyre is full, you may empty it to draw a 3rd card.")),
    )

    val all : $[CardVersion] = dragonVariant ++ secondEdition ++ printedNewBlood

    // Fan-made versions are shown in the clan info only in the games that play them
    def fan(v : CardVersion) : Boolean = v.option match {
        case ModuleOption(m) => m.fan
        case _ => false
    }

    // The options of the game being played, set by the Game (one game runs on a page; the JVM checks run one at a time);
    // before a game, as in the clan picker, the cards new games start with
    var options : $[GameOption] = $(EditionChoice.default, PrintedNewBlood)

    def apply(c : Card, printed : CardInfo) : CardInfo = all.find(v => v.card == c && options.has(v.option))./(_.info).|(printed)

    // The card in a given edition, whatever game is on (the Tutorial's lessons, shown from the menu)
    def in(c : Card, o : GameOption) : CardInfo = all.find(v => v.card == c && v.option == o)./(_.info).|(c.printed)

    // The card's other versions, for the clan info: the first edition's text in a second edition game and the other way round
    def others(c : Card) : $[(String, CardInfo)] = {
        val l = all.%(_.card == c).%(v => fan(v).not || options.has(v.option))
        if (l.none)
            $
        else {
            val current = all.find(v => v.card == c && options.has(v.option))
            // New Blood's cards before the print are the TTS mod's prototype ones, not a first edition
            // and a card only a fan version changes is the original
            val base = l.exists(_.option == PrintedNewBlood).?("Earlier version").|(l.forall(fan).?("Original").|(FirstEdition.short))
            (current.any.$(base -> c.printed) ++ l.%(v => current.contains(v).not)./(v => v.edition -> v.info))
        }
    }
}

// The card edition, on the setup screen. Games made before the choice have neither option and play the first edition
trait EditionChoice extends GameOption with OneOfGroup {
    val group = "Card edition".txt
    def short : String
}

case object FirstEdition extends EditionChoice {
    def valueOn = "1st edition".txt
    val short = "1st edition"
    override val explain = $(
        "The cards as first printed.",
    )
}

case object SecondEdition extends EditionChoice {
    def valueOn = "2nd edition".txt
    val short = "2nd edition"
    override val explain = $(
        "The updated cards of the second edition:",
        "Stag Clan: after the Recruit, one unit may make a Move 1 (ignoring Rough borders), from anywhere and to anywhere.",
        "Protector of the Land: draws at most 3 cards.",
        "Rapacious Exploitation: then you may draw 1 card.",
    )
}

object EditionChoice {
    val all : $[EditionChoice] = $(FirstEdition, SecondEdition)
    // New games, the Quick Game and the Tutorial
    val default : EditionChoice = SecondEdition
}
