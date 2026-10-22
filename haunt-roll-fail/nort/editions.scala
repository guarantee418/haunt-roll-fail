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

    val all : $[CardVersion] = secondEdition

    // The options of the game being played, set by the Game (one game runs on a page; the JVM checks run one at a time);
    // before a game, as in the clan picker, the cards new games start with
    var options : $[GameOption] = $(EditionChoice.default)

    def apply(c : Card, printed : CardInfo) : CardInfo = all.find(v => v.card == c && options.has(v.option))./(_.info).|(printed)

    // The card in a given edition, whatever game is on (the Tutorial's lessons, shown from the menu)
    def in(c : Card, o : GameOption) : CardInfo = all.find(v => v.card == c && v.option == o)./(_.info).|(c.printed)

    // The card's other versions, for the clan info: the first edition's text in a second edition game and the other way round
    def others(c : Card) : $[(String, CardInfo)] = {
        val l = all.%(_.card == c)
        if (l.none)
            $
        else {
            val current = all.find(v => v.card == c && options.has(v.option))
            (current.any.$(FirstEdition.short -> c.printed) ++ l.%(v => current.contains(v).not)./(v => v.edition -> v.info))
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
