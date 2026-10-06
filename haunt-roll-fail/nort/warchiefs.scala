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


// Warchiefs module (Warchiefs expansion rules): each clan has a warchief miniature that follows all unit rules
// (recruited, moved, fights), is worth 2 combat points (3 with some powers), counts as one unit for food and casualties,
// can't be targeted by card effects on enemy units, and goes back to the reserve when it dies

object Warchief {
    // The clans whose warchief has a portrait (the core ones, from the Tabletop Simulator standees, nort/tools/warchief-portraits.py)
    val portraits : $[Faction] = $(Bear, Boar, Goat, Raven, Snake, Stag, Wolf)

    // The New Blood clans' warchiefs are round tokens with their head from their clan card (nort/tools/warchief-heads.py)
    val tokens : $[Faction] = $(Dragon, Horse, Kraken, Lynx, Ox, Rat, Squirrel)

    def round(f : Faction) = tokens.has(f)

    // The image of f's warchief on the map, outlined in its player's color; the Automa's Leader 1 is its white miniature
    // (nort/tools/automa-leaders.py), Leader 2 (the companion) the black one
    def figure(f : Faction)(implicit game : Game) : String = (f == Automa).?("leader-1-").|((portraits ++ tokens).has(f).?("chief-" + f.style + "-").|("warchief-")) + game.colors(f).id

    def leader2(implicit game : Game) : String = "leader-2-" + game.colors(Automa).id

    // A stack of f's warriors is drawn with one of the two warrior figures (unit- and the old warchief figure, warchief-),
    // picked by a hash of where it is and how many there are, so it changes at random when units arrive or leave
    // but is the same on every redraw and for every player
    def warrior(f : Faction, where : String, n : Int)(implicit game : Game) : String =
        (((where + "/" + f + "/" + n).hashCode & 1) == 0).?("unit-").|("warchief-") + game.colors(f).id

    // Horse Clan's second warchief, Brok, a round token like Eitria's
    def brok(implicit game : Game) : String = "chief-brok-" + game.colors(Horse).id

    def name(f : Faction) : String = f match {
        case Bear => "Borgild"
        case Boar => "Svarn"
        case Goat => "Halvard"
        case Raven => "Liv"
        case Snake => "Signy"
        case Stag => "Brand"
        case Wolf => "Egil"
        case Dragon => "Surtr"
        case Horse => "Eitria"
        case Kraken => "Kàra"
        case Lynx => "Mielikki"
        case Ox => "Torfin"
        case Rat => "Eir"
        case Squirrel => "Andhrimnir"
        case _ => "Leader 1"
    }

    def power(f : Faction) : String = f match {
        case Bear => "If Borgild is the defender, she is worth 3 combat points instead of 2."
        case Boar => "If Svarn is in an open territory or a territory with wood (icons on buildings do not count), he is worth 3 combat points instead of 2."
        case Goat => "If Halvard is the defender, ignore 1 casualty inflicted by the attacker."
        case Raven => "During step 4 of combat, you may reroll the combat die once and you must accept the new result. If Liv is the attacker, this reroll must be made before the defender's roll."
        case Snake => "During step 1 of combat, you may place the Scorched Earth token in Signy's territory."
        case Stag => "During step 1 of combat, you may move 1 friendly unit from an adjacent territory into Brand's territory (ignoring Rough borders)."
        case Wolf => "If Egil is the attacker, he is worth 3 combat points instead of 2."
        case Dragon => "During step 4 of a combat involving Surtr: if you didn't roll any casualty, gain 1 casualty; if you didn't roll any point, gain 1 point."
        case Horse => "The clan has two warchiefs, Eitria and Brok. If both are in the same territory, they are worth 3 combat points instead of 4."
        case Kraken => "As defender, before the combat starts, Kàra may move a High Tide token to her territory."
        case Lynx => "If you have at least one Flash card in your active area this year, Mielikki gains 1 more combat point."
        case Ox => "When Torfin fights, he can use one additional Ancestral Equipment token that was not already used this year."
        case Rat => "Eir gains 1 combat point when fighting in a territory with at least 1 food in it, excluding Food Silos."
        case Squirrel => "Before step 1 of a defensive combat involving Andhrimnir, you gain 1 food."
        case _ => "The Automa's Leaders count as units; with the Warchiefs module they are worth 3 combat points."
    }

    def elem(f : Faction)(implicit game : Game) : Elem = name(f).styled(game.colors.get(f)./(c => c : Styling).|(f))(xstyles.bold)

    // The clan board with the warchief's portrait and power
    def board(f : Faction) = "board-" + f.style

    // The board image file: Ox Clan's is the corrected print
    def boardFile(f : Faction) = (f == Ox).?("ox-2").|(f.style)

    // Combat points of the warchief in a fight in t: 2, or 3 with Borgild defending, Egil attacking or Svarn on open or wooded land
    def strength(t : Territory, f : Faction, attacking : Boolean)(implicit game : Game) : Int =
        if (game.chiefIn(t, f).not)
            0
        else
        if (f == Automa)
            AutomaExpansion.leaderStrength
        else
        if (f == Bear && attacking.not || f == Wolf && attacking || f == Boar && (game.board.open(t) || t.areas.exists(a => game.board.spec(a).wood > 0)))
            3
        else
        // Mielikki with a Flash card in the active area, Eir where the tiles show food
        if (f == Lynx && game.states(f).active.exists(_.flash) || f == Rat && t.areas.exists(a => game.board.spec(a).food > 0))
            3
        else
            2

    // Halvard defending ignores 1 casualty inflicted by the attacker
    def shield(t : Territory, f : Faction, attacking : Boolean)(implicit game : Game) : Int =
        (f == Goat && attacking.not && game.chiefIn(t, f)).??(1)

    // Liv may reroll the combat die once
    def reroll(t : Territory, f : Faction)(implicit game : Game) : Boolean = f == Raven && game.chiefIn(t, f)
}

case class WarchiefElem(f : Faction) extends GameElementary {
    def elem(implicit game : Game) = Warchief.elem(f)
}


// STEP 1 OF COMBAT: Signy and Brand, the attacker's first
case class ChiefStepOneAction(l : $[Faction], area : AreaRef, then : ForcedAction) extends ForcedAction
case class SignyAction(self : Faction, area : AreaRef, l : $[Faction], then : ForcedAction) extends BaseAction(WarchiefElem(Snake), "in", area)("Place the", "Scorched Earth".hl, "token here")
case class SignySkipAction(self : Faction, area : AreaRef, l : $[Faction], then : ForcedAction) extends BaseAction(WarchiefElem(Snake), "in", area)("Leave the token where it is")
case class BrandAction(self : Faction, area : AreaRef, from : AreaRef, l : $[Faction], then : ForcedAction) extends BaseAction(WarchiefElem(Stag), "in", area, "can bring 1 unit from")(from) with MapTarget { def target = from }
case class BrandSkipAction(self : Faction, area : AreaRef, l : $[Faction], then : ForcedAction) extends BaseAction(WarchiefElem(Stag), "in", area)("Bring no unit")

// STEP 4: Liv's reroll; `again` is the action that rolls again, `keep` goes on with the result
case class LivRerollAction(self : Faction, again : ForcedAction) extends BaseAction(WarchiefElem(Raven))("Reroll the combat die")
case class LivKeepAction(self : Faction, keep : ForcedAction) extends BaseAction(WarchiefElem(Raven))("Keep the result")


object WarchiefsExpansion extends Expansion {
    // Territories Brand can bring a unit from: adjacent, Stag's alone, with units (Rough borders ignored)
    def brandSources(t : Territory)(implicit game : Game) : $[Territory] =
        game.board.adjacent(t).map(_._1).%(o => game.present(o) == $(Stag) && game.count(o, Stag) > 0)

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        case ChiefStepOneAction(Nil, a, then) =>
            Then(then)

        case ChiefStepOneAction(f :: rest, a, then) =>
            val t = game.board.territory(a)

            if (game.chiefIn(t, f).not)
                Then(ChiefStepOneAction(rest, a, then))
            else
            if (f == Snake && game.scorchedIn(t).not)
                Ask(f).add(SignyAction(f, a, rest, then)).add(SignySkipAction(f, a, rest, then))
            else
            if (f == Stag && brandSources(t).any)
                Ask(f).each(brandSources(t))(o => BrandAction(f, a, o.anchor, rest, then)).add(BrandSkipAction(f, a, rest, then))
            else
                Then(ChiefStepOneAction(rest, a, then))

        case SignyAction(f, a, rest, then) =>
            game.scorched = |(a)
            game.note("signy")

            f.log("placed the", "Scorched Earth".hl, "token in", a, "with", WarchiefElem(f))

            Then(ChiefStepOneAction(rest, a, then))

        case SignySkipAction(f, a, rest, then) =>
            Then(ChiefStepOneAction(rest, a, then))

        case BrandAction(f, a, from, rest, then) =>
            game.removeUnits(game.board.territory(from), f, 1)
            game.addUnits(game.board.territory(a).anchor, f, 1)
            game.note("brand")

            f.log("brought a unit from", from, "to", a, "with", WarchiefElem(f))

            Then(ChiefStepOneAction(rest, a, then))

        case BrandSkipAction(f, a, rest, then) =>
            Then(ChiefStepOneAction(rest, a, then))

        case LivRerollAction(f, again) =>
            game.note("liv")

            f.log("rerolled the die with", WarchiefElem(f))

            Then(again)

        case LivKeepAction(f, keep) =>
            Then(keep)

        case _ => UnknownContinue
    }
}
