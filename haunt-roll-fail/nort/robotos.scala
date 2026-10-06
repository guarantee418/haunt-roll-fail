package nort
//
//
//
//
import hrf.colmat._
import hrf.compute._
import hrf.logger._
//
//
//
//

import hrf.elem._

import nort.elem._

// "Robotos": the Hard bot (bot-hard.scala), cheating. A clan set to "Bot / Robotos" gets RobotosOption (options.scala)
// when the game starts (Meta.botOptions), and the game then plays by other rules for it:
// - no Winter costs (EventsExpansion.winterCost);
// - creatures ignore it: they never move into a territory with its units (CreaturesExpansion.destinations), and what
//   they do when they appear or move (a Draugr's casualty, a Fallen Valkyrie's attack, ...) doesn't touch it (below);
//   Where it has units, a creature blocks nothing either: no Brown Bear lock, no Wolf or Kobold harvest loss, ... (game.robotosIn);
// - one more unit with every Recruit, on top of Training Camps and the rest (TrainingCampsAction in map.scala);
// - one more card each year (DrawCardsAction at the start of the year in game.scala);
// - attacking, it wins ties, against clans and creatures (map.scala, creatures.scala);
// - a head start: 2 more food and 2 more wood, and 5 units in each setup placement instead of 3 (MapExpansion.robotosSetup);
// - 25 units instead of 14 (game.unitLimit);
// - clan upgrades cost 2 lore instead of 3 (game.upgradeCost).
// "Units" as in the rules: warriors, the warchief and Kaija.
// Its player panel says "(Robotos)"; tapping that lists the cheats (RobotosInfo in ui.scala).
// The Hard bot's valuation knows these rules for any clan that has them (game.robotos), so it plays them out,
// and opponents played by the Hard bot judge a Robotos clan rightly.

class BotRobotos(f : Faction) extends BotHard(f)

object RobotosExpansion extends Expansion {
    // Shown on the player panel (ui.scala)
    val cheats = $(
        "Pays no Winter costs.",
        "Creatures ignore it: they never move into its territories, never harm it, and block nothing where it has units.",
        "Recruits one more unit every time.",
        "Draws one more card each year.",
        "Wins ties when it attacks.",
        "Starts with 2 more food and 2 more wood, and places 5 units each time at setup instead of 3.",
        "Has 25 units instead of 14.",
        "Clan upgrades cost it 2 lore instead of 3.",
    )

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        // A creature appearing or moving where a Robotos clan has units does nothing to it
        case CreatureEffectAction(c, then) if game.creatureAt.contains(c) && game.present(game.board.territory(game.creatureAt(c))).exists(game.robotos) =>
            val t = game.board.territory(game.creatureAt(c))
            game.present(t).%(game.robotos).foreach(f => log(c, "ignores", f, "(Robotos)".hl))
            game.note("robotos-ignored")
            Then(then)

        case _ => UnknownContinue
    }
}
