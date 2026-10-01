package root
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
import root.elem._

case class Squires(suit : BaseSuit) extends CardEffect {
    override val name = suit.name + " Squires"
}

case object SkyCouriers extends CardEffect {
    override val name = "Sky Couriers"
}

case object SpyNetwork extends CardEffect {
    override val name = "Spy Network"
}

case object SilverTongue extends CardEffect {
    override val name = "Silver Tongue"
}

case object ShadowCouncil extends CardEffect {
    override val name = "Shadow Council"
}

case object FeatherRufflers extends CardEffect {
    override val name = "Feather Rufflers"
}

case object BoldLeadership extends CardEffect with BattleEffect {
    override val name = "Bold Leadership"
}

case object SupplyTrain extends CardEffect {
    override val name = "Supply Train"
}

case object Tactitian extends CardEffect with BattleEffect {
    override val name = "Tactitian"
}

case object Apprentice extends CardEffect {
    override val name = "Apprentice"
}

case object Lookouts extends CardEffect with BattleEffect {
    override val name = "Lookouts"
}

case object HiddenWarrens extends CardEffect {
    override val name = "Hidden Warrens"
}

case object TheFaithful extends CardEffect with BattleEffect {
    override val name = "The Faithful"
}

case object Riversteads extends CardEffect {
    override val name = "Riversteads"
}

case object StandardBearer extends CardEffect {
    override val name = "Standard Bearer"
}

case object RaidingParty extends CardEffect {
    override val name = "Raiding Party"
}

case object MiceInABush extends CardEffect with BattleEffect {
    override val name = "Mice-in-a-Bush"
}

case object BrazenDemagogue extends CardEffect {
    override val name = "Brazen Demagogue"
}


trait FriendOfThe extends CardEffect {
    val suit : BaseSuit
}

object FriendOfThe {
    def apply(s : BaseSuit) = s @@ {
        case Fox => FriendOfTheFoxes
        case Rabbit => FriendOfTheRabbits
        case Mouse => FriendOfTheMice
    }
}

case object FriendOfTheFoxes extends FriendOfThe {
    override val name = "Friend of the Foxes"
    val suit = Fox
}

case object FriendOfTheRabbits extends FriendOfThe {
    override val name = "Friend of the Rabbits"
    val suit = Rabbit
}

case object FriendOfTheMice extends FriendOfThe {
    override val name = "Friend of the Mice"
    val suit = Mouse
}


case class ShadowCouncilMainAction(self : Faction) extends BaseAction(Birdsong, "start")(ShadowCouncil) with Soft
case class ApprenticeMainAction(self : Faction) extends BaseAction(Birdsong, "start")(Apprentice) with Soft
case class SkyCouriersMainAction(self : Faction) extends BaseAction(Birdsong, "start")(SkyCouriers) with Soft
case class HiddenWarrensMainAction(self : Faction) extends BaseAction(Birdsong, "start")(HiddenWarrens) with Soft
case class RiversteadsAction(self : Faction) extends BaseAction(Birdsong, "start")(Riversteads)


object SquiresDeckExpansion extends Expansion {
    def active(setup : $[Faction], options : $[Meta.O]) = options.has(SquiresDeck)

    override def birdsong(f : Faction)(implicit game : Game, ask : ActionCollector) {
    }

    override def daylight(f : Faction)(implicit game : Game, ask : ActionCollector) {
    }

    override def evening(f : Faction)(implicit game : Game, ask : ActionCollector) {
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {

        // ...
        case _ => UnknownContinue
    }

}
