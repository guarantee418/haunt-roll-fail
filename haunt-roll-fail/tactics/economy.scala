package tactics

// The economy and the rules a match is played with: resources, players' stock and VP, the phases of a
// turn, the unit types players deploy, and the Rules that tie them together. Data only; the reducer is in turns.scala.

case class Resources(manpower : Int = 0, munitions : Int = 0, fuel : Int = 0) {
    def +(o : Resources) = Resources(manpower + o.manpower, munitions + o.munitions, fuel + o.fuel)
    def -(o : Resources) = Resources(manpower - o.manpower, munitions - o.munitions, fuel - o.fuel)
    def *(n : Int) = Resources(manpower * n, munitions * n, fuel * n)

    def negative = manpower < 0 || munitions < 0 || fuel < 0
    def covers(cost : Resources) = !(this - cost).negative

    override def toString = List(manpower -> "manpower", munitions -> "munitions", fuel -> "fuel").filter(_._1 != 0).map { case (n, s) => n.toString + " " + s } match {
        case Nil => "nothing"
        case l => l.mkString(", ")
    }
}

object Resources {
    val none = Resources()

    def sum(l : Iterable[Resources]) = l.foldLeft(none)(_ + _)
}

case class PlayerState(resources : Resources = Resources.none, vp : Int = 0)

// A turn is one player's: Income, then Action, then End, then the next player's Income
sealed abstract class Phase(val name : String)

case object IncomePhase extends Phase("Income")   // income and VP come in on entering; units can be deployed
case object ActionPhase extends Phase("Action")   // units move and attack
case object EndPhase extends Phase("End")         // the player's suppressed units recover on leaving

object Phase {
    val all : List[Phase] = List(IncomePhase, ActionPhase, EndPhase)
}

// A kind of unit players can deploy, and what it costs
case class UnitType(name : String, cost : Resources, health : Int, movement : Int, armor : Int, kind : MoveKind = Infantry, weapon : Option[Weapon] = None, sightRange : Int = 2, camouflaged : Boolean = false) {
    require(!cost.negative, "a unit's cost can't be negative")
    require(sightRange >= 0, "sight range can't be negative")

    def make(id : String, owner : String, at : Hex) = CombatUnit(id, owner, health, movement, armor, at.q, at.r, kind, weapon = weapon, maxHealth = health, sightRange = sightRange, camouflaged = camouflaged)
}

// Sample unit types, for checks and a start on the real ones
object Roster {
    import Weapons._

    // sight: riflemen and machine gunners 3, the guns' crews and the tank (buttoned up) 2, so the mortar and the guns
    // reach further than they see and need spotters; the anti-tank gun is camouflaged; scouts see 4, enough to find it
    val riflemen = UnitType("Riflemen", Resources(manpower = 3), 10, 3, 1, Infantry, Some(rifle), sightRange = 3)
    val machineGunTeam = UnitType("Machine gun team", Resources(manpower = 3, munitions = 1), 8, 2, 1, Infantry, Some(machineGun), sightRange = 3)
    val antiTankGun = UnitType("Anti-tank gun", Resources(manpower = 3, munitions = 2), 8, 1, 2, Infantry, Some(antiTank), sightRange = 2, camouflaged = true)
    val mortarTeam = UnitType("Mortar team", Resources(manpower = 2, munitions = 2), 6, 2, 0, Infantry, Some(mortar), sightRange = 2)
    val tank = UnitType("Tank", Resources(manpower = 4, fuel = 3), 20, 4, 3, Vehicle, Some(antiTank.copy(name = "Tank gun", dice = 3, infantryDamage = 2)), sightRange = 2)
    val scouts = UnitType("Scouts", Resources(manpower = 2), 6, 4, 0, Infantry, Some(rifle.copy(name = "Carbine", range = 2, dice = 2)), sightRange = 4)

    val all = List(riflemen, machineGunTeam, antiTankGun, mortarTeam, tank, scouts)
}

// `vpTarget`: the VP that wins; `start`: each player's resources at the start; `base`: the income every
// player gets each turn on top of their sectors'; `roster`: what can be deployed.
// `orders`: the range a player's order limit is drawn from each turn (None: no limit); `killVP`: the VP for
// destroying an enemy unit; `retreatHexes`: how far a retreat goes; `reinforceCost` and `reinforceHealth`: what
// reinforcing an infantry unit in its zone costs and the health it gives back.
// `fog`: units attack only what their side has spotted (vision.scala), and the screen hides the rest.
case class Rules(vpTarget : Int = 10, start : Resources = Resources(manpower = 6), base : Resources = Resources(manpower = 2), roster : List[UnitType] = Roster.all,
        orders : Option[(Int, Int)] = None, killVP : Int = 1, retreatHexes : Int = 3, reinforceCost : Resources = Resources(manpower = 2), reinforceHealth : Int = 5,
        fog : Boolean = false) {
    require(vpTarget > 0, "the VP target must be above 0")
    require(orders.forall { case (lo, hi) => lo >= 1 && hi >= lo }, "the order range must be 1 or more, lowest first")
    require(killVP >= 0 && retreatHexes >= 1 && reinforceHealth >= 1 && !reinforceCost.negative, "kill VP, retreat hexes, reinforcement health and cost can't be negative")
    require(roster.map(_.name).distinct.size == roster.size, "two unit types with one name")

    def unitType(name : String) = roster.find(_.name == name)
}
