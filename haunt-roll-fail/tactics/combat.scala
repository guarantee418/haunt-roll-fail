package tactics

// Combat: weapons, who can attack whom, and attacks resolved from dice rolls given to them.
// Pure functions of a GameState. The dice are rolled outside (by the game, recorded with the
// action) and passed in, so an attack comes out the same in every client, replay and check.
//
// Rules:
// - The attacker and target must both be alive and on the board, have different owners, and the
//   attacker must have a weapon.
// - The target must be within the weapon's range (minRange to range, in hexes) and in line of sight
//   (Board.lineOfSight: blocking terrain between them; units don't block).
// - The weapon rolls `dice` six-sided dice. Each die hits on `needed` or more:
//   the weapon's accuracy, +1 for light cover and +2 for heavy cover on the target's hex,
//   +1 if the attacker is suppressed; a 1 always misses and a 6 always hits.
// - Each hit deals the weapon's damage against the target's kind (infantry or vehicle), less the
//   target's armor beyond the weapon's penetration (armor - penetration, never below 0); never below 0.
// - Damage beyond the target's health is lost. A target at 0 health is destroyed and taken off the board.
// - Infantry hit at least once (even for 0 damage) and still standing is suppressed until newTurn.

case class Weapon(name : String, range : Int, dice : Int, accuracy : Int, infantryDamage : Int, vehicleDamage : Int, penetration : Int = 0, minRange : Int = 1) {
    require(minRange >= 1 && range >= minRange, "range must be " + minRange + " or more, got " + range)
    require(dice >= 1, "dice must be at least 1, got " + dice)
    require(accuracy >= 2 && accuracy <= 6, "accuracy must be 2 to 6, got " + accuracy)
    require(infantryDamage >= 0 && vehicleDamage >= 0 && penetration >= 0, "damage and penetration can't be negative")

    def damage(kind : MoveKind) = kind match {
        case Infantry => infantryDamage
        case Vehicle => vehicleDamage
    }

    def inRange(d : Int) = d >= minRange && d <= range
}

// Sample weapons, for checks and a start on the real ones
object Weapons {
    val rifle = Weapon("Rifle", range = 3, dice = 3, accuracy = 4, infantryDamage = 2, vehicleDamage = 1)
    val machineGun = Weapon("Machine gun", range = 4, dice = 5, accuracy = 5, infantryDamage = 2, vehicleDamage = 0)
    val antiTank = Weapon("Anti-tank gun", range = 5, dice = 2, accuracy = 4, infantryDamage = 1, vehicleDamage = 6, penetration = 3)
    val mortar = Weapon("Mortar", range = 6, dice = 2, accuracy = 5, infantryDamage = 3, vehicleDamage = 1, minRange = 2)
}

case class CombatReport(
    attacker : String,
    target : String,
    weapon : String,
    distance : Int,
    cover : Cover,
    needed : Int,          // each die hits on this or more
    rolls : List[Int],
    hits : Int,
    damagePerHit : Int,    // after armor
    damage : Int,          // health the target lost
    health : Int,          // the target's health after
    destroyed : Boolean,
    suppressed : Boolean   // the target was suppressed by this attack
)

object Combat {
    val sides = 6

    // Why the attacker can't attack the target, or the two units and the weapon if it can
    def validate(s : GameState, attackerId : String, targetId : String) : Either[String, (CombatUnit, CombatUnit, Weapon)] =
        for {
            a <- s.unit(attackerId).toRight("no unit " + attackerId)
            t <- s.unit(targetId).toRight("no unit " + targetId)
            _ <- Either.cond(a.alive, (), attackerId + " is out of action")
            _ <- Either.cond(t.alive, (), targetId + " is already out of action")
            _ <- Either.cond(a.owner != t.owner, (), attackerId + " and " + targetId + " are on the same side")
            w <- a.weapon.toRight(attackerId + " has no weapon")
            d = a.hex.distance(t.hex)
            _ <- Either.cond(w.inRange(d), (), targetId + " is " + d + " hexes away; " + w.name + " reaches " + (if (w.minRange > 1) w.minRange.toString + " to " else "") + w.range)
            _ <- Either.cond(s.board.lineOfSight(a.hex, t.hex), (), attackerId + " can't see " + targetId)
        } yield (a, t, w)

    def canAttack(s : GameState, attackerId : String, targetId : String) : Boolean = validate(s, attackerId, targetId).isRight

    // Every unit the attacker can attack now
    def targets(s : GameState, attackerId : String) : List[CombatUnit] =
        s.units.values.filter(t => canAttack(s, attackerId, t.id)).toList.sortBy(_.id)

    def coverOf(s : GameState, t : CombatUnit) : Cover = s.board.get(t.hex).map(_.cover).getOrElse(NoCover)

    // What each die needs to hit
    def needed(s : GameState, a : CombatUnit, w : Weapon, t : CombatUnit) : Int =
        (w.accuracy + coverOf(s, t).level + (if (a.suppressed) 1 else 0)).max(2).min(sides)

    // The damage each hit deals the target after its armor
    def damagePerHit(w : Weapon, t : CombatUnit) : Int = (w.damage(t.kind) - (t.armor - w.penetration).max(0)).max(0)

    def attack(s : GameState, attackerId : String, targetId : String, rolls : Seq[Int]) : Either[String, (GameState, CombatReport)] =
        validate(s, attackerId, targetId).flatMap { case (a, t, w) =>
            if (rolls.size != w.dice)
                Left(w.name + " rolls " + w.dice + " dice, got " + rolls.size)
            else
            if (rolls.exists(r => r < 1 || r > sides))
                Left("dice rolls must be 1 to " + sides + ", got " + rolls.mkString(", "))
            else {
                val need = needed(s, a, w, t)
                val hits = rolls.count(_ >= need)
                val each = damagePerHit(w, t)
                val damage = (hits * each).min(t.health)
                val health = t.health - damage
                val destroyed = health == 0
                val suppressed = !destroyed && hits > 0 && t.kind == Infantry && !t.suppressed

                val units =
                    if (destroyed)
                        s.units - t.id
                    else
                        s.units + (t.id -> t.copy(health = health, suppressed = t.suppressed || suppressed))

                Right((s.copy(units = units), CombatReport(a.id, t.id, w.name, a.hex.distance(t.hex), coverOf(s, t), need, rolls.toList, hits, each, damage, health, destroyed, suppressed)))
            }
        }
}
