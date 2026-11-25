package tactics
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

// The Easy bot: rules of thumb and chance. It moves toward the Control Points it doesn't hold, sets up its weapon teams,
// shoots what it can hurt most, buys units and upgrades, and unlocks the tiers.
class BotXX(f : Faction) extends EvalBot {
    def eval(actions : $[UserAction])(implicit game : Game) : Compute[$[ActionEval]] = {
        val ev = new GameEvaluation(game, f)
        actions./{ a => ActionEval(a, ev.eval(a)) }
    }
}

class GameEvaluation(val game : Game, val self : Faction) {
    def eval(a : Action) : $[Evaluation] = {
        var result : $[Evaluation] = Nil

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        import game._

        def wanted = Board.points.keys.$.%(h => control.get(h).has(self).not)

        def nearest(h : Hex) = wanted./(Board.distance(h, _)).minOr(99)

        // how far a side turns away from the nearest enemy (or the enemy's Spawn Area), in degrees
        def facingAway(h : Hex, e : Int) = {
            val goal = enemies(self)./~(units)./(where).sortBy(Board.distance(h, _)).starting.|(enemies(self)./(team).distinct./~(Board.spawn).sortBy(Board.distance(h, _)).head)
            if (goal == h) 0 else (((Board.angle(h, goal) - Board.normals(e)) % 360 + 540) % 360 - 180).abs.round.toInt
        }

        a.unwrap match {
            case StartPlaceAction(_, _, h, _) => true |=> (100 - nearest(h) * 5) -> "near points"
            case StartPickAction(_, t, _) => (t.weaponTeam.not) |=> 50 -> "rifles"
            case MoveAction(_, u, p, _) =>
                val from = nearest(where(u))
                val to = nearest(p.last)
                (Board.points.contains(p.last) && control.get(p.last).has(self).not) |=> 700 -> "take point"
                (to < from) |=> (300 + (p.num == 1).??(50)) -> "closer"
                (to > from) |=> -200 -> "away"
                (Board.points.contains(where(u)) && control.get(where(u)).has(self).not) |=> -500 -> "stay on point"
                (u.kind == Infantry && Board.terrain(p.last) == Building) |=> 120 -> "building"
            case SetUpAction(_, u, _) => (nearest(where(u)) <= 4) |=> 400 -> "set up"
            case PackUpAction(_, u, _) => true |=> -100 -> "stay set up"
            case SandbagsAction(_, u, _, _) => (Board.points.contains(where(u))) |=> 150 -> "dig in"
            case DefensesAction(_, u, w, t, _) => (Board.points.contains(where(u)) && enemies(self)./~(units).exists(e => Board.distance(where(e), where(u)) <= 3)) |=> (90 + (w && t).??(30)) -> "wire and traps"
            case EmbarkAction(_, _, _, _, _) => true |=> -50 -> "walk"
            case DisembarkAction(_, _, h, _) => true |=> 100 -> "out"
            case EndTurnAction(_, _) => true |=> 0 -> "end"
            case TargetAction(_, u, t, d, _, _) =>
                val hurt = d.count(x => Matrix.resist(t.kind, x.dmg).has(false)) * 2 + d.count(x => Matrix.resist(t.kind, x.dmg).has(true))
                true |=> (500 + hurt * 100 - health(t) * 20) -> "fire"
            case HoldFireAction(_, _, _, _) => true |=> -100 -> "hold"
            case SpawnAction(_, t, h) =>
                val n = units(self).num
                (n < 9) |=> (600 + t.tier * 120 + t.vehicle.??(60) - (t.weaponTeam && units(self).count(_.utype.weaponTeam) >= 2).??(500) - nearest(h) * 3) -> "spawn"
                // OKW: a truck to convert into the next Tier Building
                (t == SWSTruck && self.tier < 3 && units(self).exists(_.utype == SWSTruck).not && n >= 3) |=> 400 -> "truck"
                (t == SWSTruck && units(self).count(_.utype == SWSTruck) >= 1) |=> -400 -> "one truck"
                // British: Infantry Sections to build the Emplacements
                (t == InfantrySection && units(self).count(_.utype == InfantrySection) < 2) |=> 400 -> "builders"
            case UpgradeAction(_, u, i, e) => true |=> (250 + u.utype.tier * 50) -> "upgrade"

            // Full Game Rules
            case OpPickAction(_, c, upper) => true |=> (100 + c.turns(upper) * 20) -> "turns"
            case SpawnFaceAction(_, u, e) => true |=> (200 - facingAway(where(u), e) * 3) -> "face the enemy"
            case SetUpFacingAction(_, u, e, _) =>
                (nearest(where(u)) <= 4) |=> 400 -> "set up"
                true |=> -(facingAway(where(u), e) * 3) -> "face the enemy"
            case HeavyMoveAction(_, u, p, e, _) =>
                val from = nearest(where(u))
                val to = nearest(p.last)
                (to < from) |=> 250 -> "closer"
                (to > from) |=> -150 -> "away"
                true |=> -(facingAway(p.last, e) * 3) -> "face the enemy"
                (Board.points.contains(where(u)) && control.get(where(u)).has(self).not) |=> -500 -> "stay on point"
            case AssaultAction(_, u, h, _, _) =>
                val d = occupant(h)
                (d.exists(x => health(x) <= 2 && x.kind == Infantry)) |=> 350 -> "assault the weak"
                (Board.points.contains(h)) |=> 200 -> "assault a point"
                true |=> -100 -> "risky"
            case RetreatAction(_, u, _, _, _) =>
                (health(u) <= 1) |=> 300 -> "save it"
                (health(u) > 1) |=> -400 -> "stay"
            case FinalRetreatAction(_, u, _, e, _) =>
                (retreating.contains(u)) |=> (500 - Board.distance(e, where(u))) -> "go on"
                (retreating.contains(u).not && health(u) <= 1) |=> 300 -> "save it"
                (retreating.contains(u).not) |=> -400 -> "stay"
            case RetreatDoneAction(_, _) => true |=> 0 -> "done"
            case SplashAction(_, u, h, _, _, _) =>
                val l = splashed(u, h)
                true |=> (500 + l.%(_.faction != self).num * 150 - l.%(_.faction == self).num * 250) -> "splash"
            case ShotAction(_, _, _, _, _, _) => true |=> -150 -> "an empty building"

            // Commanders
            case CommanderTierAction(_, c, i) => true |=> (700 + (math.random() * 100).toInt) -> "commander"
            case BrigadePickAction(_, b) => true |=> (650 + (math.random() * 100).toInt) -> "lost brigade"
            case BrigadeStartAction(_, b, _) => true |=> (300 + b.starting * 50) -> "lost brigade"
            case BrigadeStartSkipAction(_, _) => true |=> 200 -> "later"
            case TradeXPAction(_, r) =>
                (self.stock(XP) >= 6) |=> 200 -> "trade"
                (self.stock(XP) < 6) |=> -300 -> "keep xp"
            case FreeMajorAction(_, _) => true |=> 900 -> "free major"
            case BreakthroughAction(_) => (self.stock(Manpower) >= 3) |=> 300 -> "breakthrough"
            case DropMenuAction(_, _) => true |=> 400 -> "drop"
            case DropAction(_, _, t, h) => true |=> (500 - nearest(h) * 20) -> "drop near points"
            case BuildMenuAction(_, _) => true |=> 350 -> "build"
            case BuildAction(_, _, _, h) => true |=> (400 - nearest(h) * 20 + Board.points.contains(h).??(200)) -> "build on points"
            case FreeSpawnAction(_, _, h) => true |=> (500 - nearest(h) * 10) -> "free"
            case DefensiveStanceAction(_, u, _) => (Board.points.contains(where(u)) && enemies(self)./~(units).exists(e => Board.distance(where(e), where(u)) <= 3)) |=> 250 -> "hold the point"
            case HullDownAction(_, _, _, _) => true |=> 100 -> "hull down"
            case RaidPointAction(_, u, _) => (nearest(where(u)) > 1) |=> 150 -> "raid"
            case OpeningAction(_, _, _) => true |=> 300 -> "advance"
            case OpeningHexAction(_, a, h, _) =>
                val n = game.area(h)./~(standing).count(_.faction != self)
                (a == ReconSweep) |=> (100 + n * 100) -> "recon"
                (a != ReconSweep) |=> (game.standing(h).count(_.faction != self) * 300 - game.standing(h).count(_.faction == self) * 400 - 100) -> "barrage"
            case OpeningLineStartAction(_, _, h, _) => true |=> (Board.neighbors(h).%(n => standing(n).exists(_.faction != self)).num * 100 + standing(h).count(_.faction != self) * 200 - 150) -> "strike"
            case OpeningLineAction(_, _, l, _) => true |=> (l./(h => standing(h).count(_.faction != self) * 300 - standing(h).count(_.faction == self) * 400).sum) -> "strike"
            case SmokeAction(_, v, _) =>
                (health(v) * 2 < maxHealth(v)) |=> 200 -> "smoke"
                (health(v) * 2 >= maxHealth(v)) |=> -100 -> "no smoke"
            case SmokeDoneAction(_, _) => true |=> 0 -> "done"
            case UnlockAction(_, t) => (units(self).num >= 4) |=> 800 -> "unlock"

            // British, Soviet and OKW
            case ConvertAction(_, t, v) => (units(self).num >= 4) |=> (850 - nearest(where(v)) * 10) -> "tier building"
            case EmplaceMenuAction(_, t) => true |=> (units(self).count(_.utype.builtBy.any) < 3).?(700).|(350) -> "emplace"
            case StrafeAction(_, _, t, _, _, _) => true |=> (400 - health(t) * 20) -> "strafe"
            case NoStrafeAction(_, _, _, _) => true |=> 0 -> "no strafe"
            case ReconModeAction(_, u, _) => (nearest(where(u)) > 1) |=> 150 -> "recon mode"
            case WarSpeedAction(_, u, _) => (nearest(where(u)) > 1 && health(u) * 2 > maxHealth(u)) |=> 120 -> "war speed"
            case SalvageAction(_, r) =>
                (self.stock(Manpower) >= 6 && self.stock(r) < 2) |=> 300 -> "salvage"
                true |=> -50 -> "keep manpower"
            case BraceAction(_, u, _) => true |=> 200 -> "brace"
            case BraceDoneAction(_, _) => true |=> 0 -> "done"
            case NoFireModeAction(_, u, _) =>
                val onPoint = Board.points.contains(where(u)) && control.get(where(u)).has(self).not
                (onPoint && noFire.contains(u).not) |=> 600 -> "capture"
                (noFire.contains(u) && onPoint.not) |=> 500 -> "fire again"
                (noFire.contains(u) && onPoint) |=> -300 -> "keep capturing"
                (noFire.contains(u).not && onPoint.not) |=> -300 -> "keep firing"
            case PassAction(_) => true |=> 100 -> "pass"
            case _ =>
        }

        result.none |=> 0 -> "none"

        true |=> -((1 + math.random() * 90).round.toInt) -> "random"

        result.sortBy(v => -v.weight.abs)
    }
}
