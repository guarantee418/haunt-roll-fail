package doms
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

class BotXX(f : Faction) extends EvalBot {
    def eval(actions : $[UserAction])(implicit game : Game) : Compute[$[ActionEval]] = {
        val ev = new GameEvaluation(f)
        actions./{ a => ActionEval(a, ev.eval(a)) }
    }
}

class GameEvaluation(val self : Faction)(implicit val game : Game) {
    def eval(a : Action) : $[Evaluation] = {
        var result : $[Evaluation] = Nil

        implicit class condToEval(val bool : Boolean) {
            def |=> (e : (Int, String)) { if (bool) result +:= Evaluation(e._1, e._2) }
        }

        def q(t : Terrain) : Int = t @@ {
            case Tundra => 1
            case Mountain => 3
            case Desert => 4
            case Forest => 5
            case Jungle => 6
            case Savannah => 7
            case Wetland => 8
            case Sea => 9
        }

        a.unwrap @@ {
            case EndAction(_) =>
                true |=> -1000 -> "dont skip"

            case PlanningAction(_, Adaptation(i), _) =>
                val p = (Adaptation(1).$ ++ Adaptation(2).$ ++ Adaptation(3).$).%(_.faction == self).num
                val n = min(6 - self.resources.num, game.adaptation.distinct.diff(game.regression).diff(self.resources).num)

                (p >= n) |=> -1000 -> "no need"
                (p < n) |=> 300 -> "gain"

                true |=> -10*i -> "earlier better"


            case AdaptationAction(_, r, _) =>
                self.resources.has(r).not |=> 1000 -> "new resource"

                game.regression.has(r) |=> -3000 -> "will be lost"

                true |=> game.resources.values.$.count(r) * 10 -> "on map"


            case PlanningAction(_, Regression(i), _) =>
                val p = Regression(1).%(_.faction == self).num + Regression(2).%(_.faction == self).num + (self == Reptile).??(1)
                val n = self.adaptation.distinct.intersect(game.regression).num

                (p >= n) |=> -1000 -> "no need"
                (p < n) |=> 1000 -> "protect"


            case PlanningAction(_, Abundance(i), _) =>
                val p = (Abundance(1).$ ++ Abundance(2).$).%(_.faction == self).num
                val n = game.abundance.%(self.resources.has).num

                (p >= n) |=> -1000 -> "no need"
                (p < n) |=> 200 -> "gain"

                true |=> -10*i -> "earlier better"


            case AbundanceAction(_, r, c, _) =>
                self.resources.has(r) |=> 1000 -> "speciate"
                self.resources.has(r).not |=> -1000 -> "hunger"

                true |=> c.tiles./(_.of(self).num).sum * 10 -> "species"


            case PlanningAction(_, Glaciation(i), _) =>
                (i > 0) |=> -1000 -> "dont"


            case PlanningAction(_, Speciation(r), _) =>
                self.reserve.none |=> -10000 -> "empty"

                self.resources.has(r) |=> 1000 -> "speciate"
                self.resources.has(r).not |=> -1000 -> "hunger"
                game.resources.values.$.has(r).not |=> -2000 -> "no resource"


            case SpeciationAction(_, x, n, _) =>
                x.corners./~(_.resource).intersect(self.resources).none |=> -1000 -> "hunger"
                true |=> n * 100 -> "speciate"


            case PlanningAction(_, Migration(i), _) =>
                val p = 2.to(7)./~(i => Migration(i).$).%(_.faction == self).num

                p > 0 |=> -1000 -> "one is enough"
                true |=> 50 -> "ok"
                true |=> 10*i -> "more better"


            case MigrateToAction(_, o, _, x, _) =>
                x.corners./~(_.resource).intersect(self.resources).none |=> -5000 -> "hunger"
                o.corners./~(_.resource).intersect(self.resources).none |=> 5000 -> "hunger"

                true |=> x.corners./~(_.resource).intersect(self.resources).num * 200 -> "eat"
                true |=> o.corners./~(_.resource).intersect(self.resources).num * -200 -> "eat"


            case WanderlustAction(_, x, t, _, _) =>
                val l = x.tiles

                true |=> l./(_.of(self).num).sum * 100 -> "own"
                true |=> l./(_.num).sum * -10 -> "competition"
                true |=> 100 * l.num * (l.num + 1) / 2 -> "terrain"

                t == Sea |=> 900 -> "terrain"
                t == Wetland |=> 800 -> "terrain"
                t == Savannah |=> 700 -> "terrain"
                t == Jungle |=> 600 -> "terrain"
                t == Forest |=> 500 -> "terrain"
                t == Desert |=> 400 -> "terrain"
                t == Mountain |=> 300 -> "terrain"
                t == Tundra |=> 100 -> "terrain"

            case WanderlustResourceAction(_, r, _, _) =>
                self.resources.has(r) |=> 1000 -> "eat"
                self.resources.has(r).not |=> -1000 -> "hunger"


            case WanderlustMoveFromAction(_, o, u, x, _) =>
                x.corners./~(_.resource).intersect(self.resources).none |=> -5000 -> "hunger"
                o.corners./~(_.resource).intersect(self.resources).none |=> 5000 -> "hunger"

                true |=> (o.of(self).num * q(x.terrain.get) - x.of(self).num * q(o.terrain.get)) * 100 -> "balance"

            case PlanningAction(_, Wanderlust(i), _) =>
                game.wanderlust.%(self.resources.has).num >= i |=> 100 -> "why not"


            case PlanningAction(_, Domination(i), _) =>
                val p = (Domination(1).$ ++ Domination(2).$ ++ Domination(3).$ ++ Domination(4).$ ++ Domination(5).$).%(_.faction == self).num
                p == 0 |=> 100 -> "try"
                true |=> -10*i -> "earlier better"

            case TakeAction(_, x, c, _) =>
                c == Aquatic       |=> 100 -> "aquatic"
                c == Biodiversity  |=> 100 -> "biodiversity"
                c == Biomass       |=> 100 -> "biomass"
                c == Blight        |=> 999 -> "blight "
                c == Catastrophe   |=> 999 -> "catastrophe"
                c == ColdSnap      |=> 800 -> "cold snap"
                c == Disease       |=> 100 -> "disease"
                c == Ecodiversity  |=> 100 -> "ecodiversity"
                c == Evolution     |=> 100 -> "evolution"
                c == Fecundity     |=> 100 -> "fecundity"
                c == Fertile       |=> 100 -> "fertile"
                c == Habitat       |=> 100 -> "habitat"
                c == Hibernation   |=> 100 -> "hibernation"
                c == IceSheet      |=> 900 -> "ice sheet"
                c == Immigrants    |=> 100 -> "immigrants"
                c == Instinct      |=> 100 -> "instinct"
                c == Intelligence  |=> 900 -> "intelligence"
                c == MassExodus    |=> 800 -> "mass exodus"
                c == Metamorphosis |=> 100 -> "metamorphosis"
                c == NicheBiomes   |=> 100 -> "niche biomes"
                c == Nocturnal     |=> 100 -> "nocturnal"
                c == Omnivore      |=> 999 -> "omnivore"
                c == Parasitism    |=> 900 -> "parasitism"
                c == Predator      |=> 100 -> "predator"
                c == Symbiotic     |=> 100 -> "symbiotic"

            case DominationAction(f, x, _) =>
                val points = x.terrain./~{
                    case Sea => $(9, 5, 3, 2)
                    case Wetland => $(8, 4, 2, 1)
                    case Savannah => $(7, 4, 2)
                    case Jungle => $(6, 3, 2)
                    case Forest => $(5, 3, 2)
                    case Desert => $(4, 2)
                    case Mountain => $(3, 2)
                    case Tundra => $(1)
                }

                val l = game.order.reverse.%(x.of(_).any)

                l.sortBy(x.of(_).num).reverse.lazyZip(points).foreach { (e, n) =>
                    (e == f) |=> n * 10000 -> "own score"
                    (e != f) |=> -n * 8000 -> "opposing score"
                }

                val own = f.claim(x)
                val enemy = l.but(f)./(_.claim(x)).maxOr(0)

                enemy > own |=> -1000000 -> "dont give enemy"
                enemy < own |=> 1000000 -> "claim"

            case ImmigrantsPawnAction(_, _) =>
                true |=> -1000 -> "just no"

            case DiseaseAction(_, r, _) =>
                true |=> self.resources.count(r) * 100 -> "duplicates"

            case _ =>
        }

        result.none |=> 0 -> "none"

        true |=> -((1 + math.random() * 7).round.toInt) -> "random"

        result.sortBy(v => -v.weight.abs)
    }
}
