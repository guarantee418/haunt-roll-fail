package root

import hrf.colmat._
import hrf.elem._
import root.elem._
import hrf.options._
import hrf.meta._
import hrf.elem.Effect

case object TwilightCouncil extends WarriorFaction {
  val name = "Twilight Council"
  val short = "TCvA"
  val style = "TC"
  val priority = "S"
  val warrior = BatAAA

  def advertising = BatAAA.img(this) ~ AssemblyAAA.img(this)
  def motto = "Debate".styled(this)

  // Pieces: 20 warriors, 6 assemblies, 6 convened, 6 commune
  def pieces(options: $[Meta.O]) =
    BatAAA *** 20 ++ AssemblyAAA *** 6 ++ ConvenedAAA *** 6 ++ CommuneAAA *** 6

  override def note: Elem = HorizontalBreak ~ "Fan Faction"

  def abilities(options: $[Meta.O]) = $(Effect.text("Governors"), Effect.text("Entreating"), Effect.text("Peacekeepers"))

  // Returns true if a clearing is governed by a Twilight Council assembly
  def isGoverned(clearing: Region)(implicit game: Game): Boolean =
    clearing.pieces.exists(t => t == AssemblyAAA && t.faction == TwilightCouncil && t.state == "Governing")
}

// Expansion logic stub
object TwilightCouncilExpansion extends FactionExpansion[TwilightCouncil.type] {
  def perform(action: Action, soft: Void)(implicit game: Game) = action @@ {
    // SETUP
    case CreatePlayerAction(f: TwilightCouncil.type) =>
      game.states = game.states + (f -> new PlayerState(f))
      FactionInitAction(f)

    case FactionSetupAction(f: TwilightCouncil.type) =>
      // 1. Choose a homeland clearing
      Ask(f).each(board.clearings) { homeland =>
        // Place 4 warriors and 1 Governing assembly
        (1 to 4).foreach(_ => f.reserve --> f.warrior --> homeland)
        val assembly = AssemblyAAA.copy(state = "Governing", faction = TwilightCouncil)
        f.reserve --> assembly --> homeland

        // 2. Place 2 warriors in a different clearing
        val otherClearings = board.clearings.filter(_ != homeland)
        Ask(f).each(otherClearings) { other =>
          (1 to 2).foreach(_ => f.reserve --> f.warrior --> other)

          // 3. Fill your Assemblies track with the remaining 5 assemblies on their Closed side
          val closedAssemblies = (1 to 5).map(_ => AssemblyAAA.copy(state = "Closed", faction = TwilightCouncil))
          closedAssemblies.foreach(a => f.reserve :+= a)

          f.log("Setup complete: 4 warriors + 1 Governing assembly in homeland, 2 warriors elsewhere, 5 Closed assemblies in reserve.")
          SetupFactionsAction
        }
      }
      soft()

    // Add Birdsong, Daylight, Evening, and ability logic here
    case BirdsongNAction(0, f: TwilightCouncil.type) =>
      // Handle Birdsong actions specific to Twilight Council
      // Example: f.placeAssembly(), f.entreating(), etc.
    case DaylightNAction(0, f: TwilightCouncil.type) =>
        // Handle Daylight actions specific to Twilight Council
        // Find all assemblies ruled by enemies and flip them to Closed
        board.clearings.foreach { clearing =>
          clearing.tokens.of(AssemblyAAA).filter(_.faction == TwilightCouncil).foreach { assembly =>
            // If this clearing is ruled by an enemy (not TC)
            if (!clearing.isRuledBy(f)) {
              assembly.state = "Closed"
              f.log("Assembly at", clearing, "was flipped to Closed (Sleep).")
            }
          }
        }
        soft()
    case EveningNAction(0, f: TwilightCouncil.type) =>
      // 1st: Convene Woodfolk
      // Prompt to return revealed cards to hand, one by one
      Ask(f).each(f.revealed)(card =>
        ConveneWoodfolkAction(f, card)
      )
      soft()

    // Handle Convene Woodfolk actions
    case ConveneWoodfolkAction(f: TwilightCouncil.type, card: Card) =>
      // Prompt to act at a matching assembly (move, battle, agitate, empower, etc.)
      val matchingAssemblies = board.clearings.filter(c =>
        c.suit == card.suit && c.tokens.exists(t => t == AssemblyAAA && t.faction == TwilightCouncil)
      )
      Ask(f).each(matchingAssemblies) { clearing =>
        Ask(f)(
          "Banish (Battle)" -> BanishAction(f, clearing, card),
          "Agitate" -> AgitateAction(f, clearing, card),
          "Empower" -> EmpowerAction(f, clearing, card)
        )
      }
      // Return card to hand after acting
      f.hand :+= card
      f.revealed :-= card
      soft()

    // Banish: Battle, but hits force defending warriors to move instead, ignoring rule
    case BanishAction(f: TwilightCouncil.type, clearing: Region, card: Card) =>
      // Implement special battle logic here
      f.log("Banish: Battled at", clearing, "with", card)
      soft()

    // Agitate: Spend the card, gain 1 Loyalist, flip assembly to Governing if Closed
    case AgitateAction(f: TwilightCouncil.type, clearing: Region, card: Card) =>
      f.hand :-= card
      f.discard :+= card
      // Gain 1 Loyalist (implement as needed)
      // Flip assembly to Governing if Closed
      clearing.tokens.of(AssemblyAAA).filter(_.faction == TwilightCouncil).foreach { assembly =>
        if (assembly.state == "Closed") assembly.state = "Governing"
      }
      f.log("Agitated at", clearing, "with", card)
      soft()

    // Empower: Roll a die, remove that many Council warriors. Score if you rule, else place in Loyalists
    case EmpowerAction(f: TwilightCouncil.type, clearing: Region, card: Card) =>
      val roll = game.rollDie()
      val warriors = clearing.pieces.of(f.warrior).take(roll)
      warriors.foreach(w => clearing.pieces :-= w)
      if (clearing.isRuledBy(f)) {
        // Score points (implement as needed)
        f.log("Empowered at", clearing, "and scored", roll, "points.")
      } else {
        // Place removed warriors in Loyalists (implement as needed)
        f.log("Empowered at", clearing, "and placed", roll, "warriors in Loyalists.")
      }
      soft()

    // 2nd: Craft with assemblies, or draw 1 card for every 2 assemblies if you don't craft
    case EveningNAction(1, f: TwilightCouncil.type) =>
      // Implement crafting logic here
      // If not crafting, draw 1 card for every 2 assemblies
      val assemblies = board.clearings.flatMap(_.tokens.of(AssemblyAAA).filter(_.faction == TwilightCouncil)).size
      if (!f.crafted) {
        val drawCount = assemblies / 2
        (1 to drawCount).foreach(_ => f.draw())
        f.log("Drew", drawCount, "cards for assemblies.")
      }
      // Discard down to 5 cards
      while (f.hand.size > 5) {
        Ask(f).each(f.hand)(card => {
          f.hand :-= card
          f.discard :+= card
          f.log("Discarded", card)
        })
      }
      soft()

    // 3rd: Adjourn. Remove any number of assemblies. Flip assemblies you rule to Governing.
    case EveningNAction(2, f: TwilightCouncil.type) =>
      // Prompt to remove assemblies (implement as needed)
      board.clearings.foreach { clearing =>
        if (clearing.isRuledBy(f)) {
          clearing.tokens.of(AssemblyAAA).filter(_.faction == TwilightCouncil).foreach(_.state = "Governing")
        }
      }
      f.log("Adjourned: Flipped assemblies you rule to Governing.")
      soft()

    // 4th: Oversee governing assemblies at enemy buildings/tokens for points
    case EveningNAction(3, f: TwilightCouncil.type) =>
      val governed = board.clearings.count { clearing =>
        clearing.tokens.of(AssemblyAAA).exists(a => a.faction == TwilightCouncil && a.state == "Governing") &&
        (clearing.buildings.exists(_.faction != TwilightCouncil) || clearing.tokens.exists(t => t.faction != TwilightCouncil && t != AssemblyAAA))
      }
      val points = governed match {
        case 1 => 1
        case 2 | 3 => 2
        case 4 => 3
        case 5 | 6 => 4
        case _ => 0
      }
      if (points > 0) f.score(points)
      f.log("Oversee: Scored", points, "points for governing assemblies at enemy pieces.")
      soft()

    // 5th: Draw 1 card, discard down to 5
    case EveningNAction(4, f: TwilightCouncil.type) =>
      f.draw()
      while (f.hand.size > 5) {
        Ask(f).each(f.hand)(card => {
          f.hand :-= card
          f.discard :+= card
          f.log("Discarded", card)
        })
      }
      soft()

    // Handle other actions specific to Twilight Council
    // For example, handling specific actions like placing assemblies, entreating, etc.
    // Ensure to define how each action interacts with the Twilight Council's unique mechanics
    // Example:
    // case BirdsongNAction(0, f: TwilightCouncil.type) => ...
    // case DaylightNAction(0, f: TwilightCouncil.type) => ...
    // case EveningNAction(0, f: TwilightCouncil.type) => ...

    // Example: Prevent enemy crafting, flipping, placing, or removing pieces in governed clearings
    case CraftAction(f: Vagabond, clearing, item) if TwilightCouncil.isGoverned(clearing) =>
      f.log("The Vagabond cannot craft in a clearing governed by the Twilight Council.")
      soft()

    case PlacePieceAction(f, piece, clearing) if TwilightCouncil.isGoverned(clearing) && f != TwilightCouncil =>
      f.log("Cannot place pieces in a clearing governed by the Twilight Council.")
      soft()

    case RemovePieceAction(f, piece, clearing) if TwilightCouncil.isGoverned(clearing) && f != TwilightCouncil =>
      f.log("Cannot remove pieces in a clearing governed by the Twilight Council.")
      soft()

    // Action: An enemy entreats an assembly
    case EntreatAssemblyAction(enemy: Faction, f: TwilightCouncil.type, assembly: Token, clearing: Region) =>
      // Flip the assembly to Closed
      assembly.state = "Closed"
      f.log("Assembly at", clearing, "was entreated by", enemy, "and flipped to Closed.")

      // Offer the Twilight Council a choice
      Ask(f)(
        "Gain 1 Loyalist" -> GainLoyalistAction(f),
        "Place Loyalists at this clearing" -> PlaceLoyalistsAtAction(f, clearing)
      )
      soft()

    // Handle gaining a Loyalist
    case GainLoyalistAction(f: TwilightCouncil.type) =>
      // Add a Loyalist to supply (implement as needed)
      f.log("Gained 1 Loyalist due to Entreating.")
      // f.supply :+= LoyalistToken
      // or
      LoyalistToken --> clearing // If placing on the board
      soft()

    // Handle placing Loyalists at a clearing
    case PlaceLoyalistsAtAction(f: TwilightCouncil.type, clearing: Region) =>
      // UI should allow player to select how many Loyalists to place
      f.log("Placed Loyalists at", clearing, "due to Entreating.")
      // Implement logic to move Loyalists from supply to clearing
      soft()

    // Allow battle as normal (do not block BattleAction)
    // Battle between enemies at an assembly: add TC warriors to defender (not if Vagabond is defender)
    case ba @ BattleAction(attacker, defender, clearing, _*) =>
      if (
        clearing.tokens.exists(t => t == AssemblyAAA && t.faction == TwilightCouncil) &&
        attacker != TwilightCouncil && defender != TwilightCouncil && !defender.isInstanceOf[Vagabond]
      ) {
        val tcWarriors = clearing.pieces.count(p => p.faction == TwilightCouncil && p.isInstanceOf[Warrior])
        val defenderWarriors = clearing.pieces.count(p => p.faction == defender && p.isInstanceOf[Warrior])
        val totalDefender = defenderWarriors + tcWarriors

        defender.log(s"Peacekeepers: $tcWarriors Twilight Council warriors join your defense.")

        // Apply hits: defender's own pieces take hits first, then TC warriors
        BattleResolution(
          attacker = attacker,
          defender = defender,
          clearing = clearing,
          defenderExtra = tcWarriors,
          tcFaction = TwilightCouncil
        )
        soft()
      } else soft()

    // When an enemy removes an assembly, remove 1 Loyalist
    case RemovePieceAction(f, piece, clearing)
      if piece == AssemblyAAA && piece.faction == TwilightCouncil && f != TwilightCouncil =>
        // Remove 1 Loyalist from supply (or board, as per your Loyalist implementation)
        TwilightCouncil.log("An assembly was removed by", f, "at", clearing, "- remove 1 Loyalist.")
        // Implement logic to remove 1 Loyalist (from supply or board)
        // Example:
        // if (TwilightCouncil.supply.has(LoyalistToken)) TwilightCouncil.supply :-= LoyalistToken
        // else remove from board if needed
        soft()

    // Allow Twilight Council to freely remove Loyalists to supply
    case RemoveLoyalistsToSupplyAction(f: TwilightCouncil.type, from: Region, count: Int) =>
      // Remove 'count' Loyalist tokens from 'from' and add to supply
      val removed = from.tokens.take(count).filter(_ == LoyalistToken)
      removed.foreach(t => {
        from.tokens :-= t
        f.supply :+= LoyalistToken
      })
      f.log(s"Removed $count Loyalist(s) from $from to supply.")
      soft()

    // Prompt to reveal a card for an action
    case BirdsongNAction(n, f: TwilightCouncil.type) =>
      Ask(f)(
        "Move" -> RevealCardForAction(f, "move"),
        "Recruit" -> RevealCardForAction(f, "recruit"),
        "Battle" -> RevealCardForAction(f, "battle"),
        "Assemble" -> RevealCardForAction(f, "assemble")
      )
      soft()

    // Reveal a card for a specific action
    case RevealCardForAction(f: TwilightCouncil.type, action: String) =>
      // Prompt to select a card to reveal
      Ask(f).each(f.hand)(card => DoActionWithCard(f, card, action))
      soft()

    // Do the action in a matching clearing
    case DoActionWithCard(f: TwilightCouncil.type, card: Card, action: String) =>
      val matchingClearings = board.clearings.filter(_.suit == card.suit)
      Ask(f).each(matchingClearings) { clearing =>
        action match {
          case "move" =>
            // Move from this clearing
            f.log("Moved from", clearing, "by revealing", card)
          case "recruit" =>
            // Place 1 warrior in this clearing
            f.reserve --> f.warrior --> clearing
            f.log("Recruited in", clearing, "by revealing", card)
          case "battle" =>
            // Battle in this clearing
            // If an assembly is there, discard the revealed card
            if (clearing.tokens.exists(t => t == AssemblyAAA && t.faction == TwilightCouncil)) {
              f.hand :-= card
              f.discard :+= card
              f.log("Battled in", clearing, "and discarded", card, "because an assembly is present.")
            } else {
              f.log("Battled in", clearing, "by revealing", card)
            }
          case "assemble" =>
            // If no assembly, place a Closed assembly and any Loyalists
            if (!clearing.tokens.exists(t => t == AssemblyAAA && t.faction == TwilightCouncil)) {
              f.reserve --> AssemblyAAA.copy(state = "Closed", faction = TwilightCouncil) --> clearing
              // Optionally prompt to place Loyalists here
              f.log("Placed a Closed Assembly in", clearing, "by revealing", card)
            }
            // If you don't rule, discard the card
            if (!clearing.isRuledBy(f)) {
              f.hand :-= card
              f.discard :+= card
              f.log("Did not rule", clearing, "- discarded", card)
            }
          case _ =>
        }
      }
      soft()
  }
}

case object BatAAA extends Warrior {
  override def id = "BatAAA"
  override def name = "Bat"
}

case object AssemblyAAA extends Token {
  override def id = "AssemblyAAA"
  override def name = "Assembly"
}

case object ConvenedAAA extends Token {
  override def id = "ConvenedAAA"
  override def name = "Convened"
}

case object CommuneAAA extends Building {
  override def id = "CommuneAAA"
  override def name = "Commune"
}

case object LoyalistToken extends Token {
  override def id = "Loyalist"
  override def name = "Loyalist"
}

class PlayerState(f: Faction) extends FactionState