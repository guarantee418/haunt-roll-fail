package root

import hrf.colmat._
import hrf.elem._
import root.elem._
import hrf.options._
import hrf.meta._

case object TwilightCouncil extends WarriorFaction {
  val name = "Twilight Council"
  val short = "TCvA"
  val style = "TC"
  val priority = "S"
  val warrior = BatAAA

  def advertising = BatAAA.img(this) ~ AssemblyAAA.img(this)
  def motto = "Debate".styled(this)

  // Pieces: adjust as needed
  def pieces(options: $[Meta.O]) =
    BatAAA *** 20 ++ AssemblyAAA *** 6 ++ ConvenedAAA *** 6 ++ CommuneAAA *** 6

  override def note: Elem = HorizontalBreak ~ "Fan Faction"

  def abilities(options: $[Meta.O]) = $("Governors", "Entreating", "Peacekeepers")

  // Returns true if a clearing is governed by a Twilight Council assembly
  def isGoverned(clearing: Region)(implicit game: Game): Boolean =
    clearing.tokens.exists(t => t == AssemblyAAA && t.faction == TwilightCouncil && t.state == "Governing")
}

// Expansion logic stub
object TwilightCouncilExpansion extends FactionExpansion[TwilightCouncil.type] {
  def perform(action: Action, soft: Void)(implicit game: Game) = action @@ {
    // SETUP
    case CreatePlayerAction(f: TwilightCouncil.type) =>
      game.states += f -> new PlayerState(f)
      FactionInitAction(f)

    case FactionSetupAction(f: TwilightCouncil.type) =>
      // Place assemblies, loyalists, etc. as per your setup
      SetupFactionsAction

    // Add Birdsong, Daylight, Evening, and ability logic here
    case BirdsongNAction(0, f: TwilightCouncil.type) =>
      // Handle Birdsong actions specific to Twilight Council
      // Example: f.placeAssembly(), f.entreating(), etc.
    case DaylightNAction(0, f: TwilightCouncil.type) =>
        // Handle Daylight actions specific to Twilight Council
    case EveningNAction(0, f: TwilightCouncil.type) =>
        // Handle Evening actions specific to Twilight Council
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
      soft()

    // Handle placing Loyalists at a clearing
    case PlaceLoyalistsAtAction(f: TwilightCouncil.type, clearing: Region) =>
      // UI should allow player to select how many Loyalists to place
      f.log("Placed Loyalists at", clearing, "due to Entreating.")
      // Implement logic to move Loyalists from supply to clearing
      soft()

    // Allow battle as normal (do not block BattleAction)
    // Battle between enemies at an assembly: add TC warriors to defender (not if Vagabond is defender)
    case BattleAction(attacker, defender, clearing, ...)
      if clearing.tokens.exists(t => t == AssemblyAAA && t.faction == TwilightCouncil) &&
         attacker != TwilightCouncil && defender != TwilightCouncil && !defender.isInstanceOf[Vagabond] =>

      val tcWarriors = clearing.pieces.count(p => p.faction == TwilightCouncil && p.isInstanceOf[Warrior])
      val defenderWarriors = clearing.pieces.count(p => p.faction == defender && p.isInstanceOf[Warrior])

      val totalDefender = defenderWarriors + tcWarriors

      defender.log(s"Peacekeepers: $tcWarriors Twilight Council warriors join your defense.")

      // Apply hits: defender's own pieces take hits first, then TC warriors
      // (You may need to adjust your battle resolution logic to support this order)
      BattleResolution(
        attacker = attacker,
        defender = defender,
        clearing = clearing,
        defenderExtra = tcWarriors, // pass this to your battle logic
        tcFaction = TwilightCouncil
      )
      soft()

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
        // f.supply :+= t // Uncomment and implement as needed
      })
      f.log(s"Removed $count Loyalist(s) from $from to supply.")
      soft()
  }
}