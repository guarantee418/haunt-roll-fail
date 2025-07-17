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

  // Example: use BatAAA as the warrior, AssemblyAAA as the main token, etc.
  val warrior = BatAAA

  def advertising = BatAAA.img(this) ~ AssemblyAAA.img(this)
  def motto = "Debate".styled(this)

  // Pieces: adjust as needed
  def pieces(options: $[Meta.O]) =
    BatAAA *** 15 ++ AssemblyAAA *** 6 ++ ConvenedAAA *** 6 ++ CommuneAAA *** 6

  // Add any special notes
  override def note: Elem = HorizontalBreak ~ "Fan Faction"

  // Add your custom abilities here
  def abilities(options: $[Meta.O]) = $("Governors", "Entreating", "Peacekeepers")

  // Setup logic, actions, and rules would go here
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

    // BIRDSONG, DAYLIGHT, EVENING, etc. phases
    // Implement each phase as a case here, following your board's rules
    // For example:
    // case BirdsongNAction(0, f: TwilightCouncil.type) => ...
    // case DaylightNAction(0, f: TwilightCouncil.type) => ...
    // case EveningNAction(0, f: TwilightCouncil.type) => ...
  }
}