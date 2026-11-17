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

// The "?" of the fan, playtest and mirror factions (Gman, 2026-11-17): who made the faction, then what it does and how
// it plays, a line each. Shown by the faction picker's ? button (Meta.factionInfo) and Advanced Setup's faction toggles
// (IncludeFaction.explain). Official factions have none.
object FanInfo {
    private def by(who : String, where : Elem) : Elem = "Made by ".txt ~ who.hl ~ ", " ~ where ~ "."
    private def doomed : Elem = "from the fan-made " ~ "Doomed Expansion".hh ~ " (BoardGameGeek 482234)"
    private def playtest(f : Faction, date : String) : Elem = "Made by ".txt ~ "Leder Games".hl ~ ": the public playtest version of the " ~ f.elem ~ " from " ~ date.hh ~ ", before " ~ "Root: Homeland".hh ~ " was printed."
    private def hrf : Elem = "Made by ".txt ~ "JT".hl ~ " for " ~ "HRF".hh ~ " (hrf.im), the site this one is built on."
    private def mirror(o : Faction) : Elem = "A second copy of the " ~ o.elem ~ ", with its own name and colour, so two of them can play in one game. It plays exactly like the " ~ o.elem ~ "."
    private def line(head : String, text : Elem) : Elem = (head + ": ").hh ~ text

    def about(f : Faction) : $[Elem] = f match {
        case WW => $(
            by("slugfacekillah", doomed),
            "Carrion birds that feed on battle: they follow the fighting, take the warriors others lose, and spread despair where nobody can build.".txt,
            line("Vultures and the Mess", "warriors removed in their Frenzy go to the Mess, which feeds them later.".txt),
            line("Despair", "Despair tokens stop anyone building in their clearings.".txt),
            line("Actions", "Bolster, Converge, Frenzy, Feast, Consume and Scavenge. Their discard pile is hidden from the others.".txt),
        )
        case DU => $(
            by("slugfacekillah", doomed),
            "A doomsday cult run like a trade union: it makes demands with dice, goes on strike, and lights Flames in the forests until the world ends.".txt,
            line("Demand dice", "black, orange and yellow dice set what the Union asks of the others; the union card holds strikes, locks and Cats on Strike!".txt),
            line("Actions", "Recruit, Renegotiate, Crusade, Consecrate, and Craft with revealed cards that score in Evening.".txt),
            line("Flames", "Flames go in enclosed forests, and no enemy may move there.".txt),
            line("The End", "the Union also wins when no Flame is left to light.".txt),
        )
        case WG => $(
            by("slugfacekillah", doomed),
            "A secret guild that lives in the forests and plays mostly on other players' turns. Only with 5 or more players.".txt,
            line("Forests", "its warriors stand in forests, not clearings.".txt),
            line("Insight", "face-down Insight markers, known only to the Guild, are revealed on other players' turns when what they wait for happens (buildings placed, crafts, removals).".txt),
            line("Favoured Suit", "counts the cards discarded from an enemy's hand.".txt),
            line("Midnight", "Midnight actions come at the start of a turn, after another player's. Instigate's points go to the Guild.".txt),
        )
        case EE => $(
            by("slugfacekillah", "a Re-Imagining of the " ~ ED.elem ~ " " ~ doomed),
            "The Eyrie in its decline: still bound by a Decree with Turmoil and leaders, but its five Cardinals can be moved by anyone.".txt,
            line("The Red Menace", "other factions may move the Cardinals with its warriors as their own, on their turn.".txt),
            line("Caravans", "caravans craft and move with its warriors, and the clearing's ruler draws a card when one arrives (Roving Caravans).".txt),
            line("Lords of Corruption", "it rules where tied, on its own turn.".txt),
            line("Leaders", "the new leaders Patron, Megalomaniac and Hedonistic. A New Roost places 3 warriors.".txt),
        )
        case DM => $(
            by("slugfacekillah", "a Re-Imagining of the " ~ MC.elem ~ " " ~ doomed),
            "The cats arrive by sea to conquer the Woodland.".txt,
            line("Landing", "its ship lands on a river clearing at the map's edge.".txt),
            line("Outposts", "an outpost is placed with two matching treaties, cards kept face up, and treaties count as warriors in battle.".txt),
            line("The Keep", "it ships wood to the Keep; with all 8 there, it wins.".txt),
        )
        case DV => $(
            by("slugfacekillah", "a Re-Imagining of the " ~ VB.elem ~ " " ~ doomed),
            "A Vagabond gone bad, who makes deals and collects bounties. With 4 or more players.".txt,
            line("Despot Infamy", "it scores by Infamy differently from the Vagabond.".txt),
            line("Peace offers", "it makes peace offers to Hostile factions.".txt),
            line("Collect Bounty", "bounty quests, offered in every menu of the current faction, freshened each Evening.".txt),
        )
        case NW => $(
            by("slugfacekillah", "a Re-Imagining of the " ~ KD.elem ~ " " ~ doomed),
            "The Knaves of the Deepwood, played with Woodland Alliance warriors: the same Captains, items and hostages.".txt,
        )
        case XC => $(
            by("slugfacekillah", "version 1.5, from the fan-made " ~ "Outsiders Expansion".hh),
            "Frog wizards who cast spells and brew magic.".txt,
            line("Wizardry", "cast a spell from the Repertoire, paying its cost, in Daylight or whenever a battle starts where it has warriors.".txt),
            line("Enchanted", "its tokens other than statues can't be removed by other players.".txt),
            line("Birdsong", "Dispel the Mudman, score 1 for each statue (and 1 more with all 3 schools out), craft with schools.".txt),
            line("Daylight", "up to 4 actions: Move, Battle, Build (a school where it rules) and Recruit (a warrior at each school).".txt),
            line("Evening", "Research a new spell into the Repertoire, Brew magic into the Cauldron with matching cards, then draw.".txt),
        )
        case BP => $(
            by("slugfacekillah", "version 2.1, from the fan-made " ~ "Outsiders Expansion".hh),
            "A gang of pugs led by their Alpha, digging up the bones buried around the Woodland and bringing them home for points.".txt,
            line("The Alpha", "deals an extra hit attacking, is never removed, and takes one hit in its clearing; Bone Patrol always rules where it is.".txt),
            line("Obedient", "its warriors only move to or from the Alpha's clearing.".txt),
            line("Guard Dogs", "on their turn, other players may give it a matching card to share rule of a clearing it rules.".txt),
            line("Bones", "buried face down where nobody else can touch them; fetched in Evening, carried by the pugs and recalled at stashes for 2 points each, into the Bone Collection (to craft with) or the Rollover (to bury again).".txt),
            line("Daylight", "3 actions, plus 1 per bird card: March, Battle, Recruit (a pug at each stash) and Build (a stash).".txt),
            line("Evening", "fetch or bury bones, craft with the Bone Collection, then draw 1, or 1 for each draw showing on its tracks.".txt),
        )
        case FH => $(
            by("jgregory17", "fan-made".txt),
            "Hamster farmers who want nothing more than to raise their crops and give gifts to their friends.".txt,
            line("Friendly", "on their turns, they and their friends count each other's pieces as their own for rule.".txt),
            line("Farmers first, fighters second", "their warriors take the lower die in battle, except where the Sheriff is.".txt),
            line("Grain", "each farm makes a grain in Birdsong; at a mill, Grind flips one to score 2, and they and each friend get the gift on its back (a point, a card or a warrior).".txt),
            line("Friends and Foes", "a player becomes a foe by removing their pieces or battling them; friends are reset each Evening.".txt),
        )
        case CUv2 => $(
            by("blbyblby", "version from 2024-05-18".txt),
            "A fox Chancellor who gains power only by giving up ideals: every morality taken off the Corruption track buys an action, but leaves less to give.".txt,
            line("Corruption", "the number of actions (Move, Battle, Build a palace) and the recruits at palaces; Celebrate Freedoms scores the track's next empty space.".txt),
            line("Moral Compromises", "remove a morality for Bribery (a card counts as a bird), Excessive Force (deal your hits again) or Living Shields (a palace flips shielded).".txt),
            line("Deserters", "revealing a Deserters space removes 3 of its warriors for good.".txt),
            line("On this site", "Bribery works only for scoring and shields.".txt),
        )
        case CU => $(
            by("blbyblby", "the original version, from " ~ "RootJam".hh ~ " (a fan design jam)"),
            "The first Chancellor's Utopia: the same Corruption track and Moral Compromises as the later version, with older costs and spaces.".txt,
            line("On this site", "Bribery works only for scoring and shields.".txt),
        )
        case OK => $(
            "Made by ".txt ~ "Leder Games".hl ~ ": the public print-and-play of the " ~ KI.elem ~ " from February 25, an early design of the published faction.",
            "Badger knights who recover relics from the forests, with caravans, waystations and a mission each turn.".txt,
            line("Armored", "ignore the first hit in battle.".txt),
            line("Cumbersome", "move one warrior, or up to three with a caravan.".txt),
            line("Relics", "Delve moves relics out of forests; Recover rolls for them at waystations, scoring 2 and filling the relic rows.".txt),
            line("Missions", "Craft, Move and Battle, or Escort: chosen each Evening, each gives a bonus.".txt),
        )
        case KDvA => $(
            playtest(KD, "October 22, 2024"),
            "Three Captains, each acting in its own phase, who take hostages and ransom them.".txt,
            line("Prepare or Strike", "in each phase its Captain either draws an item and places warriors, or moves, battles and uses its items.".txt),
            line("Hostages", "enemy warriors removed in its attacks are taken as hostages to an adjacent forest, and ransomed in Evening for points.".txt),
        )
        case TCvA => $(
            playtest(TC, "October 30, 2024"),
            "Bat assemblies that govern their clearings and act through revealed cards.".txt,
            line("Governors", "enemies can't craft with, flip, place or remove pieces at its Governing assemblies, except in battle.".txt),
            line("Reveal and Convene", "it reveals cards to act in matching clearings, then in Evening returns them to Banish, Agitate or Empower.".txt),
        )
        case TD => $(
            playtest(LDvE, "August 3, 2024"),
            "The first frog design: Frog tokens add the Frog suit to their clearings and turn Militant when the frogs are attacked.".txt,
            line("Woodland Intolerance", "a die flips a matching Frog token to Militant each Birdsong; Militant tokens punish those near them with Reprisals.".txt),
            line("Reconcile", "enemies may give it a card to calm a Militant token, drawing a Frog card.".txt),
        )
        case LDvB => $(
            playtest(LDvE, "September 17, 2024"),
            "Frog tokens add the Frog suit to their clearings; Peaceful ones score, Militant ones fight and punish.".txt,
            line("Anger or Soothe", "flip all Peaceful tokens of one suit to Militant, or every Militant one it rules to Peaceful.".txt),
            line("Integrate", "scores 1 and draws 1 per Peaceful token in the suit with the most, unless Militant ones outnumber them.".txt),
        )
        case LDvC => $(
            playtest(LDvE, "October 1, 2024"),
            "Frog tokens add the Frog suit to their clearings; Peaceful ones score, Militant ones fight and punish.".txt,
            line("Anger or Soothe", "flip all Peaceful tokens of one suit to Militant, or every Militant one it rules to Peaceful.".txt),
            line("Integrate", "once for each of fox, rabbit and mouse, spend a card to score 1 per Peaceful token of that suit.".txt),
        )
        case LDvD => $(
            playtest(LDvE, "October 24, 2024"),
            "The last playtest frogs, closest to the printed Lilypad Diaspora.".txt,
            line("Anger", "flip all Peaceful tokens of one suit, even on its board, to Militant.".txt),
            line("Settle and Soothe", "in Evening, place a Frog token or flip a Militant one to Peaceful, ruling its clearing or spending a matching card.".txt),
        )
        case AF => $(
            hrf,
            "An unfinished variant of the " ~ WA.elem ~ ", not a published design.",
            line("Outrage", "set off by moving in or battling where it has sympathy, not by removing sympathy; birds can't pay it.".txt),
            line("Revolt", "costs more where enemy warriors outnumber it.".txt),
            line("Terror", "discard a bird card to take a supporter back into hand.".txt),
            line("Delayed scoring", "sympathy scores when it is removed, not when it is placed.".txt),
        )
        case SF => $(
            hrf,
            "An unfinished variant of the " ~ RF.elem ~ ", not a published design.",
            line("Moves", "only between clearings it rules (no river moves).".txt),
            line("Scoring", "trade posts score more as there are more of them, and Surplus scores in Birdsong; Boycott costs a point for each faction that isn't its friend.".txt),
            line("Services", "cards, Balloon, Talent Scout, Outsourcing, Shares and Peacekeepers.".txt),
        )
        case MB => $(
            hrf,
            "A third copy of the " ~ VB.elem ~ ", with its own name and colour, so three can play in one game.",
        )
        case FU => $(hrf, mirror(WA))
        case BK => $(hrf, mirror(MC))
        case PE => $(hrf, mirror(ED))
        case NB => $(hrf, mirror(VB))
        case CM => $(hrf, mirror(LC))
        case DR => $(hrf, mirror(UD))
        case RI => $(hrf, mirror(CC))
        case LK => $(hrf, mirror(LH))
        case _ => $[Elem]()
    }
}
