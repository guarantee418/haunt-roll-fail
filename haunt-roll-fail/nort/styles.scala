package nort
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

package object elem {
    import hrf.elem._


    object styles extends BaseStyleMapping("nort") {
        import rules._

        val color = rules.color

        // Placeholder clan colors
        Bear --> color("#c08a52")
        Boar --> color("#8fbf4a")
        Goat --> color("#e3d28a")
        Raven --> color("#7f93e6")
        Snake --> color("#45b89f")
        Stag --> color("#e0b13a")
        Wolf --> color("#d9574a")
        Dragon --> color("#c0392b")
        Horse --> color("#b07d4f")
        Kraken --> color("#3fa7c9")
        Lynx --> color("#b9a07a")
        Ox --> color("#a0522d")
        Rat --> color("#8fa060")
        Squirrel --> color("#d98c3a")

        Blue --> color(Blue.hex)
        Red --> color(Red.hex)
        Yellow --> color(Yellow.hex)
        Purple --> color(Purple.hex)
        Green --> color(Green.hex)
        Orange --> color(Orange.hex)

        Food --> color("#e04848")
        Wood --> color("#c98a4b")
        Lore --> color("#8fa8d6")

        object title extends CustomStyle()

        // The clan picker's grid
        object pickIcon extends CustomStyle(height("7ex"), display("block"), margin.left("auto"), margin.right("auto"))
        // Adset: the draft's clans, as in the clan picker's grid
        object draftTile extends CustomStyle(display("inline-flex"), vertical.align("top"))
        object pickRandomBox extends CustomStyle(height("7ex"), display("flex"), align.items("center"), justify.content("center"))
        object pickRandom extends CustomStyle(font.size("300%"), line.height("1"))
        object menuIcon extends CustomStyle(height("2.4em"), vertical.align("middle"), margin.right("1ex"))
        object menuBoard extends CustomStyle(display("block"), width("60ex"), max.width("100%"), margin.left("auto"), margin.right("auto"))
        object menuWarchiefCard extends CustomStyle(display("block"), width("24ex"), max.width("60%"), margin.left("auto"), margin.right("auto"), margin.top("2ex"))
        object menuText extends CustomStyle(max.width("60ex"), margin.left("auto"), margin.right("auto"), margin.top("1ex"), text.align("center"))
        object menuCard extends CustomStyle(display("inline-block"), width("30%"), max.width("15ex"), margin("0.5ex"), vertical.align("top"))
        object card extends CustomStyle(display("inline-block"), width("22ex"), max.width("90%"), margin("0.3ex"))
        object handCard extends CustomStyle(display("block"), width("var(--nort-hand-card, 22ex)"))
        // Copies of the same card in hand, stacked (Card.handStack): each layer leaves room up and to the right for the copy behind
        object stackLayer extends CustomStyle(display("block"), width("fit-content"), position("relative"), padding.top("calc(0.07 * var(--nort-hand-card, 22ex))"), padding.right("calc(0.07 * var(--nort-hand-card, 22ex))"))
        object stackBack extends CustomStyle(position("absolute"), top("0"), right("0"), filter("brightness(0.6)"))
        object stackFront extends CustomStyle(position("relative"))
        // Training Fields Action cards: seven of them, so smaller than hand cards outside the ultrawide layout
        object drillCard extends CustomStyle(display("block"), width("var(--nort-hand-card, 12ex)"))
        // Raid cards are landscape: twice a hand card's width
        object raidHandCard extends CustomStyle(display("block"), width("calc(2 * var(--nort-hand-card, 22ex))"), max.width("100%"))
        // Icons in text; a number just before one stays on its line (GluesToNumber)
        object inlineIcon extends CustomStyle(height("1.4em"), vertical.align("middle")) with GluesToNumber
        // A player panel line of numbers and icons, kept on one row
        object panelLine extends CustomStyle(white.space("nowrap"))
        // The four card counts in a compact panel: a little smaller, so the row fits as the three resources do
        object cardLine extends CustomStyle(font.size("80%"))
        // The card icons without most of the clear space beside the tilted card
        object cardIcon extends CustomStyle(margin.left("-0.14em"), margin.right("-0.14em"))
        // The Stacked player panels: each icon above its number
        object stackedItem extends CustomStyle(display("inline-block"), text.align("center"), margin.left("0.4ex"), margin.right("0.4ex"))
        // The resources with the next harvest and the Winter costs under them, one column per resource (the panel's "ledger")
        object ledger extends CustomStyle(display("grid"), SimpleCSSRule("grid-template-columns", "auto auto auto auto auto"), SimpleCSSRule("align-items", "center"), SimpleCSSRule("justify-items", "center"), SimpleCSSRule("width", "max-content"), margin.left("auto"), margin.right("auto"), margin.bottom("0.3ex"), white.space("nowrap"))
        object ledgerCell extends CustomStyle(padding("0 0.2ex"))
        // Compact panels: a little smaller, so the season icons' column fits beside the three resources (as cardLine does for the cards)
        object ledgerCompact extends CustomStyle(font.size("90%"))
        // The Dragon's sacrifice, which no column holds, on its own row under the harvest's; zero width with a full
        // minimum, so they wrap within the columns instead of widening them
        object ledgerMaybe extends CustomStyle(opacity("0.55"))
        // The warchief in the player panels: grayed out while it isn't on the map
        object onBoard extends CustomStyle(white.space("nowrap"))
        object offBoard extends CustomStyle(white.space("nowrap"), filter("grayscale(1)"), opacity("0.45"))
        object ledgerGain extends CustomStyle(color("#7ccf62"))
        object ledgerLoss extends CustomStyle(color("#e5574a"))
        // Nothing in that column this harvest or winter
        object ledgerNone extends CustomStyle(opacity("0.35"))
        object buildIcon extends CustomStyle(height("2.6em"), vertical.align("middle"), margin.right("1ex"))
        // The end screen: the winners' clan cards, and warchief cards with the Warchiefs module
        object winnerCard extends CustomStyle(display("inline-block"), width("18ex"), margin("0.5ex"), vertical.align("top"))
        object winnerLine extends CustomStyle(font.size("120%"), margin.bottom("0.5ex"))
        object loreCard extends CustomStyle(display("block"), width("36ex"), max.width("100%"), margin.left("auto"), margin.right("auto"))

        object zoomCard extends CustomStyle(height("100%"), width("100%"), objectFit("contain"))

        // The court strip keeps its cards at full height and scrolls sideways when they don't fit. The cards' height comes from
        // the pane (container query units), not a percentage, so their width isn't sized from the images' natural size
        object strip extends CustomStyle(height("100%"), overflow.x("auto"), overflow.y("hidden"), SimpleCSSRule("container-type", "size"))
        object stripRow extends CustomStyle(display("flex"), justify.content("safe center"), height("100%"), padding("0.5ex"), box.sizing("border-box"))
        object stripGroup extends CustomStyle(display("flex"), flex.direction("column"), flex.shrink("0"), height("100%"), margin.left("1ex"), margin.right("1ex"))
        object stripTitle extends CustomStyle(text.align("center"), white.space("nowrap"), margin.bottom("0.3ex"))
        object stripCards extends CustomStyle(display("flex"), justify.content("center"), align.items("center"), flex.grow("1"), min.height("0"))
        object stripCard extends CustomStyle(height("calc(100cqh - 1.2em - 1.6ex)"), max.height("100%"), width("auto"), flex.shrink("0"), margin.left("0.3ex"), margin.right("0.3ex"))
        // The fold tab on the court's left edge, and the folded one-line bar
        object stripTab extends CustomStyle(display("flex"), align.items("center"), justify.content("center"), flex.shrink("0"), width("2.2ex"), min.height("1.4em"), margin.right("0.5ex"), border.radius("0.5ex"), background.color("#ffffff1a"), color("#d8d8d8"), font.size("120%"))
        object stripFolded extends CustomStyle(display("flex"), align.items("stretch"), height("100%"), padding("0.15ex 0.5ex"), box.sizing("border-box"), overflow("hidden"))
        object stripFoldedText extends CustomStyle(display("flex"), align.items("center"), justify.content("center"), flex.grow("1"), white.space("nowrap"), overflow("hidden"))
        object stripFoldedSide extends CustomStyle(display("flex"), flex.direction("column"), align.items("center"), height("100%"), padding("0.5ex 0.15ex"), box.sizing("border-box"), overflow("hidden"))
        object stripFoldedSideText extends CustomStyle(display("flex"), align.items("center"), justify.content("flex-start"), align.content("center"), SimpleCSSRule("flex-wrap", "wrap"), font.size("85%"), flex.grow("1"), white.space("nowrap"), overflow("hidden"), SimpleCSSRule("writing-mode", "vertical-rl"), margin.top("0.5ex"))
        object stripFoldedSideItem extends CustomStyle(margin.top("0.6ex"), margin.bottom("0.6ex"))
        object stripFoldedItem extends CustomStyle(margin.left("1.2ex"), margin.right("1.2ex"))
        object stripEmpty extends CustomStyle(font.style("italic"))

        object fame extends CustomStyle(color("#e8b84a"))

        // The Winter line in the player panels opens the Winter chart
        object tappable extends CustomStyle(SimpleCSSRule("text-decoration", "underline dotted"), SimpleCSSRule("text-underline-offset", "0.3ex"))
        // The discard pile overlay, opened from the action pane
        object discardCards extends CustomStyle(display("flex"), SimpleCSSRule("flex-wrap", "wrap"), SimpleCSSRule("justify-content", "center"), SimpleCSSRule("gap", "1ex"), margin.top("1ex"), margin.bottom("1ex"))
        object discardCard extends CustomStyle(width("18ex"), max.width("23%"), height("auto"))
        object winterChart extends CustomStyle(display("grid"), SimpleCSSRule("grid-template-columns", "auto auto auto"), SimpleCSSRule("column-gap", "1ex"), SimpleCSSRule("row-gap", "0.4ex"), SimpleCSSRule("width", "max-content"), max.width("100%"), margin.left("auto"), margin.right("auto"), margin.top("1ex"), margin.bottom("1ex"))
        object winterCell extends CustomStyle(padding("0.4ex 1.5ex"), text.align("left"))
        object winterHere extends CustomStyle(background.color("#3a3320"), outline.color("#e8b84a"), outline.style("solid"), outline.width("1px"))
        object creature extends CustomStyle(color("#c9a27a"), font.weight("bold"))
        // Dragon Clan's Sacrificial Pyre in its panel: the token with the units on its two circles (600x379 image)
        object pyre extends CustomStyle(position("relative"), display("inline-block"), vertical.align("middle"))
        object pyreSmall extends CustomStyle(height("1.6em"))
        object pyreLarge extends CustomStyle(height("40vh"), max.height("60vw"), margin.top("1ex"), margin.bottom("1ex"))
        object pyreImage extends CustomStyle(display("block"), height("100%"), width("auto"))
        object pyreSlot0 extends CustomStyle(position("absolute"), left("7.5%"), top("6.6%"), width("26.7%"), height("42.2%"))
        object pyreSlot1 extends CustomStyle(position("absolute"), left("8%"), top("52.8%"), width("26.7%"), height("42.2%"))
        object pyreEmpty extends CustomStyle(background.color("#000000c0"), border.radius("50%"))
        // A unit hides the figure printed on the circle
        object pyreUnit extends CustomStyle(background.color("#000000"), border.radius("50%"))

        object tile extends CustomStyle(display("inline-block"), width("26ex"), max.width("90%"), vertical.align("middle"), margin("0.3ex"))
        // Adset: a seat's three map tiles, side by side during the draft
        object seatTile extends CustomStyle(display("inline-block"), width("14ex"), vertical.align("middle"), margin("0.3ex"))
        object rot0 extends CustomStyle()
        object rot1 extends CustomStyle(transform("rotate(90deg)"))
        object rot2 extends CustomStyle(transform("rotate(180deg)"))
        object rot3 extends CustomStyle(transform("rotate(270deg)"))
        def rotate(r : Int) = $(rot0, rot1, rot2, rot3)(r % 4)

        object group extends CustomStyle(margin.top("0.5ex"), margin.bottom("0.5ex"))
        object inline extends CustomStyle(display("inline-block"))
        object nomargin extends CustomStyle(margin("0"))
        object nopadding extends CustomStyle(padding("0"))
        object halfmargin extends CustomStyle(margin.left("0.2ex"), margin.right("0.2ex"), margin.top("0.2ex"), margin.bottom("0.2ex"))
        object selected extends CustomStyle(filter("brightness(1.1) saturate(1.1)"), outline.color("#ffffff"), outline.style("solid"), outline.width("0.3vmin"))

        object smallname extends CustomStyle(font.weight("bold"))

        object artwork extends CustomStyle(max.height("100%"), max.width("100%"), margin("auto"))
        object middleScrollOut extends CustomStyle(display("flex"), align.items("center"), flex.wrap("wrap"), height("100%"), width("100%"))
        object middleScrollIn extends CustomStyle(overflow.y("auto"), height("auto"), margin("auto"))
        object seeThroughInner extends CustomStyle(background.color("#222222e0"))

        object status extends CustomStyle(
            border.width("4px"),
            border.width("0.4vmin"),
            text.align("center"),
            overflow.x("hidden"),
            overflow.y("auto"),
            text.overflow("ellipsis")
        )

        object fstatus extends CustomStyle(font.size("115%"))

        object statusUpper extends CustomStyle(height("100%"), overflow.x("hidden"), overflow.y("auto"))

        object titleLine extends CustomStyle(margin.top("-0.1ex"), margin.bottom("-0.4ex"))
    }

    implicit class ElemString(val s : String) extends AnyVal {
    }

    implicit class ElemElem(val elem : Elem) extends AnyVal {
        def larger = elem.styled(xstyles.larger125)
    }

    implicit class ElemInt(val n : Int) extends AnyVal {
        def cards = (n != 1).?(n.hl ~ " cards").|("a card")
    }

    object borders extends BaseStyleMapping("nort-border") {
        import rules._

        Bear --> outline.color("#5a3d1e")
        Boar --> outline.color("#3d5a1c")
        Goat --> outline.color("#6b5f2c")
        Raven --> outline.color("#2a3570")
        Snake --> outline.color("#1d5a4c")
        Stag --> outline.color("#6b4f10")
        Wolf --> outline.color("#6b1a12")
        Dragon --> outline.color("#5e1a12")
        Horse --> outline.color("#5a3d22")
        Kraken --> outline.color("#1a4f60")
        Lynx --> outline.color("#5a4d36")
        Ox --> outline.color("#50281a")
        Rat --> outline.color("#43502c")
        Squirrel --> outline.color("#6b4216")
    }
}
