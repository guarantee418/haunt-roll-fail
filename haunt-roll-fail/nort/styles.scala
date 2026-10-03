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

        Food --> color("#e04848")
        Wood --> color("#c98a4b")
        Lore --> color("#8fa8d6")

        object title extends CustomStyle()

        object card extends CustomStyle(display("inline-block"), width("12ex"), margin("0.3ex"))
        object fame extends CustomStyle(color("#e8b84a"))

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
    }
}
