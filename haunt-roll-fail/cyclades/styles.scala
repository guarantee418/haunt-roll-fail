package cyclades
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

package object elem {

    object styles extends BaseStyleMapping("cyclades") {
        import rules._

        Red --> color("#f0473f")
        Yellow --> color("#f2cb2a")
        Blue --> color("#5a9cff")
        Black --> color("#c8c8d0")
        Green --> color("#46c25a")

        object god extends CustomStyle(color("#f0d58a"), font.weight("bold"))
        object building extends CustomStyle(color("#bfe3f0"))
        object creature extends CustomStyle(color("#d8b0f0"), font.style("italic"))
        object isle extends CustomStyle(color("#e8d8a8"))
        object sea extends CustomStyle(color("#8fd0e8"))

        // the Tutorial's lessons and tips (tutorial.scala)
        object tutorialText extends CustomStyle(width("60ex"), max.width("100%"), margin.left("auto"), margin.right("auto"), margin.top("1ex"), text.align("left"))
        object tutorialTip extends CustomStyle(text.align("left"), SimpleCSSRule("border-left", "0.5ex solid #8fbf6f"))
        object menuText extends CustomStyle(text.align("center"))

        object card extends CustomStyle(width("9.5ex"), margin("0.3ex"), border.radius("0.8ex"))
        object godTile extends CustomStyle(width("100%"), max.width("40ex"), margin("0.2ex"))
        object godHead extends CustomStyle(height("3.2ex"), vertical.align("middle"), margin("0.1ex"))
        object piece extends CustomStyle(height("2.4ex"), vertical.align("middle"))
        object buildingIcon extends CustomStyle(height("2.2ex"), vertical.align("middle"), border.radius("0.3ex"), margin("0.1ex"))
        object trackCard extends CustomStyle(width("6.5ex"), margin("0.2ex"), border.radius("0.5ex"), vertical.align("top"))

        object name extends CustomStyle()
        object username extends CustomStyle()
        object dead extends CustomStyle()
        object used extends CustomStyle()
        object cardX extends CustomStyle()
        object halfmargin extends CustomStyle(margin("0.25ex"))
        object inline extends CustomStyle(display("inline-block"))
        object nomargin extends CustomStyle(margin("0"))
        object group extends CustomStyle()
        object selected extends CustomStyle()
        object cardS extends CustomStyle()
        object title extends CustomStyle(font.size("115%"), font.weight("bold"))
        object badge extends CustomStyle(font.size("75%"), font.weight("bold"), color("#14181f"), background.color("#e8dcc0"), border.radius("0.5ex"), padding.left("0.7ex"), padding.right("0.7ex"), margin.left("0.8ex"), white.space("nowrap"))
        object stat extends CustomStyle(display("inline-block"), white.space("nowrap"), margin.left("0.5ex"), margin.right("0.5ex"))
        object legend extends CustomStyle(text.align("left"), padding("0.6ex"))

        object row extends CustomStyle(font.size("90%"))
        object small extends CustomStyle(font.size("85%"))

        object status extends CustomStyle(
            border.width("4px"),
            border.width("0.4vmin"),
            text.align("center"),
            font.size("90%"),
            overflow.x("hidden"),
            overflow.y("auto"),
            text.overflow("ellipsis")
        )

        object fstatus extends CustomStyle()
        object gstatus extends CustomStyle(text.align("center"))
        object artwork extends CustomStyle(max.width("100%"), max.height("100%"), margin("auto"))
        object seeThroughInner extends CustomStyle()
    }

    object borders extends BaseStyleMapping("cyclades-border") {
        import rules._

        Red --> outline.color("#f0473f")
        Yellow --> outline.color("#f2cb2a")
        Blue --> outline.color("#5a9cff")
        Black --> outline.color("#c8c8d0")
        Green --> outline.color("#46c25a")
    }
}
