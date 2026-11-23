package pincer
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

    object styles extends BaseStyleMapping("pincer") {
        import rules._

        Red --> color("#e0453c")
        Green --> color("#3fae4f")
        Blue --> color("#6f8dff")
        Yellow --> color("#e8c42e")

        Wheat --> color("#e0a22a")
        Fish --> color("#7fc6c0")
        Mustang --> color("#c08a4a")
        Lumber --> color("#7fa652")
        Stone --> color("#b4aa98")
        Iron --> color("#c79bb0")

        object hex extends CustomStyle(color("#d8cfa8"))
        object kind extends CustomStyle(color("#e8dcc0"))
        object seal extends CustomStyle(color("#f0c040"), font.weight("bold"))
        object maneuver extends CustomStyle(font.style("italic"))

        object card extends CustomStyle(width("9.5ex"), margin("0.3ex"), border.radius("0.8ex"))
        object sealImg extends CustomStyle(width("6ex"), margin("0.15ex"))
        object sealSmall extends CustomStyle(width("3.6ex"), margin("0.1ex"), vertical.align("middle"))
        object die extends CustomStyle(width("4.2ex"), vertical.align("middle"))
        object piece extends CustomStyle(height("3ex"), vertical.align("middle"))
        object injuredPiece extends CustomStyle(height("3ex"), vertical.align("middle"), filter("opacity(45%)"))

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
        object row extends CustomStyle(white.space("nowrap"))
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

    object borders extends BaseStyleMapping("pincer-border") {
        import rules._

        Red --> outline.color("#e0453c")
        Green --> outline.color("#3fae4f")
        Blue --> outline.color("#6f8dff")
        Yellow --> outline.color("#e8c42e")
    }
}
