package tactics
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

    object styles extends BaseStyleMapping("tactics") {
        import rules._

        Red --> color("#e0453c")
        Blue --> color("#6f8dff")

        object hex extends CustomStyle(color("#d8cfa8"))
        object unitType extends CustomStyle(color("#e8dcc0"))

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
        object table extends CustomStyle(margin("auto"), text.align("left"))

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
        object seeThroughInner extends CustomStyle()
    }

    object borders extends BaseStyleMapping("tactics-border") {
        import rules._

        Red --> outline.color("#e0453c")
        Blue --> outline.color("#6f8dff")
    }
}
