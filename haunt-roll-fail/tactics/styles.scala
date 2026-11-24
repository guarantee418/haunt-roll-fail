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

        US --> color("#9fcf6a")
        Wehrmacht --> color("#ff7a70")

        Manpower --> color("#d8d26a")
        Munitions --> color("#ff8a50")
        Fuel --> color("#6fd27a")
        VP --> color("#9fb8ff")
        XP --> color("#f0f0f0")

        object hex extends CustomStyle(color("#d8cfa8"))
        object kind extends CustomStyle(color("#e8dcc0"))

        object die extends CustomStyle(height("1.5em"), vertical.align("middle"), margin("0.05em"))
        object token extends CustomStyle(height("2.4ex"), vertical.align("middle"))
        object boardImg extends CustomStyle(max.width("100%"), max.height("100%"), margin("auto"))

        object name extends CustomStyle()
        object username extends CustomStyle()
        object dead extends CustomStyle()
        object used extends CustomStyle()
        object halfmargin extends CustomStyle(margin("0.25ex"))
        object inline extends CustomStyle(display("inline-block"))
        object nomargin extends CustomStyle(margin("0"))
        object group extends CustomStyle()
        object selected extends CustomStyle()
        object title extends CustomStyle(font.size("115%"), font.weight("bold"))
        object badge extends CustomStyle(font.size("75%"), font.weight("bold"), color("#14181f"), background.color("#e8dcc0"), border.radius("0.5ex"), padding.left("0.7ex"), padding.right("0.7ex"), margin.left("0.8ex"), white.space("nowrap"))
        object stat extends CustomStyle(display("inline-block"), white.space("nowrap"), margin.left("0.5ex"), margin.right("0.5ex"))

        object icon extends CustomStyle(height("1.3em"), vertical.align("middle"), margin.right("0.35em"))
        object iconOnly extends CustomStyle(height("1.35em"), vertical.align("middle"), margin.left("0.1em"), margin.right("0.1em"))
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

    object borders extends BaseStyleMapping("tactics-border") {
        import rules._

        US --> outline.color("#6f9a3e")
        Wehrmacht --> outline.color("#c9423b")
    }
}
