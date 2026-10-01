package yarg
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

    def mod(from : Int, to : Int = -999) : Elem =
        (if (from == 0)
            "0".hl
        else
        if (from > 0)
            ("+" ~ from.toString).hl
        else
            ("-" ~ from.abs.toString).hl) ~ (to != -999).?(hrf.elem.MDash.toString ~ to.toString.hl)

    object styles extends BaseStyleMapping("yarg") {
        import rules._

        Pirates --> color("#007FFF")
        FireKnights --> color("#FF0000")
        Mages --> color("#c000c0")

        object health extends CustomStyle(color("#cc2012"))
        object mana extends CustomStyle(color("#0b3dcc"))
        object rage extends CustomStyle(color("#c4c400"))

        object selected extends CustomStyle(filter("brightness(1.1) saturate(1.1)"), outline.color("#ffffff"), outline.style("solid"), outline.width("0.3vmin"))

        object board extends CustomStyle(border.width("1vmin"))

        object status extends CustomStyle(
            border.width("4px"),
            border.width("0.4vmin"),
            text.align("center"),
            font.size("90%"),
            white.space("nowrap"),
            overflow.x("hidden"),
            overflow.y("hidden"),
            text.overflow("ellipsis")
        )
    }

    object borders extends BaseStyleMapping("yarg-border") {
        import rules._

        Pirates --> outline.color("#0055AA")
        FireKnights --> outline.color("#AA0000")
        Mages --> outline.color("#800080")
    }
}
