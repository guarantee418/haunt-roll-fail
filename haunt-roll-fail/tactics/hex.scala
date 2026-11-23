package tactics

// Hex grid math on axial coordinates (q, r), the same for pointy- and flat-top hexes.
// Axial is cube coordinates (q, r, s) with q + r + s == 0, so s is worked out when needed.
// Formulas from https://www.redblobgames.com/grids/hexagons/

case class Hex(q : Int, r : Int) {
    def s = -q - r

    def +(o : Hex) = Hex(q + o.q, r + o.r)
    def -(o : Hex) = Hex(q - o.q, r - o.r)

    def distance(o : Hex) = Hex.distance(this, o)
    def neighbors = Hex.directions.map(this + _)
}

object Hex {
    // The six directions, counter-clockwise from east
    val directions : List[Hex] = List(Hex(1, 0), Hex(1, -1), Hex(0, -1), Hex(-1, 0), Hex(-1, 1), Hex(0, 1))

    def distance(a : Hex, b : Hex) : Int = {
        val d = a - b
        d.q.abs.max(d.r.abs).max(d.s.abs)
    }

    // Nearest hex to fractional cube coordinates: round each, then fix the one
    // that moved most, so q + r + s == 0 still holds
    def round(fq : Double, fr : Double, fs : Double) : Hex = {
        var q = math.round(fq).toInt
        var r = math.round(fr).toInt
        val s = math.round(fs).toInt

        val dq = (q - fq).abs
        val dr = (r - fr).abs
        val ds = (s - fs).abs

        if (dq > dr && dq > ds)
            q = -r - s
        else
        if (dr > ds)
            r = -q - s

        Hex(q, r)
    }

    // The hexes on the straight line from a to b, both included, one per step.
    // A line running exactly along a hex edge could round either way, so both ends
    // are moved by a tiny nudge to pick a side; a negative nudge picks the other side.
    def line(a : Hex, b : Hex, nudge : Double = 1e-6) : List[Hex] = {
        val n = distance(a, b)

        if (n == 0)
            return List(a)

        // (+e, +e, -2e) keeps the sum at zero
        val (aq, ar, as) = (a.q + nudge, a.r + nudge, a.s - 2 * nudge)
        val (bq, br, bs) = (b.q + nudge, b.r + nudge, b.s - 2 * nudge)

        0.to(n).toList.map { i =>
            val t = i.toDouble / n
            round(aq + (bq - aq) * t, ar + (br - ar) * t, as + (bs - as) * t)
        }
    }
}
