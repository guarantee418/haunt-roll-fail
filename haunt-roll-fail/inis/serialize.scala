package inis
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

import hrf.serialize._

import fastparse._, NoWhitespace._

object Serialize extends Serializer {
    val gaming = inis.gaming

    def writeFaction(f : F) = Meta.writeFaction(f)
    def parseFaction(s : String) = Meta.parseFaction(s)

    val prefix = "inis."

    override def write(o : Any) : String = o match {
        // case p : Figure => p.faction.short + "/" + write(p.piece) + "/" + p.index
        // case c : Color => c.name
        case _ => super.write(o)
    }
}
