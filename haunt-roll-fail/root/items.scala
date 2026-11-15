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


sealed trait Item extends Record with Elementary {
    def name = toString
    // what players read; `name` is what saved games write
    def title = name
    def elem = title.styled(styles.itemText)
    def imgid = "item-" + toString
    def img = Image(imgid, styles.piece)
    def imgEmpty = Image(imgid + "-empty", styles.piece)
    def imgD = Image(imgid, styles.iii)
    def imgEmptyD = Image(imgid + "-empty", styles.iii)
    def ref(exhausted : Boolean, damaged : Boolean) = ItemRef(this, exhausted, damaged)
    def pristine = ref(false, false)
    def exhaust = ref(true, false)
    def damage = ref(false, true)
}

case object Teapot extends Item
case object Boots extends Item
case object Sword extends Item
case object Torch extends Item
case object Crossbow extends Item
case object Hammer extends Item
case object Coins extends Item
case object Bag extends Item
case object Club extends Item
// The Dark Deck's items (The Doomed Expansion): the Vagabond's actions for the Book of Names and the Rabbit's Paw aren't done
case object BookOfNames extends Item { override def title = "Book of Names" }
case object RabbitsPaw extends Item { override def title = "Rabbit's Paw" }
case object CursedIdol extends Item { override def title = "Cursed Idol" }
// the Legends Scenarios' Ring (root/legends.scala): never crafted, discarded or returned to the supply
case object Ring extends Item
// the Legends Scenarios' monsters' weapons (root/legends-monsters.scala), taken by slaying them
case object Mauler extends Item { override def title = "The Mauler" }
case object SpiritSword extends Item { override def title = "Spirit Sword" }
case object Chainsaw extends Item { override def title = "Bloody Chainsaw" }
case object Slaymore extends Item { override def title = "The Slaymore" }
case object Bushblazer extends Item

case class ItemRef(item : Item, exhausted : Boolean, damaged : Boolean) extends HitTarget with Elementary with GoodMatch {
    def elem = (damaged, exhausted) @@ {
        case (true , true ) => item.title.styled(styles.itemText, styles.damexh)
        case (true , false) => item.title.styled(styles.itemText, styles.damaged)
        case (false, true ) => item.title.styled(styles.itemText, styles.exhausted)
        case (false, false) => item.title.styled(styles.itemText)
    }
    def imgid = item.imgid + damaged.??("-damaged") + exhausted.??("-exhausted")
    def img = Image(imgid, styles.piece)
    def imgD = Image(imgid, styles.iii)
    def exhaust = item.ref(true, damaged)
    def refresh = item.ref(false, damaged)
    def damage = item.ref(exhausted, true)
    def repair = item.ref(exhausted, false)
    def pristine = item.ref(false, false)
}

object ItemRef {
    implicit class ItemRefList(val l : $[ItemRef]) extends AnyVal {
        def ready = l.notExhausted.notDamaged
        def exhausted = l.%(_.exhausted)
        def damaged = l.%(_.damaged)
        def notExhausted = l.%!(_.exhausted)
        def notDamaged = l.%!(_.damaged)
        def count(i : Item) = l.%(_.item == i).num
        def sort = Item.order./~(i => l.%(_.item == i).sortBy(r => r.exhausted.??(1) + r.damaged.??(2)))
    }
}

object Item extends GoodMatch {
    def track = $(Teapot, Coins, Bag)./~(i => 3.times(i.ref(false, false)))
    def mucho = order./~(i => 4.times(i.ref(false, false)))
    def order = $(Teapot, Boots, Sword, Torch, Crossbow, Hammer, Coins, Bag) ++ dark ++ legends
    def legends = $[Item](Ring) ++ weapons
    def weapons = $[Item](Mauler, SpiritSword, Chainsaw, Slaymore, Bushblazer)
    def dark = $[Item](BookOfNames, RabbitsPaw, CursedIdol)
}

trait QuasiItem extends GoodMatch {
    def elem(selected : Boolean) : Image
    def available : Boolean
    def real = this @@ {
        case r : RealItem => Some(r.ref)
        case _ => None
    }
    def figure = this @@ {
        case FigureRemove(ref) => Some(ref)
        case _ => None
    }
}

trait RealItem extends QuasiItem {
    def ref : ItemRef
}

object ItemEmptySpace extends QuasiItem {
    def elem(selected : Boolean) = Image("item-x-spacer")
    def available = false
}

object ItemPlaceholder extends QuasiItem {
    def elem(selected : Boolean) = Image("item-x-placeholder")
    def available = false
}

case class FigureRemove(ref : Figure) extends QuasiItem {
    def elem(selected : Boolean) = Image(selected.?(ref.piece.iem(ref.faction)).|(ref.piece.imgid(ref.faction)))
    def available = true
}

case class FigureSelect(ref : Figure) extends QuasiItem {
    def elem(selected : Boolean) = Image(ref.piece.imgid(ref.faction))
    def available = true
}

case class ToRefresh(ref : ItemRef) extends RealItem {
    def elem(selected : Boolean) = Image(selected.?(ref.refresh).|(ref).imgid)
    def available = ref.exhausted
}

case class ToExhaust(ref : ItemRef) extends RealItem {
    def elem(selected : Boolean) = Image(selected.?(ref.exhaust).|(ref).imgid)
    def available = ref.exhausted.not && ref.damaged.not && ref.item != CursedIdol
}

case class ToExhaustDamage(l : $[Item], ref : ItemRef) extends RealItem {
    def elem(selected : Boolean) = Image(selected.?(l.has(ref.item).?(ref.exhaust).|(ref.exhaust.damage)).|(ref).imgid)
    def available = ref.exhausted.not && ref.damaged.not && ref.item != CursedIdol
}

case class ToRepair(ref : ItemRef) extends RealItem {
    def elem(selected : Boolean) = Image(selected.?(ref.repair).|(ref).imgid)
    def available = ref.damaged
}

case class ToDamage(ref : ItemRef) extends RealItem {
    def elem(selected : Boolean) = Image(selected.?(ref.damage).|(ref).imgid)
    def available = ref.damaged.not && ref.item != CursedIdol
}

case class ToDiscard(ref : ItemRef) extends RealItem {
    def elem(selected : Boolean) = Image(selected.?(ref.item.imgid + "-empty").|(ref.imgid))
    // the Dark Deck's Cursed Idol is never discarded, only given, taken or swapped (Law of Doom D3.1.2)
    def available = ref.item != CursedIdol && Item.legends.has(ref.item).not
}

case class ToShow(ref : ItemRef) extends RealItem {
    def elem(selected : Boolean) = Image(ref.imgid)
    def available = false
}

case class ToCraft(ref : SuitAsset) extends QuasiItem {
    def elem(selected : Boolean) = dt.CraftSuit(ref)
    def available = false
}
