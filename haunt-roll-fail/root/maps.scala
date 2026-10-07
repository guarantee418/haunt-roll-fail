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

object AutumnBoard extends Board {
    val id = "autumn"
    val name = "Autumn"

    object Hill extends Clearing(1)
    object Glade extends Clearing(2)
    object Meadow extends Clearing(2)
    object Mountain extends Clearing(2)
    object Haven extends Clearing(1)
    object Creek extends Clearing(2)
    object Beach extends Clearing(2)
    object Weald extends Clearing(1)
    object Dune extends Clearing(2)
    object Waterfall extends Clearing(3)
    object Pond extends Clearing(2)
    object Quarry extends Clearing(2)

    val defaultMapping = Map[Suitable, BaseSuit](
        Hill -> Fox,
        Glade -> Fox,
        Meadow -> Fox,
        Mountain -> Fox,
        Haven -> Rabbit,
        Creek -> Rabbit,
        Beach -> Rabbit,
        Weald -> Rabbit,
        Dune -> Mouse,
        Waterfall -> Mouse,
        Pond -> Mouse,
        Quarry -> Mouse
    ).view.mapValues($).toMap

    val clearings = $(Hill, Creek, Beach, Quarry, Dune, Glade, Waterfall, Mountain, Haven, Meadow, Pond, Weald)

    val ruins = $(Glade, Mountain, Beach, Waterfall)

    val diagonals = $((Hill, Weald), (Quarry, Haven))

    val inner = $(Beach, Glade, Waterfall)

    // The trackers in the usual places on this board's image, 20 pixels bigger on every side
    override val scoreTrack = |((108.3, 2162.0, 74.63))

    override def itemSlots = itemGrid(445.0, 88.0, 109.5, 193.5)

    def connected(c : Clearing) = c @@ {
        case Hill => $(Creek, Beach, Dune)
        case Glade => $(Beach, Waterfall, Pond, Haven, Dune)
        case Meadow => $(Pond, Haven)
        case Mountain => $(Weald, Waterfall, Quarry)
        case Haven => $(Glade, Meadow, Dune)
        case Creek => $(Quarry, Hill)
        case Beach => $(Quarry, Glade, Hill)
        case Weald => $(Mountain, Pond, Waterfall)
        case Dune => $(Hill, Glade, Haven)
        case Waterfall => $(Mountain, Weald, Glade)
        case Pond => $(Weald, Meadow, Glade)
        case Quarry => $(Mountain, Creek, Beach)
    }

    object AutumnN extends UnnamedForest
    object AutumnNW extends UnnamedForest
    object Witchwood extends NamedForest("Witchwood")
    object AutumnW extends UnnamedForest
    object AutumnS extends UnnamedForest
    object AutumnE extends UnnamedForest
    object AutumnSW extends UnnamedForest

    val forests = $(AutumnN, AutumnNW, Witchwood, AutumnW, AutumnS, AutumnE, AutumnSW)

    def fromForest(f : Forest) = f @@ {
        case AutumnN => $(Hill, Creek, Quarry, Beach)
        case AutumnNW => $(Dune, Hill, Beach, Glade)
        case Witchwood => $(Glade, Beach, Quarry, Mountain, Waterfall)
        case AutumnW => $(Dune, Glade, Haven)
        case AutumnS => $(Glade, Waterfall, Weald, Pond)
        case AutumnE => $(Waterfall, Mountain, Weald)
        case AutumnSW => $(Haven, Glade, Pond, Meadow)
    }

    def byRiver(c : Clearing) = c @@ {
        case Haven => $(Pond)
        case Creek => $(Beach)
        case Beach => $(Creek, Waterfall)
        case Waterfall => $(Beach, Pond)
        case Pond => $(Waterfall, Haven)
        case _ => $()
    }

    def fix(a : Int, b : Int) = (a + 20, b + 20)

    def center(r : Region) : (Int, Int) = r @@ {
        case Hill => fix(274, 301)
        case Haven => fix(280, 1798)
        case Quarry => fix(2106, 506)
        case Weald => fix(1991, 1930)
        case Creek => fix(1304, 199)
        case Beach => fix(1034, 635)
        case Dune => fix(244, 903)
        case Glade => fix(742, 1224)
        case Waterfall => fix(1522, 1067)
        case Mountain => fix(2173, 1151)
        case Meadow => fix(881, 1981)
        case Pond => fix(1405, 1706)

        case AutumnN => $((837, 386), (1554, 463)).shuffle.head
        case Witchwood => $((1853, 804), (1160, 980)).shuffle.head
        case AutumnNW => (601, 770)
        case AutumnS => $((1194, 1346), (1560, 1400)).shuffle.head
        case AutumnSW => $((940, 1592), (650, 1749)).shuffle.head
        case AutumnE => (1893, 1377)
        case AutumnW => (403, 1344)

        case Burrow(_) => (1208, 999999)
        case _ => (0, 0); throw new Error("no center for " + r)
    }

    override def port(r : Region)(flooded : $[Clearing]) = r @@ {
        case Creek => fix(1304-50-20, 199+100)
        case Beach => fix(1034+200, 635+100)
        case Waterfall => fix(1522+200-50, 1067-200+50)
        case Pond => fix(1405-210-20, 1706-20)
        case Haven => fix(280+150+20+10, 1798-150+20+10)

        case _ => (0, 0); throw new Error("no port for " + r)
    }

    override def gates(r : Region) = r @@ {
        case Hill => $((217, 284))./(fix)
        case Glade => $((849, 1206), (748, 1313))./(fix)
        case Meadow => $((955, 1865), (914, 1999))./(fix)
        case Mountain => $((2201, 1068), (2094, 1183))./(fix)
        case Haven => $((360, 1797))./(fix)
        case Creek => $((1371, 152), (1239, 168))./(fix)
        case Beach => $((1063, 751), (928, 585))./(fix)
        case Weald => $((1953, 1907))./(fix)
        case Dune => $((306, 816), (201, 1015))./(fix)
        case Waterfall => $((1475, 1143), (1590, 1072), (1488, 997))./(fix)
        case Pond => $((1401, 1611), (1321, 1740))./(fix)
        case Quarry => $((2103, 481), (2157, 587))./(fix)
        case _ => super.gates(r)
    }
}

object WinterBoard extends Board {
    val id = "winter"
    val name = "Winter"

    object Mound extends Clearing(1)
    object Trench extends Clearing(2)
    object Pit extends Clearing(2)
    object Moor extends Clearing(1)

    object Drift extends Clearing(1)
    object Wade extends Clearing(3)
    object Bend extends Clearing(3)
    object Ford extends Clearing(1)

    object Spire extends Clearing(2)
    object Rock extends Clearing(2)
    object Dale extends Clearing(2)
    object Hedge extends Clearing(2)

    val clearings = $(Mound, Trench, Pit, Moor, Drift, Wade, Bend, Ford, Spire, Rock, Dale, Hedge)

    val ruins = $(Wade, Bend, Rock, Dale)

    val diagonals = $((Mound, Hedge), (Moor, Spire))

    val inner = $(Wade, Bend)

    // The score tracker along the bottom edge of this board's image, 8 pixels taller than most
    override val scoreTrack = |((88.3, 2130.0, 74.63))

    object WinterNW extends UnnamedForest
    object Deadwood extends NamedForest("Deadwood")
    object WinterNE extends UnnamedForest
    object WinterW extends UnnamedForest
    object WinterSW extends UnnamedForest
    object Mirkwood extends NamedForest("Mirkwood")
    object WinterSE extends UnnamedForest
    object WinterE extends UnnamedForest

    val forests = $(WinterNW, Deadwood, WinterNE, WinterW, WinterSW, Mirkwood, WinterSE, WinterE)

    def connected(c : Clearing) = c @@ {
        case Mound => $(Trench, Wade, Drift)
        case Trench => $(Mound, Pit)
        case Pit => $(Trench, Moor)
        case Moor => $(Ford, Bend, Pit)
        case Drift => $(Mound, Spire)
        case Wade => $(Rock, Spire, Mound)
        case Bend => $(Moor, Hedge, Dale)
        case Ford => $(Hedge, Moor)
        case Spire => $(Wade, Rock, Drift)
        case Rock => $(Wade, Dale, Spire)
        case Dale => $(Bend, Hedge, Rock)
        case Hedge => $(Ford, Dale, Bend)
    }

    def fromForest(f : Forest) = f @@ {
        case WinterNW => $(Mound, Drift, Wade)
        case Deadwood => $(Mound, Trench, Pit, Moor, Bend, Wade)
        case WinterNE => $(Moor, Bend, Ford)
        case WinterW=> $(Drift, Wade, Spire)
        case WinterSW => $(Wade, Spire, Rock)
        case Mirkwood => $(Wade, Bend, Dale, Rock)
        case WinterSE => $(Bend, Dale, Hedge)
        case WinterE=> $(Bend, Ford, Hedge)
    }

    def byRiver(c : Clearing) = c @@ {
        case Drift => $(Wade)
        case Wade => $(Drift, Bend)
        case Bend => $(Wade, Ford)
        case Ford => $(Bend)
        case _ => $()
    }

    def center(r : Region) : (Int, Int) = r @@ {
        case Mound => (322, 292)
        case Moor => (2104, 518)
        case Hedge => (2028, 1892)
        case Spire => (337, 1792)
        case Trench => (908, 388)
        case Pit => (1432, 530)
        case Drift => (334, 943)
        case Wade => (892, 1129)
        case Bend => (1527, 1037)
        case Ford => (2203, 1294)
        case Rock => (889, 1896)
        case Dale => (1408, 1607)

        case WinterNW => $((433, 629), (631, 927)).shuffle.head
        case Deadwood => $((913, 728), (1167, 777)).shuffle.head
        case WinterNE => (1946, 957)
        case WinterW => (532, 1312)
        case WinterSW => (748, 1592)
        case Mirkwood => (1194, 1346)
        case WinterSE => (1729, 1679)
        case WinterE => (1893, 1377)

        case Burrow(_) => (1208, 999999)
        case _ => (0, 0); throw new Error("no center for " + r)
    }

    override def gates(r : Region) = r @@ {
        case Mound => $((300, 286))
        case Trench => $((940, 324), (908, 434))
        case Pit => $((1472, 482), (1378, 576))
        case Moor => $((2116, 526))
        case Drift => $((320, 930))
        case Wade => $((836, 1040), (980, 1170), (848, 1174))
        case Bend => $((1432, 948), (1486, 1114), (1558, 996))
        case Ford => $((2220, 1328))
        case Spire => $((410, 1774), (284, 1740))
        case Rock => $((878, 1950), (916, 1844))
        case Dale => $((1372, 1558), (1438, 1658))
        case Hedge => $((2004, 1966), (2032, 1850))
        case _ => super.gates(r)
    }
}

object LakeBoard extends Board {
    val id = "lake"
    val name = "Lake"

    object Den extends Clearing(1)
    object Prairie extends Clearing(1)
    object Vert extends Clearing(2)
    object Shade extends Clearing(1)
    object Lawn extends Clearing(1)
    object Shoal extends Clearing(3)
    object Bay extends Clearing(3)
    object Yard extends Clearing(3)
    object Grove extends Clearing(1)
    object Marsh extends Clearing(3)
    object Alley extends Clearing(1)
    object Gulf extends Clearing(2)

    val clearings = $(Den, Prairie, Vert, Shade, Lawn, Shoal, Bay, Yard, Grove, Marsh, Alley, Gulf)

    val ruins = $(Marsh, Shoal, Bay, Yard)

    val diagonals = $((Den, Gulf), (Shade, Grove))

    val inner = $(Marsh, Shoal, Bay)

    override val ferry = $(Gulf)

    val coastal = $(Gulf, Marsh, Shoal, Bay)

    object LakeW extends UnnamedForest
    object LakeNW extends UnnamedForest
    object LakeN extends UnnamedForest
    object LakeNE extends UnnamedForest
    object AcresWild extends NamedForest("Acres Wild")
    object HeartOfOak extends NamedForest("Heart of Oak")
    object LakeS extends UnnamedForest
    object LakeSE extends UnnamedForest
    object LakeE extends UnnamedForest

    val forests = $(LakeW, LakeNW, LakeN, LakeNE, AcresWild, HeartOfOak, LakeS, LakeSE, LakeE)

    override def forestsConnected(o : Forest, d : Forest) : Boolean = super.forestsConnected(o, d) ||
        coastal.intersect(fromForest(o)).intersect(fromForest(d)).any

    def connected(c : Clearing) = c @@ {
        case Den => $(Prairie, Shoal, Lawn)
        case Prairie => $(Den, Shoal, Bay, Vert)
        case Vert => $(Prairie, Bay, Shade)
        case Shade => $(Vert, Yard)
        case Lawn => $(Den, Shoal, Grove)
        case Shoal => $(Lawn, Den, Prairie)
        case Bay => $(Prairie, Vert, Yard)
        case Yard => $(Shade, Bay, Gulf)
        case Grove => $(Lawn, Marsh, Alley)
        case Marsh => $(Grove, Alley)
        case Alley => $(Marsh, Gulf, Grove)
        case Gulf => $(Alley, Yard)
    }

    def fromForest(f : Forest) = f @@ {
        case LakeW => $(Den, Shoal, Lawn)
        case LakeNW => $(Den, Shoal, Prairie)
        case LakeN => $(Shoal, Prairie, Bay)
        case LakeNE => $(Vert, Prairie, Bay)
        case AcresWild => $(Vert, Shade, Yard, Bay)
        case HeartOfOak => $(Marsh, Grove, Lawn, Shoal)
        case LakeS => $(Grove, Marsh, Alley)
        case LakeSE => $(Marsh, Alley, Gulf)
        case LakeE => $(Bay, Yard, Gulf)
    }

    def byRiver(c : Clearing) = c @@ {
        case Marsh => $(Shoal, Bay, Gulf)
        case Shoal => $(Marsh, Bay, Gulf)
        case Bay => $(Marsh, Shoal, Gulf)
        case Gulf => $(Marsh, Shoal, Bay)
        case _ => $()
    }

    def center(r : Region) : (Int, Int) = r @@ {
        case Den => (360, 418)
        case Prairie => (1080, 272)
        case Vert => (1658, 502)
        case Shade => (2164, 728)
        case Lawn => (253, 1058)
        case Shoal => (769, 798)
        case Bay => (1569, 1070)
        case Yard => (2127, 1256)
        case Grove => (300, 1773)
        case Marsh => (821, 1467)
        case Alley => (1080, 1940)
        case Gulf => (2043, 1870)

        case LakeW => (418, 755)
        case LakeNW => (754, 465)
        case LakeN => (1224, 817)
        case LakeNE => (1328, 521)
        case AcresWild => (1860, 829)
        case HeartOfOak => (606, 1175)
        case LakeS => (722, 1793)
        case LakeSE => (1220, 1575)
        case LakeE => (1842, 1447)

        case Burrow(_) => (1208, 999999)
        case _ => (0, 0); throw new Error("no center for " + r)
    }

    override def port(r : Region)(flooded : $[Clearing]) = r @@ {
        case Shoal => (992, 989)
        case Bay => (1300, 1145)
        case Marsh => (1090, 1377)
        case Gulf => (1756, 1727)

        case _ => (0, 0); throw new Error("no port for " + r)
    }

    override def gates(r : Region) = r @@ {
        case Den => $((360, 455))
        case Prairie => $((1119, 282))
        case Vert => $((1711, 508), (1591, 542))
        case Shade => $((2231, 780))
        case Lawn => $((294, 1074))
        case Shoal => $((841, 806), (781, 707), (719, 834))
        case Bay => $((1633, 980), (1496, 1027), (1584, 1162))
        case Yard => $((2151, 1382), (2050, 1228), (2170, 1261))
        case Grove => $((266, 1789))
        case Marsh => $((794, 1552), (843, 1420), (912, 1536))
        case Alley => $((1114, 1914))
        case Gulf => $((2078, 1894), (1981, 1771))
        case _ => super.gates(r)
    }
}

object MountainBoard extends Board {
    val id = "mountain"
    val name = "Mountain"

    object Slope extends Clearing(2)
    object Ledge extends Clearing(3)
    object Mine extends Clearing(1)
    object Peak extends Clearing(2)
    object Brim extends Clearing(1)
    object Pass extends Clearing(2)
    object Valley extends Clearing(3)
    object Ridge extends Clearing(1)
    object Drain extends Clearing(2)
    object Ramp extends Clearing(3)
    object Cliff extends Clearing(1)
    object Crest extends Clearing(2)

    val clearings = $(Slope, Ledge, Mine, Peak, Brim, Pass, Valley, Ridge, Drain, Ramp, Cliff, Crest)

    val ruins = $(Ledge, Pass, Valley, Ramp)

    val diagonals = $((Slope, Crest), (Drain, Peak))

    val inner = $(Pass, Valley)

    // The score tracker along the bottom edge of this board's image, 8 pixels taller than most
    override val scoreTrack = |((88.3, 2130.0, 74.63))

    override val rubble = $((Brim, Ledge), (Ledge, Mine), (Mine, Peak), (Ramp, Valley), (Valley, Ridge), (Cliff, Crest))

    override val tower = $(Pass)

    object MountainW extends UnnamedForest
    object MountainNW extends UnnamedForest
    object MountainCW extends UnnamedForest
    object MountainC extends UnnamedForest
    object MountainCN extends UnnamedForest
    object MountainN extends UnnamedForest
    object MountainNE extends UnnamedForest
    object MountainSE extends UnnamedForest
    object LiarWind extends NamedForest("Liar Wind")
    object NadaMoonshine extends NamedForest("Nada Moonshine")

    val forests = $(MountainW, MountainNW, MountainCW, MountainC, MountainCN, MountainN, MountainNE, MountainSE, LiarWind, NadaMoonshine)

    def connected(c : Clearing) = c @@ {
        case Slope => $(Ledge, Brim)
        case Ledge => $(Slope, Mine, Brim, Pass, Ramp)
        case Mine => $(Ledge, Peak, Pass, Valley)
        case Peak => $(Mine, Valley, Ridge)
        case Brim => $(Slope, Ledge, Drain)
        case Pass => $(Ledge, Mine, Valley, Ramp)
        case Valley => $(Mine, Peak, Pass, Ridge, Ramp, Crest)
        case Ridge => $(Peak, Valley, Crest)
        case Drain => $(Brim, Ramp)
        case Ramp => $(Ledge, Pass, Valley, Drain, Cliff)
        case Cliff => $(Ramp, Crest)
        case Crest => $(Valley, Ridge, Cliff)
    }

    def fromForest(f : Forest) = f @@ {
        case MountainW => $(Slope, Ledge, Brim)
        case MountainNW => $(Ledge, Mine, Pass)
        case MountainCW => $(Ledge, Pass, Ramp)
        case MountainC => $(Pass, Valley, Ramp)
        case MountainCN => $(Mine, Pass, Valley)
        case MountainN => $(Mine, Peak, Valley)
        case MountainNE => $(Peak, Valley, Ridge)
        case MountainSE => $(Valley, Ridge, Crest)
        case LiarWind => $(Ledge, Brim, Drain, Ramp)
        case NadaMoonshine => $(Valley, Ramp, Cliff, Crest)
    }

    def byRiver(c : Clearing) = c @@ {
        case Peak => $(Pass)
        case Pass => $(Peak, Drain)
        case Drain => $(Pass)
        case _ => $()
    }

    def center(r : Region) : (Int, Int) = r @@ {
        case Slope => (273, 378)
        case Ledge => (713, 761)
        case Mine => (1287, 356)
        case Peak => (2001, 495)
        case Brim => (215, 1273)
        case Pass => (1188, 886)
        case Valley => (1527, 1360)
        case Ridge => (2195, 1106)
        case Drain => (357, 1838)
        case Ramp => (820, 1486)
        case Cliff => (1388, 1900)
        case Crest => (2078, 1739)

        case MountainW => (354, 814)
        case MountainNW => (1044, 636)
        case MountainCW => (888, 1098)
        case MountainC => (1190, 1216)
        case MountainCN => (1438, 1015)
        case MountainN => (1608, 638)
        case MountainNE => (1916, 946)
        case MountainSE => (1952, 1404)
        case LiarWind => (528, 1442)
        case NadaMoonshine => (1207, 1622)

        case Burrow(_) => (1208, 999999)
        case _ => (0, 0); throw new Error("no center for " + r)
    }

    override def gates(r : Region) = r @@ {
        case Slope => $((326, 394), (238, 293))
        case Ledge => $((760, 699), (733, 813), (639, 723))
        case Mine => $((1290, 349))
        case Peak => $((2082, 463), (1995, 556))
        case Brim => $((226, 1258))
        case Pass => $((1129, 795), (1219, 961))
        case Valley => $((1483, 1272), (1498, 1394), (1616, 1326))
        case Ridge => $((2185, 1091))
        case Drain => $((277, 1795),  (432, 1856))
        case Ramp => $((767, 1548), (800, 1435), (883, 1531))
        case Cliff => $((1398, 1869))
        case Crest => $((2025, 1678), (2118, 1766))
        case _ => super.gates(r)
    }
}

object TidalBoard extends Board {
    val id = "tidal"
    val name = "Tidal Flats"

    object NorthGlen extends Clearing(2) { override val name = "North Glen" }
    object OldDock extends Clearing(2) { override val name = "Old Dock" }
    object Cottage extends Clearing(1)
    object TreeHouse extends Clearing(1) { override val name = "Tree House" }
    object HouseBoat extends Clearing(3) { override val name = "House Boat" }
    object TwinPines extends Clearing(2) { override val name = "Twin Pines" }
    object Inn extends Clearing(2)
    object Stilts extends Clearing(3)
    object Wetlands extends Clearing(3)
    object Underpass extends Clearing(1)
    object Barn extends Clearing(1)
    object ShantyTown extends Clearing(2) { override val name = "Shanty Town" }
    object BigTree extends Clearing(2) { override val name = "Big Tree" }

    val clearings = $(NorthGlen, OldDock, Cottage, TreeHouse, HouseBoat, TwinPines, Inn, Stilts, Wetlands, Underpass, Barn, ShantyTown, BigTree)

    val ruins = $(OldDock, HouseBoat, Stilts, Wetlands, ShantyTown)

    val diagonals = $((NorthGlen, BigTree), (Cottage, Barn))

    val inner = $(HouseBoat, Stilts, Wetlands)

    object TidalN extends NamedForest("North")
    object TidalNW extends NamedForest("North-West")
    object TidalNE extends NamedForest("North-East")
    object TidalC extends NamedForest("Center")
    object TidalSW extends NamedForest("South-West")
    object TidalS extends NamedForest("South")
    object TidalSE extends NamedForest("South-East")

    val forests = $(TidalN, TidalNW, TidalNE, TidalC, TidalSW, TidalS, TidalSE)

    def connected(c : Clearing) = c @@ {
        case NorthGlen => $(OldDock, HouseBoat, TreeHouse)
        case OldDock => $(Cottage, NorthGlen) ++ $(TwinPines, Inn)
        case Cottage => $(TwinPines, HouseBoat, OldDock)
        case TreeHouse => $(Stilts, Inn, NorthGlen)
        case HouseBoat => $(Wetlands, Stilts, NorthGlen, Cottage)
        case TwinPines => $(Underpass, Cottage) ++ $(ShantyTown, OldDock)
        case Inn => $(Barn, TreeHouse) ++ $(OldDock, ShantyTown)
        case Stilts => $(Wetlands, Barn, TreeHouse, HouseBoat)
        case Wetlands => $(Underpass, BigTree, Stilts, HouseBoat)
        case Underpass => $(BigTree, Wetlands, TwinPines)
        case Barn => $(ShantyTown, Inn, Stilts)
        case ShantyTown => $(BigTree, Barn) ++ $(Inn, TwinPines)
        case BigTree => $(ShantyTown, Wetlands, Underpass)
    }

    def fromForest(f : Forest) = f @@ {
        case TidalN => $(Cottage, HouseBoat, NorthGlen, OldDock)
        case TidalNW => $(HouseBoat, Stilts, TreeHouse, NorthGlen)
        case TidalNE => $(TwinPines, Underpass, Wetlands, HouseBoat, Cottage)
        case TidalC => $(Wetlands, Stilts, HouseBoat)
        case TidalSW => $(Stilts, Barn, Inn, TreeHouse)
        case TidalS => $(BigTree, ShantyTown, Barn, Stilts, Wetlands)
        case TidalSE => $(Underpass, BigTree, Wetlands)
    }

    def byRiver(c : Clearing) = $

    def center(r : Region) : (Int, Int) = r @@ {
        case NorthGlen => (282, 290)
        case OldDock => (1219, 282)
        case Cottage => (2142, 334)
        case TreeHouse => (396, 835)
        case HouseBoat => (1165, 855)
        case TwinPines => (1997, 827)
        case Inn => (253, 1343)
        case Stilts => (802, 1389)
        case Wetlands => (1505, 1374)
        case Underpass => (2134, 1361)
        case Barn => (430, 1883)
        case ShantyTown => (1210, 1878)
        case BigTree => (1999, 1877)

        case TidalN => (1444, 540)
        case TidalNW => (669, 731)
        case TidalNE => (1675, 933)
        case TidalC => (1186, 1225)
        case TidalSW => (550, 1307)
        case TidalS => (1121, 1534)
        case TidalSE => (1931, 1565)

        case Burrow(_) => (1208, 999999)
        case _ => (0, 0); throw new Error("no center for " + r)
    }

    override def gates(r : Region) = r @@ {
        case NorthGlen => $((346, 236), (188, 340))
        case OldDock => $((1200, 198), (1264, 330))
        case Cottage => $((2190, 324))
        case TreeHouse => $((460, 878))
        case HouseBoat => $((1200, 720), (1074, 884), (1280, 900))
        case TwinPines => $((2106, 852), (1910, 870))
        case Inn => $((154, 1268), (342, 1382))
        case Stilts => $((890, 1456),  (822, 1264), (706, 1410))
        case Wetlands => $((1428, 1416), (1616, 1400), (1522, 1228))
        case Underpass => $((2210, 1412))
        case Barn => $((442, 1900))
        case ShantyTown => $((1196, 1726), (1262, 1922))
        case BigTree => $((2044, 1938), (1896, 1834))
        case _ => super.gates(r)
    }
}

case object Blizzard extends Effect

object TundraBoard extends Board {
    val id = "tundra"
    val name = "Tundra"

    object DeepWoods extends Clearing(2) { override val name = "Deep Woods" }
    object Hovel extends Clearing(1)
    object Seaside extends Clearing(2)
    object OldFarm extends Clearing(2) { override val name = "Old Farm" }
    object RiverBend extends Clearing(3) { override val name = "River Bend" }
    object LoneCabin extends Clearing(1) { override val name = "Lone Cabin" }
    object Mansion extends Clearing(1)
    object ColdCreek extends Clearing(2) { override val name = "Cold Creek" }
    object FrozenHollow extends Clearing(2) { override val name = "Frozen Hollow" }
    object MountainPass extends Clearing(1) { override val name = "Mountain Pass" }
    object GlacialVale extends Clearing(3) { override val name = "Glacial Vale" }
    object Cliffside extends Clearing(2)

    val clearings = $(DeepWoods, Hovel, Seaside, OldFarm, RiverBend, LoneCabin, Mansion, ColdCreek, FrozenHollow, MountainPass, GlacialVale, Cliffside)

    val ruins = $(OldFarm, RiverBend, FrozenHollow, GlacialVale)

    val diagonals = $((DeepWoods, Cliffside), (Seaside, MountainPass))

    val inner = $(OldFarm, RiverBend, ColdCreek, FrozenHollow, GlacialVale)

    override val blizzard = $((OldFarm, RiverBend), (RiverBend, FrozenHollow), (FrozenHollow, GlacialVale), (GlacialVale, ColdCreek), (ColdCreek, OldFarm), (ColdCreek, RiverBend))

    object TundraNW extends NamedForest("North-West")
    object TundraNE extends NamedForest("North-East")
    object TundraW extends NamedForest("West")
    object TundraE extends NamedForest("East")
    object TundraC extends NamedForest("Center")
    object TundraSW extends NamedForest("South-West")
    object TundraSE extends NamedForest("South-East")
    object TundraS extends NamedForest("South")

    val forests = $(TundraNW, TundraNE, TundraW, TundraE, TundraC, TundraSW, TundraSE, TundraS)

    def connected(c : Clearing) = c @@ {
        case DeepWoods => $(Hovel, OldFarm, LoneCabin)
        case Hovel => $(Seaside, OldFarm, DeepWoods)
        case Seaside => $(Mansion, RiverBend, Hovel)
        case OldFarm => $(Hovel, RiverBend, ColdCreek, DeepWoods)
        case RiverBend => $(Seaside, FrozenHollow, OldFarm)
        case LoneCabin => $(DeepWoods, ColdCreek, MountainPass)
        case Mansion => $(Seaside, Cliffside, FrozenHollow)
        case ColdCreek => $(OldFarm, GlacialVale, LoneCabin)
        case FrozenHollow => $(RiverBend, Mansion, Cliffside, GlacialVale)
        case MountainPass => $(LoneCabin, GlacialVale, Cliffside)
        case GlacialVale => $(FrozenHollow, MountainPass, ColdCreek)
        case Cliffside => $(Mansion, MountainPass, FrozenHollow)
    }

    def fromForest(f : Forest) = f @@ {
        case TundraNW => $(DeepWoods, Hovel, OldFarm)
        case TundraNE => $(Hovel, Seaside, RiverBend, OldFarm)
        case TundraW => $(DeepWoods, OldFarm, ColdCreek, LoneCabin)
        case TundraE => $(Seaside, Mansion, FrozenHollow, RiverBend)
        case TundraC => $(RiverBend, FrozenHollow, GlacialVale, ColdCreek, OldFarm)
        case TundraSW => $(LoneCabin, ColdCreek, GlacialVale, MountainPass)
        case TundraSE => $(Mansion, Cliffside, FrozenHollow)
        case TundraS => $(FrozenHollow, Cliffside, MountainPass, GlacialVale)
    }

    def byRiver(c : Clearing) = c @@ {
        case MountainPass => $(ColdCreek)
        case ColdCreek => $(MountainPass, RiverBend)
        case RiverBend => $(ColdCreek, Mansion)
        case Mansion => $(RiverBend)
        case _ => $()
    }

    def center(r : Region) : (Int, Int) = r @@ {
        case DeepWoods => (273, 387)
        case Hovel => (1158, 383)
        case Seaside => (2054, 385)
        case OldFarm => (764, 811)
        case RiverBend => (1538, 817)
        case LoneCabin => (244, 1053)
        case Mansion => (2073, 1062)
        case ColdCreek => (687, 1293)
        case FrozenHollow => (1593, 1444)
        case MountainPass => (271, 1800)
        case GlacialVale => (1044, 1623)
        case Cliffside => (2122, 1809)

        case TundraNW => (729, 502)
        case TundraNE => (1562, 499)
        case TundraW => (453, 801)
        case TundraE => (1879, 789)
        case TundraC => (1132, 949)
        case TundraSW => (397, 1414)
        case TundraS => (1498, 1777)
        case TundraSE => (1950, 1440)

        case Burrow(_) => (1208, 999999)
        case _ => (0, 0); throw new Error("no center for " + r)
    }

    override def mid(a : Region, b : Region) = (a, b) @@ {
        case (OldFarm, RiverBend) => (1123, 804)
        case (RiverBend, FrozenHollow) => (1553, 1118)
        case (FrozenHollow, GlacialVale) => (1331, 1530)
        case (GlacialVale, ColdCreek) => (850, 1441)
        case (ColdCreek, OldFarm) => (714, 1065)
        case (ColdCreek, RiverBend) => (1125, 1129)
        case _ => super.mid(a, b)
    }

    override def gates(r : Region) = r @@ {
        case DeepWoods => $((324, 298), (211, 463))
        case Hovel => $((1099, 312))
        case Seaside => $((2002, 308), (2096, 432))
        case OldFarm => $((833, 720), (703, 873))
        case RiverBend => $((1478, 716), (1632, 732), (1548, 914))
        case LoneCabin => $((246, 1040))
        case Mansion => $((2142, 1160))
        case ColdCreek => $((623, 1216), (737, 1338))
        case FrozenHollow => $((1660, 1502), (1516, 1478))
        case MountainPass => $((190, 1725))
        case GlacialVale => $((1131, 1652), (1067, 1506), (1003, 1630))
        case Cliffside => $((2181, 1857), (2087, 1733))

        case _ => super.gates(r)
    }
}

object GloomBoard extends Board {
    val id = "gloom"
    val name = "Gloom"

    object Effigy extends Clearing(2)
    object Tailbone extends Clearing(2)
    object Bog extends Clearing(2)
    object Depot extends Clearing(1)
    object Haunted extends Clearing(1)
    object Runestone extends Clearing(2)
    object Cemetery extends Clearing(1)
    object CorkIsle extends Clearing(3) { override val name = "Cork Isle" }
    object Deadwing extends Clearing(2)
    object UpperJaw extends Clearing(2) { override val name = "Upper Jaw" }
    object LowerJaw extends Clearing(2) { override val name = "Lower Jaw" }
    object Auberge extends Clearing(2)

    val clearings = $(Effigy, Tailbone, Bog, Depot, Haunted, Runestone, Cemetery, CorkIsle, Deadwing, UpperJaw, LowerJaw, Auberge)

    val ruins = $(Runestone, CorkIsle, UpperJaw, LowerJaw)

    val diagonals = $((Effigy, Auberge), (Bog, Deadwing))

    val inner = $(Haunted, Runestone, CorkIsle)

    object GloomW extends NamedForest("West")
    object GloomNW extends NamedForest("North-West")
    object GloomNE extends NamedForest("North-East")
    object GloomE extends NamedForest("East")
    object GloomCW extends NamedForest("Center-West")
    object GloomC extends NamedForest("Center")
    object GloomCE extends NamedForest("Center-East")
    object GloomSW extends NamedForest("South-West")
    object GloomSE extends NamedForest("South-East")

    val forests = $(GloomW, GloomNW, GloomNE, GloomE, GloomCW, GloomC, GloomCE, GloomSW, GloomSE)

    override def forestsConnected(o : Forest, d : Forest) : Boolean = super.forestsConnected(o, d) ||
        (o == GloomCW && d == GloomSW) || (o == GloomSW && d == GloomCW)

    def connected(c : Clearing) = c @@ {
        case Effigy => $(Tailbone, Haunted, Depot)
        case Tailbone => $(Bog, Effigy)
        case Bog => $(Cemetery, Runestone, Tailbone)
        case Depot => $(Effigy, Haunted, UpperJaw, Deadwing)
        case Haunted => $(Runestone, CorkIsle, Depot, Effigy)
        case Runestone => $(Bog, Cemetery, Haunted)
        case Cemetery => $(Bog, Auberge, CorkIsle, Runestone)
        case CorkIsle => $(Cemetery, Haunted)
        case Deadwing => $(Depot, UpperJaw)
        case UpperJaw => $(LowerJaw, Deadwing, Depot)
        case LowerJaw => $(UpperJaw, Auberge)
        case Auberge => $(Cemetery, LowerJaw)
    }

    def fromForest(f : Forest) = f @@ {
        case GloomW => $(Effigy, Haunted, Depot)
        case GloomNW => $(Tailbone, Runestone, Haunted, Effigy)
        case GloomNE => $(Bog, Runestone, Tailbone)
        case GloomE => $(Bog, Cemetery, Runestone)
        case GloomCW => $(Haunted, CorkIsle, Depot)
        case GloomC => $(Haunted, Runestone, CorkIsle)
        case GloomCE => $(Runestone, Cemetery, CorkIsle)
        case GloomSW => $(Depot, UpperJaw, Deadwing)
        case GloomSE => $(Cemetery, Auberge, LowerJaw, CorkIsle)
    }

    def byRiver(c : Clearing) = c @@ {
        case Tailbone => $(Runestone)
        case Runestone => $(Tailbone, CorkIsle)
        case CorkIsle => $(Runestone, LowerJaw, UpperJaw)
        case UpperJaw => $(CorkIsle, LowerJaw)
        case LowerJaw => $(UpperJaw, CorkIsle)
        case _ => $()
    }

    def center(r : Region) : (Int, Int) = r @@ {
        case Effigy => (428, 325)
        case Tailbone => (1369, 243)
        case Bog => (2178, 413)
        case Depot => (285, 977)
        case Haunted => (949, 724)
        case Runestone => (1549, 926)
        case Cemetery => (2118, 1201)
        case CorkIsle => (1144, 1352)
        case Deadwing => (237, 1811)
        case UpperJaw => (866, 1850)
        case LowerJaw => (1544, 1906)
        case Auberge => (2142, 1870)

        case GloomW => (547, 642)
        case GloomNW => (904, 381)
        case GloomNE => (1692, 478)
        case GloomE => (1935, 797)
        case GloomCW => (779, 1138)
        case GloomC => (1208, 980)
        case GloomCE => (1781, 1168)
        case GloomSW => (482, 1566)
        case GloomSE => (1760, 1560)

        case Burrow(_) => (1208, 999999)
        case _ => (0, 0); throw new Error("no center for " + r)
    }

    override def gates(r : Region) = r @@ {
        case Effigy => $((386, 245-5), (450, 331+5))
        case Tailbone => $((1302, 266), (1459, 184))
        case Bog => $((2149, 345), (2215, 449))
        case Depot => $((279, 955))
        case Haunted => $((956, 686))
        case Runestone => $((1588, 898), (1436, 886))
        case Cemetery => $((2124, 1173))
        case CorkIsle => $((1120, 1457), (1057, 1282), (1254, 1309))
        case Deadwing => $((210, 1748), (289, 1845))
        case UpperJaw => $((771, 1896), (765, 1786))
        case LowerJaw => $((1613, 1975), (1489, 1890))
        case Auberge => $((2189, 1794), (2099, 1925))

        case _ => super.gates(r)
    }
}

object GorgeBoard extends Board {
    val id = "gorge"
    val name = "Gorge"

    object Ranch extends Clearing(1)
    object Mesa extends Clearing(2)
    object Camp extends Clearing(1)
    object Chapel extends Clearing(1)
    object Bluff extends Clearing(1)
    object Saloon extends Clearing(2)
    object Lookout extends Clearing(2)
    object Rapids extends Clearing(2)
    object Homestead extends Clearing(2)
    object Forge extends Clearing(2)
    object Fork extends Clearing(3)
    object Pueblo extends Clearing(3)

    val clearings = $(Ranch, Mesa, Camp, Chapel, Bluff, Saloon, Lookout, Rapids, Homestead, Forge, Fork, Pueblo)

    val ruins = $(Saloon, Homestead, Fork, Pueblo)

    val diagonals = $((Ranch, Camp), (Mesa, Chapel))

    val inner = $(Fork, Pueblo)

    object GorgeNW extends NamedForest("North-West")
    object GorgeN extends NamedForest("North")
    object GorgeNE extends NamedForest("North-East")
    object GorgeW extends NamedForest("West")
    object GorgeC extends NamedForest("Center")
    object GorgeE extends NamedForest("East")
    object GorgeSW extends NamedForest("South-West")
    object GorgeS extends NamedForest("South")

    val forests = $(GorgeNW, GorgeN, GorgeNE, GorgeW, GorgeC, GorgeE, GorgeSW, GorgeS)

    // The path from Fork to Pueblo goes through the dam, crossing the dam path from Forge to Saloon,
    // so the four forests around the crossing touch the forests on either side of each half-path
    val damCrossing = $[(Forest, Forest)]((GorgeW, GorgeN), (GorgeN, GorgeE), (GorgeE, GorgeC), (GorgeC, GorgeW))

    override def forestsConnected(o : Forest, d : Forest) : Boolean = super.forestsConnected(o, d) ||
        damCrossing.has((o, d)) || damCrossing.has((d, o))

    // Forge - Saloon is the dam path, Homestead - Lookout is the bridge path
    def connected(c : Clearing) = c @@ {
        case Ranch => $(Bluff, Forge, Fork)
        case Bluff => $(Ranch, Mesa, Fork, Saloon)
        case Mesa => $(Bluff, Saloon)
        case Fork => $(Ranch, Bluff, Pueblo)
        case Forge => $(Ranch, Homestead, Saloon)
        case Saloon => $(Bluff, Mesa, Forge, Lookout)
        case Pueblo => $(Fork, Rapids, Camp)
        case Homestead => $(Forge, Chapel, Rapids, Lookout)
        case Lookout => $(Saloon, Homestead, Camp)
        case Rapids => $(Homestead, Pueblo, Chapel, Camp)
        case Chapel => $(Homestead, Rapids)
        case Camp => $(Pueblo, Lookout, Rapids)
    }

    // The dam path divides forests, the bridge path does not, and neither do the gorge sides
    def fromForest(f : Forest) = f @@ {
        case GorgeNW => $(Ranch, Bluff, Fork)
        case GorgeN => $(Bluff, Fork, Saloon)
        case GorgeNE => $(Bluff, Mesa, Saloon)
        case GorgeW => $(Ranch, Fork, Forge)
        case GorgeC => $(Forge, Pueblo, Rapids, Homestead)
        case GorgeE => $(Saloon, Lookout, Camp, Pueblo)
        case GorgeSW => $(Homestead, Chapel, Rapids)
        case GorgeS => $(Pueblo, Rapids, Camp)
    }

    def byRiver(c : Clearing) = c @@ {
        case Mesa => $(Fork)
        case Forge => $(Fork)
        case Fork => $(Mesa, Forge, Rapids)
        case Rapids => $(Fork)
        case _ => $()
    }

    def center(r : Region) : (Int, Int) = r @@ {
        case Ranch => (310, 325)
        case Bluff => (1472, 317)
        case Mesa => (2076, 378)
        case Fork => (1193, 725)
        case Forge => (476, 854)
        case Saloon => (1880, 861)
        case Homestead => (378, 1315)
        case Pueblo => (1246, 1353)
        case Lookout => (2091, 1307)
        case Rapids => (1136, 1829)
        case Chapel => (385, 1850)
        case Camp => (1721, 1870)

        case GorgeNW => (710, 370)
        case GorgeW => (390, 600)
        case GorgeNE => (1797, 499)
        case GorgeN => (1525, 710)
        case GorgeC => (846, 1572)
        case GorgeE => (1737, 1542)
        case GorgeSW => (544, 1768)
        case GorgeS => (1344, 1753)

        case Burrow(_) => (1208, 999999)
        case _ => (0, 0); throw new Error("no center for " + r)
    }

    override def mid(a : Region, b : Region) = (a, b) @@ {
        case (Homestead, Lookout) | (Lookout, Homestead) => (1057, 1526)
        case _ => super.mid(a, b)
    }

    override def gates(r : Region) = r @@ {
        case Ranch => $((302, 346))
        case Bluff => $((1466, 321))
        case Mesa => $((2015, 369), (2138, 355))
        case Fork => $((1241, 836), (1231, 659), (1128, 693))
        case Forge => $((518, 814), (435, 921))
        case Saloon => $((1822, 906), (1938, 827))
        case Homestead => $((429, 1296), (292, 1364))
        case Pueblo => $((1287, 1419), (1271, 1297), (1188, 1359))
        case Lookout => $((2131, 1263), (2045, 1349))
        case Rapids => $((1129, 1756), (1146, 1909))
        case Chapel => $((395, 1954))
        case Camp => $((1721, 1974))
        case _ => super.gates(r)
    }
}

// Root: Homeland Marsh map. The printed board has no clearing names.
// Numbers in comments are the ones on the Law of Root map diagram.
object MarshBoard extends Board {
    val id = "marsh"
    val name = "Marsh"

    object Thatch extends Clearing(1)   // 1
    object Jetty extends Clearing(2)    // 2
    object Bayou extends Clearing(1)    // 3
    object Delta extends Clearing(2)    // 4
    object Sedge extends Clearing(1)    // 5
    object Weir extends Clearing(2)     // 6, dark green
    object Heron extends Clearing(1)    // 7
    object Mudflat extends Clearing(2)  // 8, dark green
    object Cattail extends Clearing(2)  // 9
    object Bulrush extends Clearing(1)  // 10, brown
    object Peat extends Clearing(1)     // 11, brown
    object Mire extends Clearing(2)     // 12, light green
    object Fen extends Clearing(2)      // 13, light green
    object Confluence extends Clearing(3)     // 14
    object Crossing extends Clearing(3) // 15

    val clearings = $(Thatch, Jetty, Bayou, Delta, Sedge, Weir, Heron, Mudflat, Cattail, Bulrush, Peat, Mire, Fen, Confluence, Crossing)

    // "R" slots: two without numbers always get ruins, then the two lowest numbers not flooded (R1 Mire, R2 Fen, R3 Weir, R4 Mudflat)
    val ruins = $(Crossing, Confluence, Mire, Fen, Weir, Mudflat)

    override def ruinsIn(l : $[Clearing]) = $(Crossing, Confluence).intersect(l) ++ $(Mire, Fen, Weir, Mudflat).intersect(l).take(2)

    val diagonals = $((Thatch, Bayou), (Jetty, Delta))

    val inner = $(Peat, Crossing, Mire, Confluence, Fen)

    // Where the item tracker (top left, as on the printed board) and the score tracker
    // (along the bottom edge, below the Delta and Bayou names) are drawn
    override def itemSlots = itemGrid(423.5, 87.4, 90.0, 173.5)

    override val scoreTrack = |((79.9, 2201.0, 74.63))

    override val floodPairs = $((Weir, Mudflat), (Mire, Fen), (Bulrush, Peat))

    override def floodPaths(c : Clearing) = c @@ {
        case Weir => $((Sedge, Jetty))
        case Mudflat => $((Cattail, Bayou))
        case Mire => $((Crossing, Jetty), (Crossing, Fen), (Jetty, Fen))
        case Fen => $((Confluence, Mire), (Confluence, Bayou), (Mire, Bayou))
        case Bulrush => $((Thatch, Delta))
        case Peat => $((Thatch, Confluence))
        case _ => $()
    }

    object MarshNW extends UnnamedForest
    object MarshW extends UnnamedForest
    object MarshN extends UnnamedForest
    object MarshC extends UnnamedForest
    object MarshE extends UnnamedForest
    object MarshSW extends UnnamedForest
    object MarshS extends UnnamedForest

    val forests = $(MarshNW, MarshW, MarshN, MarshC, MarshE, MarshSW, MarshS)

    def connected(c : Clearing) = c @@ {
        case Thatch => $(Sedge, Peat, Bulrush)
        case Jetty => $(Weir, Mire, Heron)
        case Bayou => $(Heron, Fen, Mudflat)
        case Delta => $(Bulrush, Cattail)
        case Sedge => $(Thatch, Weir, Crossing)
        case Weir => $(Sedge, Jetty)
        case Heron => $(Jetty, Bayou)
        case Mudflat => $(Cattail, Bayou)
        case Cattail => $(Confluence, Delta, Mudflat)
        case Bulrush => $(Thatch, Confluence, Delta)
        case Peat => $(Thatch, Crossing, Confluence)
        case Mire => $(Jetty, Crossing, Fen)
        case Fen => $(Mire, Confluence, Bayou)
        case Confluence => $(Peat, Bulrush, Fen, Cattail)
        case Crossing => $(Sedge, Peat, Mire)
    }

    // Flooded paths divide forests like paths, so forests keep their shapes when a clearing floods
    def fromForest(f : Forest) = f @@ {
        case MarshNW => $(Thatch, Sedge, Crossing, Peat)
        case MarshW => $(Thatch, Peat, Confluence, Bulrush)
        case MarshN => $(Sedge, Weir, Jetty, Mire, Crossing)
        case MarshC => $(Peat, Crossing, Mire, Fen, Confluence)
        case MarshE => $(Jetty, Heron, Bayou, Fen, Mire)
        case MarshSW => $(Bulrush, Confluence, Cattail, Delta)
        case MarshS => $(Confluence, Fen, Bayou, Mudflat, Cattail)
    }

    def byRiver(c : Clearing) = c @@ {
        case Weir => $(Crossing)
        case Crossing => $(Weir, Confluence)
        case Confluence => $(Crossing, Delta, Mudflat)
        case Delta => $(Confluence)
        case Mudflat => $(Confluence)
        case _ => $()
    }

    def center(r : Region) : (Int, Int) = r @@ {
        case Thatch => (322, 322)
        case Jetty => (2190, 420)
        case Bayou => (2030, 1890)
        case Delta => (195, 1882)
        case Sedge => (1020, 248)
        case Weir => (1560, 297)
        case Heron => (2220, 1418)
        case Mudflat => (1239, 1897)
        case Cattail => (840, 1740)
        case Bulrush => (207, 1227)
        case Peat => (658, 751)
        case Mire => (1950, 920)
        case Fen => (1540, 1432)
        case Confluence => (930, 1140)
        case Crossing => (1305, 818)

        case MarshNW => (705, 465)
        case MarshW => (435, 900)
        case MarshN => (1725, 630)
        case MarshC => (1320, 1140)
        case MarshE => (1950, 1320)
        case MarshSW => (495, 1500)
        case MarshS => (1350, 1620)

        case Burrow(_) => (1208, 999999)
        case _ => (0, 0); throw new Error("no center for " + r)
    }

    // Ruins slots first, ruins take the first free slot
    override def gates(r : Region) = r @@ {
        case Thatch => $((294, 273))
        case Jetty => $((2145, 378), (2242, 428))
        case Bayou => $((2018, 1974))
        case Delta => $((132, 1920), (258, 1852))
        case Sedge => $((1022, 210))
        case Weir => $((1580, 368), (1512, 292))
        case Heron => $((2246, 1413))
        case Mudflat => $((1218, 2010), (1192, 1924))
        case Cattail => $((818, 1680), (852, 1792))
        case Bulrush => $((180, 1230))
        case Peat => $((668, 693))
        case Mire => $((1875, 915), (2010, 862))
        case Fen => $((1490, 1410), (1582, 1462))
        case Confluence => $((1005, 1208), (858, 1065), (972, 1118))
        case Crossing => $((1407, 758), (1205, 788), (1372, 888))
        case _ => super.gates(r)
    }
}

case class ClearPathMainAction(self : Faction, then : ForcedAction) extends BaseAction(None)(ClearPath) with Soft
case class ClearPathBetweenAction(self : Faction, a : Clearing, b : Clearing, then : ForcedAction) extends BaseAction(ClearPath)("Clear path between", a, "and", b) with Soft
case class ClearPathAction(self : Faction, a : Clearing, b : Clearing, then : ForcedAction) extends ForcedAction

case object ClearPath extends DisplayEffect {
    override val name = "Clear Path"
}

// Homeland landmarks
case object RabbittownMuster extends DisplayEffect {
    override val name = "Rabbittown"
}

case class ToRabbittown(c : Clearing) extends Message {
    def elem(implicit game : Game) = "to place warriors in " ~ c.elem
}

case class RabbittownMainAction(self : WarriorFaction, c : Clearing, n : Int, then : ForcedAction) extends BaseAction(None)("Rabbittown".styled(Rabbit), "(" ~ n.of("warrior") ~ ")") with Soft
case class RabbittownAction(self : WarriorFaction, c : Clearing, then : ForcedAction) extends ForcedAction
case class MouseholdAction(b : Battle) extends ForcedAction
case class MouseholdPlaceAction(self : Faction, c : Clearing, l : $[Piece], then : ForcedAction) extends BaseAction(self, "can place warriors removed in battle at", "Mousehold".styled(Mouse))(l.none.?("Skip").|("Place " ~ l./(_.of(self)).comma))

object MapsExpansion extends MandatoryExpansion {
    def rabbittownWarriors(f : Faction, c : Clearing)(implicit game : Game) = clearings.but(c).%(x => game.mapping.get(x).?(_.has(Rabbit))).%(x => f.at(x).any).num

    override def daylight(f : Faction)(implicit game : Game, ask : ActionCollector) {
        if (game.rubble.any && f.used.has(ClearPath).not)
            + ClearPathMainAction(f, Repeat).!(f.hand.none, "no cards").!(game.rubble.forall { case (a, b) => f.at(a).none && f.at(b).none })

        f.as[WarriorFaction].foreach { f =>
            if (f.is[Hero].not && f.used.has(RabbittownMuster).not)
                game.rabbittown.foreach { c =>
                    val n = rabbittownWarriors(f, c)

                    + RabbittownMainAction(f, c, n, Repeat)
                        .!(f.hand.none, "no cards")
                        .!(n == 0, "no other rabbit clearings")
                        .!(f.pooled(f.warrior) == 0, "no warriors")
                        .!(f.canPlace(c).not, "can't place")
                }
        }
    }

    def perform(action : Action, soft : Void)(implicit game : Game) = action @@ {
        case ClearPathMainAction(f, then) =>
            Ask(f).each(game.rubble){ case (a, b) => ClearPathBetweenAction(f, a, b, then).!(f.at(a).none && f.at(b).none, "no presence") }.cancel

        case ClearPathBetweenAction(f, a, b, then) =>
            OptionalDiscardCardAction(f, ToClearPath(a, b), AnySuit, ClearPathAction(f, a, b, then))

        case RabbittownMainAction(f, c, _, then) =>
            OptionalDiscardCardAction(f, ToRabbittown(c), Rabbit, RabbittownAction(f, c, then))

        case RabbittownAction(f, c, then) =>
            f.drawn --> discard.quiet

            f.used :+= RabbittownMuster

            val n = min(rabbittownWarriors(f, c), f.pooled(f.warrior))

            1.to(n).foreach(_ => f.reserve --> f.warrior --> c)

            f.log("placed", n.times(f.warrior)./(_.of(f)).comma, "in", c, "with", RabbittownMuster)

            if (f.canAttackIn(c))
                BattleInitAction(f, f, NoMessage, $(c), $, then)
            else
                then

        // Mousehold: removed warriors may be placed there, one faction at a time
        case MouseholdAction(b) =>
            val c = game.mousehold.head

            val ff = game.mouseholdPending./(_.faction).distinct.%(_.canPlace(c))

            if (ff.none) {
                game.mouseholdPending = $

                BattleFinishedAction(b)
            }
            else {
                val f = ff.head
                val pp = game.mouseholdPending.%(_.faction == f)./(_.piece)
                game.mouseholdPending = game.mouseholdPending.%(_.faction != f)

                val l = pp.distinct./~(p => pp.%(_ == p).take(f.pooled(p)))

                if (l.any)
                    Ask(f).each(l.num.to(1, -1).$)(n => MouseholdPlaceAction(f, c, l.take(n), MouseholdAction(b))).add(MouseholdPlaceAction(f, c, $, MouseholdAction(b)))
                else
                    MouseholdAction(b)
            }

        case MouseholdPlaceAction(f, c, l, then) =>
            l.foreach(p => f.reserve --> p --> c)

            if (l.any)
                f.log("placed", l./(_.of(f)).comma, "at", "Mousehold".styled(Mouse), "in", c)

            then

        case ClearPathAction(f, a, b, then) =>
            f.nscore(1)("clearing path")(f, "cleared path between", a, "and", b, "with", f.drawn.get, ForVP)
            f.drawn --> discard.quiet
            game.rubble :-= (a, b)
            f.used :+= ClearPath
            then

        case _ => UnknownContinue
    }
}
