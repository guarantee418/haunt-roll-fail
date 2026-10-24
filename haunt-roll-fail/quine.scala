package hrf.quine
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

import hrf._
import hrf.loader._
import hrf.meta._
import hrf.html._
import hrf.web._

import scalajs.js

import org.scalajs.dom

import scala.collection.decorators._


object Quine {
    def openTag(name : String) = "<!-- { | " + name + " | { -->"
    def closeTag(name : String) = "<!-- } | " + name + " | } -->"

    def replaceSections(replace : $[(String, $[String])])(s : String) : $[String] = replace match {
        case Nil => $(s)
        case (name, value) :: rest =>
            val a = openTag(name)
            val b = closeTag(name)

            val l1 = s.splt(a)

            val l2 = l1.take(1)./(_.splt(b) match {
                case List(x) => x
                case _ => throw new Error("orphaned closing tag " + b)
            }) ++ l1.drop(1)./(_.splt(b) match {
                case List(_, y) => y
                case List(_) => throw new Error("missing closing tag " + b)
                case _ => throw new Error("extra closing tag " + b)
            })

            val l3 = l2./(replaceSections(rest)).intersperse($(a) ++ value ++ $(b)).flatten

            l3
    }

    def replaceUrls(substitutions : $[(String, String)])(l : $[String]) : $[String] = l./{ s =>
        substitutions.foldLeft(s) { case (s, (url, data)) =>
            val a = openTag(url)
            val b = closeTag(url)

            val l1 : $[String] = s.splt(a)

            val l2 : $[String] = l1.take(1)./(_.splt(b) match {
                case List(x) => x
                case _ => throw new Error("orphaned closing tag " + b)
            }) ++ l1.drop(1)./(_.splt(b) match {
                case List(_, y) => y
                case List(_) => throw new Error("missing closing tag " + b)
                case Nil => throw new Error("extra closing tag " + b)
            })

            val l3 : $[String] = l2.intersperse(data)

            l3.join("")
        }
    }

    // A small replay: the game's lobby lines and moves, deflated and put after "#small-replay=" in a link to this
    // site's page for the game, in an .html file that only opens that link. A few dozen KB instead of the tens of MB of
    // the full replay (client and images inlined), so it can be shared on Discord; it needs the site to play.
    // The part after "#" never reaches the server, so the server serves the page as usual
    val smallReplayKey = "small-replay"

    private def deflate(s : String, compress : Boolean)(then : Array[Byte] => Unit)(fail : String => Unit) {
        import scalajs.js.typedarray._
        import scalajs.js.JSConverters._

        val kind = compress.?("CompressionStream").|("DecompressionStream")
        // Through window, as a missing global name would throw instead of being undefined
        val window = dom.window.asInstanceOf[js.Dynamic]
        val stage = compress.?(window.CompressionStream).|(window.DecompressionStream)

        if (js.isUndefined(stage))
            fail("This browser can't " + compress.?("compress").|("decompress") + " replays (" + kind + " is missing).")
        else {
            val bytes = compress.?(s.getBytes("UTF-8")).|(java.util.Base64.getUrlDecoder().decode(s))
            val blob = new dom.Blob(js.Array(bytes.toTypedArray), new dom.BlobPropertyBag { `type` = "application/octet-stream" })
            val stream = blob.asInstanceOf[js.Dynamic].stream().pipeThrough(js.Dynamic.newInstance(stage)("deflate-raw"))

            js.Dynamic.newInstance(window.Response)(stream).arrayBuffer().`then`({ (b : ArrayBuffer) =>
                then(int8Array2ByteArray(new Int8Array(b)))
            } : js.Function1[ArrayBuffer, Unit], { (e : js.Any) =>
                fail("The replay couldn't be read: " + e)
            } : js.Function1[js.Any, Unit])
        }
    }

    def lobbyLines(meta : MetaGame)(title : String, seating : $[meta.F], options : $[meta.O], resources : Resources) : $[String] =
        $("meta " + meta.name, "version " + meta.gaming.version, "title " + title) ++
            seating./(f => "user " + meta.writeFaction(f) + " player-" + meta.writeFaction(f).toLowerCase) ++
            seating.%(f => resources.getName(f).any)./(f => "name player-" + meta.writeFaction(f).toLowerCase + " " + resources.getName(f).get) ++
            $("seating " + seating./(meta.writeFaction).join(" ")) ++
            $("options " + options./(meta.writeOption).join(" "))

    def saveSmall(meta : MetaGame)(title : String, seating : $[meta.F], options : $[meta.O], resources : Resources, journal : Journal[meta.gaming.ExternalAction], filename : String, onSave : => Unit) {
        journal.read(0) { actions =>
            // A blank line between the lobby lines and the moves, neither of which has blank lines
            val text = lobbyLines(meta)(title, seating, options, resources).join("\n") + "\n\n" + actions./(meta.writeActionExternal).join("\n")

            deflate(text, true) { bytes =>
                val url = dom.window.location.origin + "/play/" + meta.name + "#" + smallReplayKey + "=" + java.util.Base64.getUrlEncoder().withoutPadding().encodeToString(bytes)

                def escape(s : String) = s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace("\"", "&quot;")

                val result =
                    "<!doctype html>\n<html>\n<head>\n<meta charset=\"utf-8\">\n<title>" + escape(title + " | " + meta.label) + " | Replay</title>\n</head>\n<body>\n" +
                    "<p>Opening the replay of " + escape(title) + " on <a id=\"replay\" href=\"" + escape(url) + "\">" + escape(dom.window.location.host) + "</a>...</p>\n" +
                    "<script>window.location.replace(document.getElementById(\"replay\").href)</script>\n</body>\n</html>\n"

                download(result, filename)
                onSave
            } { error =>
                dom.window.alert(error)
                onSave
            }
        }
    }

    // Opens a small replay from the page's link: fills the lobby and replay elements and the settings the way a
    // full replay file has them, then goes on
    def openSmall(data : String)(then : => Unit) {
        deflate(data, false) { bytes =>
            val text = new String(bytes, "UTF-8")
            val (lobby, story) = text.indexOf("\n\n") match {
                case -1 => (text, "")
                case i => (text.take(i), text.drop(i + 2))
            }

            val div = dom.document.createElement("div").asInstanceOf[dom.html.Div]
            div.id = "lobby"
            div.style.display = "none"
            div.textContent = lobby
            dom.document.body.appendChild(div)

            getElem("replay").textContent = story

            val settings = getElem("settings")
            settings.setAttribute("data-replay", "true")
            lobby.split('\n').$.%(_.startsWith("meta "))./(_.drop(5).trim).foreach(m => settings.setAttribute("data-meta", m))

            then
        } { error =>
            dom.window.alert(error)
            then
        }
    }

    private def download(result : String, filename : String) {
        val blob = {
            import scalajs.js.typedarray._
            import scalajs.js.JSConverters._

            new dom.Blob(js.Array(result.getBytes().toTypedArray), new dom.BlobPropertyBag { `type` = "text/html" })
        }

        val link = dom.document.createElement("a").asInstanceOf[dom.html.Anchor]
        link.href = dom.URL.createObjectURL(blob)
        link.asInstanceOf[js.Dynamic].download = filename.replace(" ", "_").replace("/", "-").replace(":", "-") + ".html"
        dom.document.body.appendChild(link)
        link.click()
    }

    def save(meta : MetaGame)(title : String, seating : $[meta.F], options : $[meta.O], resources : Resources, journal : Journal[meta.gaming.ExternalAction], filename : String, replay : Boolean, server : String, onSave : => Unit) {
        val tab = "\n        "

        if (HRF.embedded) {
            val html : String = HRF.originalOuterHtml

            journal.read(0) { actions =>
                val story = actions./(meta.writeActionExternal)
                val lobby = lobbyLines(meta)(title, seating, options, resources)

                val result = replaceSections($(
                    "settings".toUpperCase -> $(tab, "<title id=\"settings\" data-embedded-assets=\"true\" data-server=\"" + server + "\" data-replay=\"", replay.??("true"), "\" data-meta=\"", meta.name, "\" >", title, " | ", meta.label, "</title>", tab),
                    "replay".toUpperCase -> replay.??($(tab, "<div id=\"lobby\" style=\"display: none\" >") ++ lobby./~($(tab, "    ", _)) ++ $(tab, "</div>", tab, "<div id=\"replay\" style=\"display: none\" >") ++ story./~($(tab, "    ", _)) ++ $(tab, "</div>", tab)),
                ))(html).join("")

                val blob = {
                    import scalajs.js.typedarray._
                    import scalajs.js.JSConverters._

                    new dom.Blob(js.Array(result.getBytes().toTypedArray), new dom.BlobPropertyBag { `type` = "text/html" })
                }

                val link = dom.document.createElement("a").asInstanceOf[dom.html.Anchor]
                link.href = dom.URL.createObjectURL(blob)
                link.asInstanceOf[js.Dynamic].download = filename.replace(" ", "_").replace("/", "-").replace(":", "-") + ".html"
                dom.document.body.appendChild(link)
                link.click()
                onSave
            }
        }
        else
        HRF.stringLoader.wait($(HRF.html, HRF.script)) {
            val html = HRF.stringLoader.get(HRF.html).splt("\n")
            val script = HRF.stringLoader.get(HRF.script)

            val urls = html.%(_.trim.startsWith("url(\""))./~(_.splt("\"").lift(1))

            val nhtml = html./(s => s.trim.startsWith("url(\"").?(s.splt("\"").use(l => l(0) + openTag("URL " + l(1)) + l(1) + closeTag("URL " + l(1)) + l.drop(2).join("\""))).|(s))

            val loader = HRF.embedded.?(new EmbeddedImageSourceLoader(s => "asset-" + s)).|(DataUrlLoader)

            loader.wait(urls ++ resources.images.sources.values.$) {
                journal.read(0) { actions =>
                    val self = $(tab, """<!-- "/> --><img style="display: none" id="html-quine" data-src="""", java.util.Base64.getEncoder().encodeToString(HRF.stringLoader.get(HRF.html).getBytes), """" />""")
                    val assets = resources.images.sources.keys.$./~(k => $(tab, """<!-- "/> --><img style="display: none" id="asset-""", k, """" src="""", loader.get(resources.images.sources(k)), """" />"""))

                    val story = actions./(meta.writeActionExternal)
                    val lobby = lobbyLines(meta)(title, seating, options, resources)

                    val uhtml = replaceUrls(urls./(u => ("URL " + u) -> DataUrlLoader.get(u)))(nhtml)

                    val result = replaceSections($(
                        "base href".toUpperCase -> $(tab),
                        "settings".toUpperCase -> $(tab, "<title id=\"settings\" data-embedded-assets=\"true\" data-server=\"" + server + "\" data-replay=\"", replay.??("true"), "\" data-meta=\"", meta.name, "\" >", title, " | ", meta.label, "</title>", tab),
                        "script".toUpperCase -> $(tab, "<script id=\"script\" type=\"text/javascript\" >\n", script, tab, "</scr", "ipt>", tab),
                        "replay".toUpperCase -> replay.??($(tab, "<div id=\"lobby\" style=\"display: none\" >") ++ lobby./~($(tab, "    ", _)) ++ $(tab, "</div>", tab, "<div id=\"replay\" style=\"display: none\" >") ++ story./~($(tab, "    ", _)) ++ $(tab, "</div>", tab)),
                        "assets".toUpperCase -> (self ++ assets ++ $(tab)),
                        "cache invalidation".toUpperCase -> $(tab)
                    ))(uhtml.join("\n")).join("")

                    val blob = {
                        import scalajs.js.typedarray._
                        import scalajs.js.JSConverters._

                        new dom.Blob(js.Array(result.getBytes().toTypedArray), new dom.BlobPropertyBag { `type` = "text/html" })
                    }

                    val link = dom.document.createElement("a").asInstanceOf[dom.html.Anchor]
                    link.href = dom.URL.createObjectURL(blob)
                    link.asInstanceOf[js.Dynamic].download = filename.replace(" ", "_").replace("/", "-").replace(":", "-") + ".html"
                    dom.document.body.appendChild(link)
                    link.click()
                    onSave
                }
            }
        }
    }

}
