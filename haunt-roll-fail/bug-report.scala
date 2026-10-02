package hrf
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

import org.scalajs.dom
import scalajs.js

// "Report a Bug" dialog: the player describes the problem, the client adds
// the game details and the console errors (collected by the "console-capture"
// script in index.html), and the server posts it all as a GitHub issue
object BugReport {
    def consoleLog() : $[String] = {
        val log = js.Dynamic.global.hrfConsoleLog

        if (js.isUndefined(log) || log == null)
            $
        else
            log.asInstanceOf[js.Array[String]].toList
    }

    private def el[T <: dom.html.Element](tag : String, style : String) : T = {
        val e = dom.document.createElement(tag).asInstanceOf[T]
        e.setAttribute("style", style)
        e
    }

    private val buttonStyle = "font: inherit; font-size: 2.2vh; margin: 1vh 1vh 0 0; padding: 0.6vh 2vh; cursor: pointer; border-radius: 0.6vh; border: 1px solid #707070; background: #303030; color: #e0e0e0;"

    // details: the game information to put above the description, as markdown
    def show(server : String, details : () => String, onClose : () => Unit) {
        val overlay = el[dom.html.Div]("div", "position: fixed; left: 0; top: 0; width: 100%; height: 100%; z-index: 100000; background: rgba(0, 0, 0, 0.75); display: flex; align-items: center; justify-content: center;")
        val box = el[dom.html.Div]("div", "box-sizing: border-box; width: 90vw; max-width: 90vh; max-height: 90vh; overflow: auto; padding: 2vh; background: #181818; color: #c0c0c0; border: 1px solid #707070; border-radius: 1vh; font-size: 2.2vh; font-family: system-ui, sans-serif;")

        val title = el[dom.html.Div]("div", "font-size: 2.8vh; color: #ffffff; margin-bottom: 1vh;")
        title.textContent = "Report a Bug"

        val help = el[dom.html.Div]("div", "margin-bottom: 1vh;")
        help.textContent = "What did you do, and what went wrong? The game, the factions, the moves so far and any console errors are sent with the report."

        val summary = el[dom.html.Input]("input", "box-sizing: border-box; width: 100%; font: inherit; padding: 0.6vh; margin-bottom: 1vh; background: #000000; color: #e0e0e0; border: 1px solid #707070;")
        summary.placeholder = "Short summary, e.g. Council can't recruit in a ruled clearing"
        summary.maxLength = 120

        val text = el[dom.html.TextArea]("textarea", "box-sizing: border-box; width: 100%; height: 25vh; font: inherit; padding: 0.6vh; background: #000000; color: #e0e0e0; border: 1px solid #707070; resize: vertical;")
        text.placeholder = "What you did, what happened, and what you expected to happen"

        val status = el[dom.html.Div]("div", "margin-top: 1vh; min-height: 2.5vh; white-space: pre-wrap; word-break: break-all;")

        val send = el[dom.html.Button]("button", buttonStyle)
        send.textContent = "Send"

        val cancel = el[dom.html.Button]("button", buttonStyle)
        cancel.textContent = "Cancel"

        def close() {
            overlay.remove()
            onClose()
        }

        // keep typing from reaching the game's keyboard shortcuts
        overlay.addEventListener("keyup", (e : dom.KeyboardEvent) => e.stopPropagation())
        overlay.addEventListener("keydown", (e : dom.KeyboardEvent) => e.stopPropagation())

        cancel.onclick = (e : dom.MouseEvent) => close()

        send.onclick = (e : dom.MouseEvent) => {
            val description = text.value.trim

            if (description == "") {
                status.textContent = "Describe the problem first."
            }
            else {
                send.disabled = true
                status.textContent = "Sending..."

                val errors = consoleLog()

                val short = summary.value.trim
                val heading = (short != "").?(short).|(description.split('\n').head.take(80))

                val body =
                    "### What happened\n\n" + description + "\n\n" +
                    details() + "\n\n" +
                    "### Console\n\n" + errors.none.?("No errors or warnings.").|("```\n" + errors.takeRight(50).join("\n").take(20000) + "\n```") + "\n\n" +
                    "### Browser\n\n" + dom.window.navigator.userAgent + ", window " + dom.window.innerWidth + "x" + dom.window.innerHeight + "\n"

                val xhr = new dom.XMLHttpRequest()

                xhr.onload = (e : dom.Event) => {
                    if (xhr.status < 400) {
                        val url = xhr.responseText.trim

                        status.innerHTML = ""
                        status.appendChild(dom.document.createTextNode("Thank you! The report was sent" + url.startsWith("http").??(":") + "\n"))

                        if (url.startsWith("http")) {
                            val a = el[dom.html.Anchor]("a", "color: #80c0ff;")
                            a.href = url
                            a.target = "_blank"
                            a.textContent = url
                            status.appendChild(a)
                            status.appendChild(dom.document.createTextNode("\nOpen it to add a screenshot."))
                        }

                        send.style.display = "none"
                        cancel.textContent = "Close"
                    }
                    else {
                        send.disabled = false
                        status.textContent = "Sending failed (" + xhr.status + "): " + xhr.responseText.take(300)
                    }
                }

                xhr.onerror = (e : dom.ProgressEvent) => {
                    send.disabled = false
                    status.textContent = "Sending failed: no connection to the server."
                }

                xhr.open("POST", server + "/report-bug", true)
                xhr.send(heading + "\n" + body)
            }
        }

        box.appendChild(title)
        box.appendChild(help)
        box.appendChild(summary)
        box.appendChild(text)
        box.appendChild(send)
        box.appendChild(cancel)
        box.appendChild(status)
        overlay.appendChild(box)
        dom.document.body.appendChild(overlay)

        summary.focus()
    }
}
