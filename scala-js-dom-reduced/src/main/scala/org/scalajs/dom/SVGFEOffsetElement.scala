/** All documentation for facades is thanks to Mozilla Contributors at https://developer.mozilla.org/en-US/docs/Web/API
  * and available under the Creative Commons Attribution-ShareAlike v2.5 or later.
  * http://creativecommons.org/licenses/by-sa/2.5/
  *
  * Everything else is under the MIT License http://opensource.org/licenses/MIT
  */
package org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation._

@js.native
@JSGlobal
abstract class SVGFEOffsetElement extends SVGElement with SVGFilterPrimitiveStandardAttributes {
  def dy: SVGAnimatedNumber = js.native

  def in1: SVGAnimatedString = js.native

  def dx: SVGAnimatedNumber = js.native
}
