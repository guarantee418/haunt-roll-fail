/** All documentation for facades is thanks to Mozilla Contributors at https://developer.mozilla.org/en-US/docs/Web/API
  * and available under the Creative Commons Attribution-ShareAlike v2.5 or later.
  * http://creativecommons.org/licenses/by-sa/2.5/
  *
  * Everything else is under the MIT License http://opensource.org/licenses/MIT
  */
package org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.annotation._

/** The SVGAnimatedBoolean interface is used for attributes of type boolean which can be animated. */
@js.native
@JSGlobal
class SVGAnimatedBoolean extends js.Object {

  /** If the given attribute or property is being animated, contains the current animated value of the attribute or
    * property. If the given attribute or property is not currently being animated, contains the same value as baseVal.
    */
  def animVal: Boolean = js.native

  /** The base value of the given attribute before applying any animations. */
  var baseVal: Boolean = js.native
}
