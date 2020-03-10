package app

import scala.scalajs.js
import scala.scalajs.js.|
import scala.scalajs.js.annotation.JSImport

import org.scalajs.dom.{Element, Event}

package object superfine {
  type Props = js.Object
  type Children = js.UndefOr[String]|js.Array[js.Object]
  type VNode = js.Object

  @js.native
  @JSImport("superfine", JSImport.Namespace)
  object Superfine extends js.Object {
    def patch(node: Element, view: VNode): Element = js.native
    def h(
      tag: String,
      props: Props,
      children: Children = js.undefined
    ): VNode = js.native
  }
}
