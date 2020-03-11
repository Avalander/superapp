package app.superfine

import Superfine.h
import scalajs.js

package object svg {
  private def vnode(tag: String): (Props, Seq[VNode]) => VNode = {
    def inner(p: Props, c: Seq[VNode] = Nil): VNode = {
      val children = js.Array[VNode]()
      c foreach (children.push(_))
      h(tag, p, children)
    }
    inner
  }

  val svg = vnode("svg")
  val g = vnode("g")
  val rect = vnode("rect")
  def text (p: Props, text: String): VNode = {
    h("text", p, text)
  }
}
