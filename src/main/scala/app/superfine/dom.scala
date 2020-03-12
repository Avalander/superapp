package app.superfine

import Superfine.h

object dom {
  private def vnode(tag: String): (Superfine.Props, Superfine.Children) => VNode =
    h(tag, _: Superfine.Props, _: Superfine.Children)
  val div = vnode("div")
  val h1 = vnode("h1")
  val h2 = vnode("h2")
  val h3 = vnode("h3")
  val h4 = vnode("h4")
  val h5 = vnode("h5")
  val h6 = vnode("h6")
  val button = vnode("button")
  val p = vnode("p")
}
