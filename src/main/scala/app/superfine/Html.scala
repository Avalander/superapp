package app.superfine

import org.scalajs.dom.Event

object Html {
  val div = hh("div", _: Props, _: NodeContent)
  val button = hh("button", _: Props, _: NodeContent)
  val h1 = hh("h1", _: Props, _: NodeContent)
}

object Attributes {
  def `class` (value: String): Prop = StringProp("class", value)
}

object EventHandlers {
  def onClick (fn: Event => Unit): Prop = EventProp("onclick", fn)
}
