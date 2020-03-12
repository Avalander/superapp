package app.superfine

import org.scalajs.dom.Event

object Html {
  val div = hh("div", _: Props, _: NodeContent)
  val button = hh("button", _: Props, _: NodeContent)
  val h1 = hh("h1", _: Props, _: NodeContent)
}

trait Assignable[T] {
  def apply (value: T): Prop = ???
  def := (value: T): Prop = apply(value)
}

object Attributes {
  abstract class StringAssignable (name: String) extends Assignable[String] {
    override def apply(value: String): Prop = StringProp(name, value)
  }

  object accept extends StringAssignable("accept")
  object acceptCharset extends StringAssignable("accept-charset")
  object accessKey extends StringAssignable("accesskey")
  object action extends StringAssignable("action")
  object alt extends StringAssignable("alt")
  object autocomplete extends StringAssignable("autocomplete")
  object `class` extends StringAssignable("class")
  object `type` extends StringAssignable("type")
}

object EventHandlers {
  type Fn = Event => Unit

  abstract class EventAssignable (name: String) extends Assignable[Fn] {
    override def apply(value: Fn): Prop = EventProp(name, value)
  }

  object onClick extends EventAssignable("onclick")
  object onInput extends EventAssignable("oninput")
}
