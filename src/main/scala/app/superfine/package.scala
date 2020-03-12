package app

import scala.scalajs.js
import scala.scalajs.js.|
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.JSConverters._

import org.scalajs.dom.{Element, Event}

package object superfine {
  type VNode = js.Object

  sealed trait Prop
  case class StringProp(key: String, value: String) extends Prop
  case class IntProp(key: String, value: Int) extends Prop
  case class EventProp(key: String, value: Function1[Event, Unit]) extends Prop

  type Props = Seq[Prop]
  object Props {
    def apply (props: Prop*) = props.toList
  }

  sealed trait NodeContent
  case class TextContent(text: String) extends NodeContent
  case class Children(nodes: List[VNode]) extends NodeContent
  object Children {
    def apply (nodes: VNode*): Children = new Children(nodes.toList)
  }

  def hh (tag: String, props: Props, content: NodeContent): VNode = {
    val _props = (props map {
      case EventProp(key, value)  => (key -> value): (String, js.Function1[Event, Unit])
      case StringProp(key, value) => (key -> value)
      case IntProp(key, value)    => (key -> value)
    }).toMap.toJSDictionary
    content match {
      case TextContent(text) => Superfine.h(tag, _props, text)
      case Children(nodes)   => Superfine.h(tag, _props, nodes.toJSArray)
    }
  }

  @js.native
  @JSImport("superfine", JSImport.Namespace)
  object Superfine extends js.Object {
    type Children = js.UndefOr[String]|js.Array[js.Object]
    type Props = js.Object|js.Dictionary[Any]

    def patch(node: Element, view: VNode): Element = js.native
    def h(
      tag: String,
      props: Props,
      children: Children = js.undefined
    ): VNode = js.native
  }
}
