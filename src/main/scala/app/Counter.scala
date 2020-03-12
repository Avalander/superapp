package app

import superfine._
import superfine.Html._
import superfine.{
  Attributes => Attrs,
  EventHandlers => Ev,
}
import tea.{BasicApp, Dispatch, start}

import org.scalajs.dom.{document, Event}

object Counter {
  type State = Int

  sealed trait Message
  case object Increment extends Message
  case object Decrement extends Message

  def mount (): Unit = {
    start(new BasicApp[State, Message] {
      val node = document.querySelector("#counter")

      def init () = 0

      def update (state: State, message: Message) =
        message match {
          case Decrement => state - 1
          case Increment => state + 1
        }
      
      def view (state: State, dispatch: Dispatch[Message]) = {
        div(Props.empty, Children(
          h1(Props(
            Attrs.`class`("title"),
          ), TextContent(state.toString)),
          div(Props.empty, Children(
            button(Props(
              Ev.onClick((e: Event) => dispatch(Decrement))
            ), TextContent(" - ")),
            button(Props(
              Ev.onClick((e: Event) => dispatch(Increment))
            ), TextContent(" + "))
          ))
        ))
      }
    })
  }
}
