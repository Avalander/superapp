package app

import superfine._
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
        hh("div", Props(), Children(
          hh("h1", Props(
            StringProp("class", "title")
          ), TextContent(state.toString)),
          hh("div", Props(), Children(
            hh("button", Props(
              EventProp("onclick", (e: Event) => dispatch(Decrement))
            ), TextContent(" - ")),
            hh("button", Props(
              EventProp("onclick", (e: Event) => dispatch(Increment))
            ), TextContent(" + ")),
          ))
        ))
      }
    })
  }
}
