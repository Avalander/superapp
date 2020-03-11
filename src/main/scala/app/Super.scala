package app

import superfine.Superfine._
import superfine.dom._
import superfine._

import org.scalajs.dom
import org.scalajs.dom.document

import scala.scalajs.js
import scala.scalajs.js.JSON

import api.{fetchJoke, Joke}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

import tea.{App, BasicApp, Dispatch, EffectHandler, start}

object Super {
  private val EmptyObj = js.Dynamic.literal()

  def counter(): Unit = {
    type State = Int

    sealed trait Message
    case object Increment extends Message
    case object Decrement extends Message

    start(new BasicApp[State, Message] {
      val node = document.querySelector("#app")

      def init(): State = 0

      def update(prev: State, message: Message): State =
        message match {
          case Decrement => prev - 1
          case Increment => prev + 1
        }

      def view(state: State, dispatch: Dispatch[Message]): js.Object = {
        div(EmptyObj, js.Array(
          h1(EmptyObj, state.toString),
          div(EmptyObj, js.Array(
            button(js.Dynamic.literal(
              onclick = {
                (e: dom.Event) => dispatch(Decrement)
              }
            ), " - "),
            button(js.Dynamic.literal(
              onclick = {
                (e: dom.Event) => dispatch(Increment)
              }
            ), " + ")
          ))
        ))
      }
    })
  }

  def jokes(): Unit = {
    type State = Option[String]

    sealed trait Message
    case class SetJoke(joke: String) extends Message
    case object FetchJoke extends Message
    case class JokeError(e: Throwable) extends Message

    sealed trait Effect
    case object OnFetchJoke extends Effect

    implicit def handleEffect (dispatch: Dispatch[Message], effect: Effect): Unit =
      effect match {
        case OnFetchJoke =>
          fetchJoke() andThen {
            case Failure(e) => dispatch(JokeError(e))
            case Success(j) => dispatch(SetJoke(j.joke))
          }
      }

    start(new App[State, Message, Effect] {
      val node = document.querySelector("#jokes")

      def init(): (State, Seq[Effect]) = (None, List(OnFetchJoke))

      def update (prev: State, message: Message): (State, Seq[Effect]) =
        message match {
          case SetJoke(joke) => (Some(joke), Nil)
          case FetchJoke     => (prev, List(OnFetchJoke))
          case JokeError(e)  => {
            println(e)
            (Some("Oops, something went wrong"), Nil)
          }
        }

      def view (state: State, dispatch: Dispatch[Message]): VNode = {
        state match {
          case None =>
            div(EmptyObj, js.Array(
              p(EmptyObj, "Press the button to load a joke."),
              button(js.Dynamic.literal(
                onclick = {
                  (e: dom.Event) => dispatch(FetchJoke)
                }
              ), "Gimme a joke!")
            ))
          case Some(joke) =>
            div(EmptyObj, js.Array(
              p(EmptyObj, joke),
              button(js.Dynamic.literal(
                onclick = {
                  (e: dom.Event) => dispatch(FetchJoke)
                }
              ), "Another joke!")
            ))
        }
      }
    })
  }
}
