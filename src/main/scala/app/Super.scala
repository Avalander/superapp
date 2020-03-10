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

object Super {
  private val EmptyObj = js.Dynamic.literal()

  type Dispatch[M] = M => Unit

  trait App[T, M] {
    def init(): T
    def view(state: T, dispatch: Dispatch[M]): js.Object
    def update(prev: T, message: M): T
    val node: dom.Element
  }

  def start[T, M](app: App[T, M]): Unit = {
    var _state = app.init()
    var _node = app.node

    def dispatch (message: M): Unit = setState(app.update(_state, message))
    def setState (state: T): Unit = {
      _state = state
      _node = patch(_node, app.view(state, dispatch))
    }

    setState(_state)
  }

  def counter(): Unit = {
    type State = Int

    sealed trait Message
    case object Increment extends Message
    case object Decrement extends Message

    start(new App[State, Message] {
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

    start(new App[State, Message] {
      val node = document.querySelector("#jokes")

      def init(): State = None

      def update (prev: State, message: Message): State =
        message match {
          case SetJoke(joke) => Some(joke)
          case FetchJoke     => None
          case JokeError(e)  => {
            println(e)
            Some("Oops, something went wrong")
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
                  (e: dom.Event) => {
                    // dispatch(FetchJoke)
                    fetchJoke() andThen {
                      case Failure(e) => dispatch(JokeError(e))
                      case Success(j) => dispatch(SetJoke(j.joke))
                    }
                  }
                }
              ), "Another joke!")
            ))
        }
      }
    })
  }

  def app(): Unit = {
    val node = document.querySelector("#app")

    def setState[T] (state: T): Unit = {
      val view = h("div", EmptyObj, js.Array(
        h("h1", EmptyObj, state.toString),
        h("input", js.Dynamic.literal(
          `type` = "text",
          value = state.toString,
          oninput = {
            (e: js.Dynamic) => {
              setState(e.target.value)
            }
          },
          autofocus = true
        ))
      ))
      patch(node, view)
    }

    setState("Potato")
  }
}