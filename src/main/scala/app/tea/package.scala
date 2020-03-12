package app

import superfine.Superfine._
import superfine._

import org.scalajs.dom


package object tea {
  type Dispatch[M] = M => Unit
  type EffectHandler[M, E] = (Dispatch[M], E) => Unit

  trait BasicApp[S, M] {
    def init (): S
    def update (prev: S, message: M): S
    def view (state: S, dispatch: Dispatch[M]): VNode
    val node: dom.Element
  }

  trait App[T, M, E] {
    def init(): (T, Seq[E])
    def view(state: T, dispatch: Dispatch[M]): VNode
    def update(prev: T, message: M): (T, Seq[E])
    val node: dom.Element
  }

  def start[S, M] (app: BasicApp[S, M]): Unit = {
    var _state = app.init()
    var _node = app.node

    def dispatch (message: M): Unit =
      setState(app.update(_state, message))

    def setState (state: S): Unit = {
      _state = state
      _node = patch(_node, app.view(state, dispatch))
    }

    setState(_state)
  }

  def start[T, M, E](app: App[T, M, E]) (implicit effects: EffectHandler[M, E]): Unit = {
    var (_state, _e) = app.init()
    var _node = app.node

    def dispatch (message: M): Unit = {
      val (state, _effects) = app.update(_state, message)
      setState(state)
      _effects foreach (effects(dispatch, _))
    }

    def setState (state: T): Unit = {
      _state = state
      _node = patch(_node, app.view(state, dispatch))
    }

    setState(_state)
    _e foreach (effects(dispatch, _))
  }
}
