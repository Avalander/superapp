package app

import scala.util.Random

import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.raw.KeyboardEvent
import scalajs.js
import scala.scalajs.js.timers.setTimeout

import superfine.{VNode}
import superfine.svg._

import tea.{App, Dispatch, EffectHandler, start}

object Snake {
  val SIZE = 15
  val WIDTH = SIZE * 40
  val HEIGHT = SIZE * 27

  object Colors {
    val background = "#088c64"
    object Snake {
      val fill = "#bcaba0"
      val stroke = "#706660"
    }
    object Apple {
      val fill = "#ff5a5f"
      val stroke = "#b23e42"
    }
  }

  val rand = new Random()


  // State

  class State(
    val snake: Seq[Point],
    val direction: Direction,
    val nextDirection: Direction,
    val apple: Point,
    val score: Int,
    val isRunning: Boolean,
  ) {
    def withSnake (snake: Seq[Point]): State =
      new State(
        snake = snake,
        direction = this.direction,
        nextDirection = this.nextDirection,
        apple = this.apple,
        score = this.score,
        isRunning = this.isRunning,
      )

    def withApple (apple: Point): State =
      new State(
        snake = this.snake,
        direction = this.direction,
        nextDirection = this.nextDirection,
        apple = apple,
        score = this.score,
        isRunning = this.isRunning,
      )

    def incScore (value: Int): State =
      new State(
        snake = this.snake,
        direction = this.direction,
        nextDirection = this.nextDirection,
        apple = this.apple,
        score = this.score + value,
        isRunning = this.isRunning,
      )

    def end (): State =
      new State(
        snake = this.snake,
        direction = this.direction,
        nextDirection = this.nextDirection,
        apple = this.apple,
        score = this.score,
        isRunning = false,
      )
  }

  case class Point(val x: Int, val y: Int)

  def createApple (): Point = {
    val x = rand.nextInt(WIDTH / SIZE) * SIZE
    val y = rand.nextInt(HEIGHT / SIZE) * SIZE
    Point(x, y)
  }

  def collision (a: Point, b: Point) = {
    a.x == b.x && a.y == b.y
  }

  def isOutOfBounds (p: Point): Boolean = {
    val Point(x, y) = p
    x < 0 || x >= WIDTH || y < 0 || y >= HEIGHT
  }

  def selfCollision (ps: Seq[Point]): Boolean = {
    val (head :: tail) = ps
    tail.exists(collision(head, _))
  }

  def hasLost (snake: Seq[Point]): Boolean = {
    isOutOfBounds(snake.head) || selfCollision(snake)
  }

  sealed trait Direction {
    val opposite: Direction
  }
  case object Right extends Direction {
    val opposite = Left
  }
  case object Left extends Direction {
    val opposite = Right
  }
  case object Up extends Direction {
    val opposite = Down
  }
  case object Down extends Direction {
    val opposite = Up
  }

  val keyToMessage = Map(
    "ArrowUp"    -> SetDirection(Up),
    "ArrowDown"  -> SetDirection(Down),
    "ArrowLeft"  -> SetDirection(Left),
    "ArrowRight" -> SetDirection(Right),
    "r"          -> Restart,
  )

  object State {
    def of (
      snake: Seq[Point],
      direction: Direction = Right,
      nextDirection: Direction = Right,
      apple: Point = createApple(),
      score: Int = 0,
      isRunning: Boolean = true,
    ): State =
      new State(snake, direction, nextDirection, apple, score, isRunning)
  }


  // Messages

  sealed trait Message
  case object UpdateState extends Message
  case class SetDirection(d: Direction) extends Message
  case object UpdateApple extends Message
  case object Continue extends Message
  case object Restart extends Message

  def updateSnake (snake: Seq[Point], direction: Direction): Seq[Point] = {
    val Point(x, y) = snake.head
    val tail = snake.init
    direction match {
      case Down  => Point(x, y + SIZE) +: tail
      case Up    => Point(x, y - SIZE) +: tail
      case Left  => Point(x - SIZE, y) +: tail
      case Right => Point(x + SIZE, y) +: tail
    }
  }

  def growSnake (prev: State): State =
    prev withSnake {
      prev.snake :+ prev.snake.last
    }

  def updateState (prev: State): State = {
    State.of(
      snake = updateSnake(prev.snake, prev.nextDirection),
      direction = prev.nextDirection,
      nextDirection = prev.nextDirection,
      apple = prev.apple,
      score = prev.score,
      isRunning = prev.isRunning,
    )
  }

  def changeDirection (prev: State, next: Direction): State = {
    if (prev.direction.opposite == next) prev
    else State.of(
      snake = prev.snake,
      direction = prev.direction,
      nextDirection = next,
      apple = prev.apple,
      score = prev.score,
      isRunning = prev.isRunning,
    )
  }

  def updateApple (prev: State): State = {
    if (collision(prev.snake.head, prev.apple)) {
      eatApple(prev)
    }
    else prev
  }

  def eatApple (prev: State): State =
    growSnake(prev withApple (createApple()) incScore 10)


  // Effects

  sealed trait Effect
  case class Delay(dt: Int, m: Message) extends Effect
  case object Frame extends Effect
  case object KeyDown extends Effect

  implicit def effectHandler (dispatch: Dispatch[Message], effect: Effect): Unit = {
    effect match {
      case Delay(dt, m) => {
        setTimeout(dt) {
          dispatch(m)
        }
      }
      case Frame => {
        dispatch(UpdateApple)
        dispatch(UpdateState)
        effectHandler(dispatch, Delay(150, Continue))
      }
      case KeyDown => {
        def listener(ev: KeyboardEvent): Unit = {
          keyToMessage get ev.key match {
            case None    => ()
            case Some(m) => {
              ev.preventDefault()
              dispatch(m)
            }
          }
        }
        document.addEventListener("keydown", listener)
      }
    }
  }


  // App

  def app (parent: dom.Element): App[State, Message, Effect] =
    new App[State, Message, Effect] {
      val node = parent
      def init(): (State, Seq[Effect]) = (State.of(
        snake = List(
          Point(3 * SIZE, 3 * SIZE),
          Point(2 * SIZE, 3 * SIZE),
          Point(1 * SIZE, 3 * SIZE),
        )
      ), List(
        KeyDown,
        Frame
      ))
      def update(prev: State, message: Message): (State, Seq[Effect]) = {
        message match {
          case UpdateState => (
            updateState(prev),
            Nil
          )
          case UpdateApple => (
            updateApple(prev),
            Nil
          )
          case SetDirection(d) => (
            changeDirection(prev, d),
            Nil
          )
          case Continue =>
            if (hasLost(prev.snake)) (prev.end, Nil)
            else (prev, List(Frame))
          case Restart =>
            if (prev.isRunning) (prev, Nil)
            else (init()._1, List(Frame))
        }
      }
      def view(state: State, dispatch: Dispatch[Message]): VNode = {
        svg(
          js.Dynamic.literal(
            viewBox = s"0 0 $WIDTH $HEIGHT",
            width = WIDTH,
            height = HEIGHT
          ), List(
            Views.background(),
            Views.apple(state.apple),
            Views.snake(state),
            if (state.isRunning) Views.score(state.score)
            else Views.gameOver(state.score),
          )
        )
      }
    }

  def snake (): Unit = {
    start(app(document.querySelector("#snake")))
  }


  // Views

  object Views {
    private def withKey (_key: String): VNode =
      js.Dynamic.literal(
        key = _key
      )

    def background (): VNode = {
      g(
        withKey("background"),
        List(
          rect(
            js.Dynamic.literal(
              x = 0,
              y = 0,
              width = WIDTH,
              height = HEIGHT,
              fill = Colors.background
            ),
            Nil
          )
        )
      )
    }

    private def point (x: Int, y: Int, fill: String, stroke: String): VNode =
      js.Dynamic.literal(
        x = x,
        y = y,
        fill = fill,
        stroke = stroke,
        width = SIZE,
        height = SIZE
      )

    def snake (state: State): VNode = {
      g(
        withKey("snake"),
        state.snake map {
          case Point(x, y) => rect(
            point(x, y, Colors.Snake.fill, Colors.Snake.stroke),
            Nil
          )
        }
      )
    }

    def apple (state: Point): VNode = {
      val Point(x, y) = state
      g(
        withKey("apple"),
        List(
          rect(
            point(x, y, Colors.Apple.fill, Colors.Apple.stroke),
            Nil
          )
        )
      )
    }

    private val scoreStyle =
      "font: bold 20px sans-seriff; fill: #fff; opacity: 0.8;"

    def score (state: Int): VNode = {
      g(
        withKey("score"),
        List(
          text(
            js.Dynamic.literal(
              x = 5,
              y = 20,
              style = scoreStyle,
            ),
            s"Score: $state"
          )
        )
      )
    }

    private object GameOverStyle {
      val title = List(
        "font: bold 48px sans-seriff",
        "fill: #fff",
        "opacity: 0.8",
        "text-anchor: middle"
      ) mkString "; " :+ ';'

      val text = List(
        "font: 30px sans-seriff",
        "fill: #fff",
        "opacity: 0.8",
        "text-anchor: middle"
      ) mkString "; " :+ ';'
    }

    def gameOver (score: Int): VNode = {
      g(withKey("game-over"),
        List(
          rect(js.Dynamic.literal(
            x = 0,
            y = 0,
            width = WIDTH,
            height = HEIGHT,
            fill = "#000",
            opacity = 0.4,
          ), Nil),
          text(js.Dynamic.literal(
            style = GameOverStyle.title,
            x = WIDTH / 2,
            y = 100,
          ), "Game Over"),
          text(js.Dynamic.literal(
            style = GameOverStyle.text,
            x = WIDTH / 2,
            y = 160,
          ), s"Score: $score"),
          text(js.Dynamic.literal(
            style = GameOverStyle.text,
            x = WIDTH / 2,
            y = 190,
          ), "Press 'R' to play again")
        )
      )
    }
  }
}
