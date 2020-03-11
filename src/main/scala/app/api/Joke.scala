package app.api

import scala.scalajs.js

@js.native
trait Joke extends js.Object {
  val id: String
  val joke: String
  val status: Int
}
