package app

import scala.concurrent.Future

import http.{get, Request}
import api.Joke

package object api {
  def fetchJoke (): Future[Joke] = {
    get("https://icanhazdadjoke.com", Request(Map(
      "Accept" -> "application/json"
    )))
  }
}
