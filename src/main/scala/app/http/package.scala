package app

import scala.concurrent.{Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global

import scala.scalajs.js.JSON
import org.scalajs.dom.raw.{Event, XMLHttpRequest}

package object http {
  case class Request(val headers: Map[String, String] = Map())
  private val EMPTY_REQUEST = new Request()

  def fetch[T](method: String, url: String, request: Request = EMPTY_REQUEST): Future[T] = {
    val p = Promise[T]()
    val xhr = new XMLHttpRequest()
    xhr.open(method, url)
    for {
      (key, value) <- request.headers
    } xhr.setRequestHeader(key, value)
    xhr.onload = {
      (e: Event) =>
        if (xhr.status >= 200 && xhr.status < 300) {
          val r = JSON.parse(xhr.responseText)
          p success r.asInstanceOf[T]
        }
        else p failure new FetchError(xhr.responseText, xhr.status)
    }
    xhr.send()
    p.future
  }

  val get = fetch("GET", _: String, _: Request)
  val post = fetch("POST", _: String, _: Request)
  val put = fetch("PUT", _: String, _: Request)
  val delete = fetch("DELETE", _: String, _: Request)

  class FetchError(val message: String, val statusCode: Int) extends Exception
}
