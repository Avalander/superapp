package app

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Success, Failure}

import org.scalajs.dom
import org.scalajs.dom.document
import scala.scalajs.js.annotation.JSExportTopLevel

import app.api.{fetchJoke, Joke}
import app.Super

object App {
  def appendPar (targetNode: dom.Node, text: String, id: Option[String] = None): Unit = {
    val par = document.createElement("p")
    par.textContent = text
    id match {
      case None    => null
      case Some(x) => par.setAttribute("id", x)
    }
    targetNode.appendChild(par)
  }

  @JSExportTopLevel("addClickedMessage")
  def addClickedMessage (): Unit = {
    appendPar(document.body, "You clicked the button!")
  }

  def main (args: Array[String]): Unit = {
    Super.counter()
    Super.jokes()
    document.addEventListener("DOMContentLoaded", {
      (e: dom.Event) => {
        setupUI()
        fetchJoke() andThen {
          case Success(j) => document.querySelector("#joke").textContent = j.joke
          case Failure(e) => println(e)
        }
      }
    })
  }

  def setupUI (): Unit = {
    appendPar(document.body, "Hello world!")
    appendPar(document.body, "...", Some("joke"))
    val button = document.createElement("button")
    button.textContent = "Click me!"
    button.addEventListener("click", {
      (e: dom.MouseEvent) =>
        fetchJoke() andThen {
          case Success(j) => document.querySelector("#joke").textContent = j.joke
          case Failure(e) => println(e)
        }
    })
    document.body.appendChild(button)
  }
}