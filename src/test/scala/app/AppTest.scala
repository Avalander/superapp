package app

import utest._

import scala.scalajs.js

import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.ext._

object AppTest extends TestSuite {
  App.setupUI()

  def tests = Tests {
    test("First test") {
      assert(document.querySelectorAll("p")
        .count(_.textContent == "Hello world!") == 1)
    }

    test("Button click") {
      def messageCount =
        document.querySelectorAll("p").count(_.textContent == "You clicked the button!")
      
      val button = document.querySelector("button").asInstanceOf[dom.html.Button]
      assert(button != null && button.textContent == "Click me!")
      assert(messageCount == 0)

      for (c <- 1 to 5) {
        button.click()
        assert(messageCount == c)
      }
    }
  }
}