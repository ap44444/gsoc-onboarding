package gsoc
package contributors

import cats.effect.*

import fs2.concurrent.*
import fs2.dom.HtmlElement

import calico.html.io.{*, given}
import calico.syntax.*

val raibow = List("red", "orange", "yellow", "green", "blue", "indigo", "violet")

val gfinol = Contributor("gfinol"):
  SignallingRef[IO].of(0).toResource.flatMap { count =>
    div(
      p(
        "I am ",
        count.map(r => raibow(r % raibow.length)).changes.map { color =>
          span(
            styleAttr := s"color: $color; font-weight: bold",
            "@gfinol"
          )
        },
        " on GitHub. I agree to follow the Typelevel CoC and GSoC AI policy."
      ),
      button(
        onClick --> (_.foreach(_ => count.update(_ + 1 % raibow.length))),
        "Click me! 🌈"
      )
    )
  }
