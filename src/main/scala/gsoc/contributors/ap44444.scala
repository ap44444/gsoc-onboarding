package gsoc
package contributors

import cats.effect.*
import fs2.concurrent.*
import fs2.dom.HtmlElement
import calico.html.io.{*, given}
import calico.syntax.*
import scala.concurrent.duration.DurationInt

val ap44444: Contributor = Contributor("ap44444"):

  case class Dot(handle: String, x: Double, y: Double, vx: Double, vy: Double)

  def makeDot(handle: String, i: Int): Dot =
    Dot(handle, 50 + (i * 37) % 300, 50 + (i * 53) % 200, 1.5 + (i % 3) * 0.5, 1.0 + (i % 2) * 0.5)

  def bounce(d: Dot): Dot =
    val nx = d.x + d.vx
    val ny = d.y + d.vy
    val nvx = if nx < 0 || nx > 370 then -d.vx else d.vx
    val nvy = if ny < 0 || ny > 270 then -d.vy else d.vy
    d.copy(x = nx.max(0).min(370), y = ny.max(0).min(270), vx = nvx, vy = nvy)

  case class State(dots: List[Dot], meAdded: Boolean)

  IO {
    allContributors.toList.filterNot(_.handle == "ap44444").zipWithIndex.map { case (c, i) => makeDot(c.handle, i) }
  }.toResource.flatMap { others =>
    SignallingRef[IO].of(State(others, false)).toResource.flatMap { state =>
      val ticker = fs2.Stream
        .fixedRate[IO](50.millis)
        .evalMap(_ => state.update(s => s.copy(dots = s.dots.map(bounce))))
        .compile.drain.background

      ticker.flatMap { _ =>
        div(
          p("Hello, I'm @ap44444 on GitHub. I agree to follow the Typelevel CoC and GSoC AI policy."),
          state.map { s =>
            button(
              onClick --> (_.foreach(_ =>
                if s.meAdded then
                  state.update(s => s.copy(dots = s.dots.filterNot(_.handle == "ap44444"), meAdded = false))
                else
                  state.update(s => s.copy(dots = s.dots :+ makeDot("ap44444", 99), meAdded = true))
              )),
              if s.meAdded then "Take me out :(" else "Add me!"
            )
          },
          h3("Contributor Box"),
          state.map { s =>
            div(
              styleAttr := "position: relative; width: 400px; height: 300px; background: #111; border-radius: 8px; overflow: hidden; margin-top: 8px;",
              s.dots.map { d =>
                val isMe = d.handle == "ap44444"
                div(
                  styleAttr := s"position: absolute; left: ${d.x}px; top: ${d.y}px; transform: translate(-50%,-50%); font-size: 0.65rem; color: ${if isMe then "#ffdd00" else "white"}; background: ${if isMe then "#554400" else "#333"}; padding: 2px 4px; border-radius: 4px; white-space: nowrap;",
                  d.handle
                )
              }
            )
          }
        )
      }
    }
  }