import wop.game.{Point, WopState}
import wop.game.WopState.{InProgress, Player}
import wop.game.ai.WopSolver

import scala.annotation.tailrec

object RunSolver extends App {
  @tailrec def loop(state: WopState): Option[WopState] = {
    println(state)
    state match {
      case s: InProgress if s.player == Player.P2 =>
        WopSolver.minMax(state) match {
          case Some(p) => {
            s(p) match {
              case Right(newState) => loop(newState)
              case x => {
                println("i dont know", x)
                None
              }
            }
          }
          case x => {
            println("i dont know", x)
            None
          }
        }
      case prevState: WopState.Turn =>
        println("input local x,y")
        val point: Point = (readInt(), readInt())
        prevState(point) match {
          case Right(newState) => loop(newState)
          case _ =>
            println("wrong input")
            loop(prevState)
        }
      case prevState: WopState.Select =>
        println("input global x,y")
        val point: Point = (readInt(), readInt())
        prevState(point) match {
          case Right(newState) => loop(newState)
          case _ =>
            println("wrong input")
            loop(prevState)
        }
      case _ => None
    }
  }

  loop(WopState.initial)

}
