import wop.game.{Point, WopState}
import wop.game.WopState.{InProgress, Player}
import wop.game.ai.WopSolver

import scala.annotation.tailrec

object RunSolver extends App {
  @tailrec def loop(state: WopState, whoAI: Player): Option[WopState] = {
    println(state)
    state match {
      case s: InProgress if s.player == whoAI =>
        WopSolver.minMax(state) match {
          case Some(p) => {
            s(p) match {
              case Right(newState) => loop(newState, whoAI)
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
          case Right(newState) => loop(newState, whoAI)
          case _ =>
            println("wrong input")
            loop(prevState, whoAI)
        }
      case prevState: WopState.Select =>
        println("input global x,y")
        val point: Point = (readInt(), readInt())
        prevState(point) match {
          case Right(newState) => loop(newState, whoAI)
          case _ =>
            println("wrong input")
            loop(prevState, whoAI)
        }
      case _ => None
    }
  }
  @tailrec def whoAI : Player = {
    println("Which are AI X or O?")
    readChar() match {
      case 'X' => Player.P1
      case 'O' => Player.P2
      case _ => whoAI
    }
  }
  loop(WopState.initial, whoAI)
//  loop(WopState.initial, Player.P1)

}
