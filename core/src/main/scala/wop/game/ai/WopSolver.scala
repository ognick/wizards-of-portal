package wop.game.ai

import wop.game._
import wop.game.TicTacToe._
import wop.game.WopState._

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.math._

object WopSolver {
  val MAX_DEPTH = 8

  implicit case object Time extends TimeProvider {
    def currentTime: Long = 0
  }

  object Heuristica {
    implicit def evalFactor(xo: WopState.XO): Int = xo match {
      case WopState.Player.P1.xo => 1
      case WopState.Player.P2.xo => -1
      case WopState.XO.Empty => 0
    }

    implicit def evalFactor(board: SubBoard): Int = board.status match {
      case Status.Finished(x) => evalFactor(x) * 10
      case _ => 0
    }

    def scan[T](board: TicTacToe[T])(implicit item: Item[T], evalFactor: (T => Int)): Int = {
      @tailrec def loop(x: Int, y: Int, dx: Int, dy: Int, prev: Option[T] = None, acc: Int = 0): Int = {
        if (x > board.boundary || y > board.boundary || x < 0 || y < 0) acc
        else {
          val (curr: T) = board.apply(x, y)
          if (item.nonEmpty(curr)) prev match {
            case Some(p) if item.compare(curr, p) =>
              loop(x + dx, y + dy, dx, dy, Some(curr), acc + evalFactor(curr) * 2)
            case None => loop(x + dx, y + dy, dx, dy, Some(curr), acc + evalFactor(curr))
            case _ => 0
          } else loop(x + dx, y + dy, dx, dy, prev, acc)
        }
      }

      val sizeRange = (0 until WopState.Size).toList
      val vertical = (for (x <- sizeRange) yield loop(x = x, y = 0, 0, 1)).sum
      val horizontal = (for (y <- sizeRange) yield loop(x = 0, y = y, 1, 0)).sum
      val diagonal = loop(x = 0, y = 0, 1, 1) + loop(x = board.boundary, y = 0, -1, 1)

      vertical + diagonal + horizontal
    }

    def evalBoard[T](board: TicTacToe[T])(implicit item: Item[T], evalFactor: (T => Int)): Int =
      board.status match {
        case Status.Draw => 0
        case Status.Finished(x) => 20 * evalFactor(x)
        case Status.NotFinished => scan(board)
      }

    def evalState(state: WopState): Int = state match {
      case s: Win => 10000 * evalFactor(s.player.xo)
      case s: Draw => 0
      case s: InProgress =>
        val sizeRange = (0 until WopState.Size).toList
        val subBoardsEval = for (x <- sizeRange; y <- sizeRange) yield evalBoard(s.board.apply((x, y)))
        evalBoard(s.board) + subBoardsEval.sum
    }
  }

  def freePoints[T](board: TicTacToe[T])(implicit item: Item[T]): List[Point] = {
    val sizeRange = (0 until WopState.Size).toList
    val optionList =
      for (x <- sizeRange; y <- sizeRange)
        yield if (item.nonEmpty(board(x, y))) None else Some(x, y)
    optionList.flatten
  }

  type EvalPoint = (Int, Option[Point])

  case object AlphaBeta {
    type AB = (Int, Int)
    val initAB: AB = (Int.MinValue, Int.MaxValue)
    val testPruning: (AB) => Boolean = {
      case (a, b) => b <= a
    }

    val initlEval: (Player) => Int = {
      case Player.P1 => Int.MinValue
      case Player.P2 => Int.MaxValue
    }

    def compare(player: Player, curEval: EvalPoint, bestEval: EvalPoint) = player match {
      case Player.P1 => if (bestEval._1 < curEval._1) curEval else bestEval
      case Player.P2 => if (curEval._1 < bestEval._1) curEval else bestEval
    }

    def update(player: Player, ab: AB, evalPoint: EvalPoint): AB = {
      val (eval: Int, _) = evalPoint
      val (a: Int, b: Int) = ab
      player match {
        case Player.P1 => (max(a, eval), b)
        case Player.P2 => (a, min(b, eval))
      }
    }
  }

  def minMax(state: WopState): Option[Point] = {
    def loop(state: Either[Foul, WopState], currPoint: Option[Point], depth: Int, ab: AlphaBeta.AB): EvalPoint =
      state match {
        case Right(s: WopState) =>
          lazy val returnResult = (Heuristica.evalState(s), currPoint)
          if (depth >= MAX_DEPTH)
            returnResult
          else
            s match {
              case is: InProgress =>
                @tailrec def processChild(points: List[Point], currEval: EvalPoint, ab: AlphaBeta.AB): EvalPoint =
                  points match {
                    case (point: Point) :: tl =>
                      val nextPoint = currPoint match {
                        case None => Some(point)
                        case _ => currPoint
                      }
                      val eval = loop(is(point), nextPoint, depth + 1, ab)
                      val bestEval = AlphaBeta.compare(is.player, eval, currEval)
                      val abUp = AlphaBeta.update(is.player, ab, bestEval)
                      if (AlphaBeta.testPruning(ab))
                        bestEval
                      else
                        processChild(tl, bestEval, abUp)
                    case _ => currEval
                  }

                val initEvalPoint: EvalPoint = (AlphaBeta.initlEval(is.player), currPoint)
                is match {
                  case s: Select => processChild(freePoints(s.board), initEvalPoint, ab)
                  case s: Turn =>
                    processChild(freePoints(s.board(s.currentSubBoard)), initEvalPoint, ab)
                }
              case _ => returnResult
            }

        case _ => (0, None)
      }
    val (_, point) = loop(Right(state), None, 0, AlphaBeta.initAB)
    point
  }
}
