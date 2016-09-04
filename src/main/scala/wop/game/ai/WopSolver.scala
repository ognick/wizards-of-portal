package wop.game.ai
import wop.game._
import wop.game.TicTacToe._
import wop.game.WopState._

import scala.annotation.tailrec
import scala.language.implicitConversions

object WopSolver {
  val MAX_DEPTH = 5

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

    def evalBoard[T](board: TicTacToe[T])(implicit item: Item[T], factor: (T => Int)): Int =
      board.status match {
        case Status.Draw => 0
        case Status.Finished(x) => 20 * factor(x)
        case Status.NotFinished => scan(board)
      }

    def evalState(state: WopState): Int = state match {
      case s: Win => 500 * evalFactor(s.player.xo)
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
  case class AlphaBeta(player: Player, ab: Option[(Int, Int)]) {
    private def gt(a: Int, b: Int) = a >= b
    private def lt(a: Int, b: Int) = a <= b

    val (alpha, beta) = ab match {
      case Some(x) => x
      case _ =>
        player match {
          case Player.P1 => (Int.MaxValue, Int.MinValue)
          case Player.P2 => (Int.MinValue, Int.MaxValue)
        }
    }

    val initial: Int = player match {
      case Player.P1 => alpha
      case Player.P2 => beta
    }

    def compare(a: EvalPoint, b: EvalPoint): EvalPoint = {
      val cmp: ((Int, Int) => Boolean) = player match {
        case Player.P1 => gt
        case Player.P2 => lt
      }
      if (cmp(a._1, b._1)) a else b
    }

    def update(score: Int): (Int, Int) = player match {
      case Player.P1 => (score, beta)
      case Player.P2 => (alpha, score)
    }

    def testPruning(score: Int): Boolean = player match {
      case Player.P1 => gt(score, beta)
      case Player.P2 => lt(score, alpha)
    }

    def playerFactor(xo: WopState.XO): Int = xo match {
      case WopState.Player.P1.xo => 1
      case WopState.Player.P2.xo => -1
      case WopState.XO.Empty => 0
    }
  }

  def minMax(state: WopState): Option[Point] = {
    def loop(state: Either[Foul, WopState], currPoint: Option[Point], depth: Int, ab: Option[(Int, Int)]): EvalPoint =
      state match {
        case Right(s: WopState) =>
          lazy val returnResult = (Heuristica.evalState(s), currPoint)
          if (depth >= MAX_DEPTH) returnResult
          else
            s match {
              case is: InProgress =>
                val alphaBeta = AlphaBeta(is.player, ab)
                @tailrec def processChild(points: List[Point], player: Player, currEval: EvalPoint): EvalPoint =
                  points match {
                    case (point: Point) :: tl =>
                      val nextPoint = currPoint match {
                        case None => Some(point)
                        case _ => currPoint
                      }
                      val ab = Some(alphaBeta.update(currEval._1))
                      val eval = loop(is(point), nextPoint, depth + 1, ab)
                      val bestEval = alphaBeta.compare(currEval, eval)
                      if (alphaBeta.testPruning(bestEval._1)) bestEval
                      else processChild(tl, player, bestEval)
                    case _ => currEval

                  }

                val initEval = (alphaBeta.initial, currPoint)
                is match {
                  case s: Select => processChild(freePoints(s.board), s.player, initEval)
                  case s: Turn =>
                    processChild(freePoints(s.board(s.currentSubBoard)), s.player, initEval)
                }
              case _ => returnResult
            }

        case _ => (0, None)
      }
    loop(Right(state), None, 0, None)._2
  }
}
