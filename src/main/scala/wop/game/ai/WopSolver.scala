package wop.game.ai
import wop.game._
import wop.game.TicTacToe.{Item, Status}
import wop.game.WopState.{Draw, Foul, InProgress, Player, Select, SubBoard, Turn, Win}

import scala.annotation.tailrec

object WopSolver {
  val MAX_DEPTH = 2

  def playerFactor(xo: WopState.XO): Int = xo match {
    case WopState.Player.P1.xo => 1
    case WopState.Player.P2.xo => -1
    case WopState.XO.Empty => 0
  }

  def scan(subBoard: SubBoard)(implicit item: Item[WopState.XO]): Int = {
    @tailrec def loop(x: Int, y: Int, dx: Int, dy: Int, prev: Option[WopState.XO] = None, acc: Int = 0): Int = {
      if (x > subBoard.boundary || y > subBoard.boundary || x < 0 || y < 0) acc
      else {
        val curr = subBoard.apply(x, y)
        if (item.nonEmpty(curr)) prev match {
          case Some(p) if item.compare(curr, p) =>
            loop(x + dx, y + dy, dx, dy, Some(curr), acc + playerFactor(curr) * 2)
          case None => loop(x + dx, y + dy, dx, dy, Some(curr), acc + playerFactor(curr))
          case _ => 0
        } else loop(x + dx, y + dy, dx, dy, prev, acc)
      }
    }

    val sizeRange = (0 until WopState.Size).toList
    val vertical = (for (x <- sizeRange) yield loop(x = x, y = 0, 0, 1)).sum
    val horizontal = (for (y <- sizeRange) yield loop(x = 0, y = y, 1, 0)).sum
    val diagonal = loop(x = 0, y = 0, 1, 1) + loop(x = subBoard.boundary, y = 0, -1, 1)

    vertical + diagonal + horizontal
  }

  def heuristicEvalSubBoard(subBoard: SubBoard): Int = subBoard.status match {
    case Status.Draw => 0
    case Status.Finished(xo) => 20 * playerFactor(xo)
    case Status.NotFinished => scan(subBoard)
  }

  def heuristicEvalState(state: WopState): Int = state match {
    case s: Win => 200 * playerFactor(s.player.xo)
    case s: Draw => 0
    case s: InProgress =>
      val sizeRange = (0 until WopState.Size).toList
      val evals = for (x <- sizeRange; y <- sizeRange) yield heuristicEvalSubBoard(s.board.apply((x, y)))
      evals.sum
  }

  def freePoints[T](board: TicTacToe[T])(implicit item: Item[T]): List[Point] = {
    val sizeRange = (0 until WopState.Size).toList
    val optionList =
      for (x <- sizeRange; y <- sizeRange)
        yield if (item.nonEmpty(board(x, y))) None else Some(x, y)
    optionList.flatten
  }

  type EvalPoint = (Int, Option[Point])

  def max(best: EvalPoint, up: EvalPoint) = if (best._1 > up._1) best else up
  def min(best: EvalPoint, up: EvalPoint) = if (best._1 < up._1) best else up

  def playerSolve(player: Player): (EvalPoint, EvalPoint) => EvalPoint = player match {
    case Player.P1 => max
    case Player.P2 => min
  }

  def playerInitialVal(player: Player): Int = player match {
    case Player.P1 => Int.MinValue
    case Player.P2 => Int.MaxValue
  }

  def alphaBeta(state: Either[Foul, WopState], prevPoint: Option[Point], depth: Int): EvalPoint = state match {
    case Right(s: WopState) =>
      lazy val returnResult = (WopSolver.heuristicEvalState(s), prevPoint)
      if (depth >= MAX_DEPTH) returnResult
      else
        s match {
          case is: InProgress =>
            @tailrec def processChild(points: List[Point], player: Player, bestEval: EvalPoint): EvalPoint =
              points match {
                case (point: Point) :: tl =>
                  val nextPoint = prevPoint match {
                    case None => Some(point)
                    case _ => prevPoint
                  }
                  val upEval = alphaBeta(is(point), nextPoint, depth + 1)
                  val solver = playerSolve(player)
                  processChild(tl, player, solver(bestEval, upEval))
                case _ => bestEval

              }
            is match {
              case s: Select => processChild(freePoints(s.board), s.player, (playerInitialVal(s.player), prevPoint))
              case s: Turn =>
                processChild(freePoints(s.board(s.currentSubBoard)), s.player, (playerInitialVal(s.player), prevPoint))
            }
          case _ => returnResult
        }

    case _ => (0, None)
  }
}
