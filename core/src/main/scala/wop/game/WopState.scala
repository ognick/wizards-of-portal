package wop.game

import TicTacToe._
import scala.concurrent.duration._

object WopState {

  val Size = 3
  def TurnTime = (30 seconds).toMillis

  sealed trait Foul

  object Foul {
    case object CellIsNotEmpty extends Foul
    case object AlreadyFinished extends Foul
    case object NotYourTurn extends Foul
    case class Timeout(player: Player) extends Foul
  }

  sealed trait Player {
    def xo: XO
    def next: Player
  }

  object Player {
    case object P1 extends Player {
      val xo = XO.X
      val next = P2
    }
    case object P2 extends Player {
      val xo = XO.O
      val next = P1
    }
  }

  sealed trait XO

  object XO {
    case object X extends XO
    case object O extends XO
    case object Empty extends XO {
      override def toString = " "
    }
    val empty: XO = Empty
  }

  // Alias for tic-tac-toe of tic-tac-toes
  type Board = TicTacToe[SubBoard]
  type SubBoard = TicTacToe[XO]

  implicit object SubBoardItem extends Item[XO] {
    def compare(a: XO, b: XO): Boolean = a == b
    def nonEmpty(v: XO): Boolean = v != XO.Empty
  }

  implicit object BoardItem extends Item[SubBoard] {
    def compare(a: SubBoard, b: SubBoard): Boolean = a.status == b.status
    def nonEmpty(v: SubBoard): Boolean = v.status.finished
  }

  trait TimeProvider {
    def currentTime: Long
  }

  sealed trait InProgress extends WopState {
    def player: Player
    def board: Board
    def currentTime: Long
    def finishTime: Long
    def apply(point: Point)(implicit time: TimeProvider): Either[Foul, WopState]
    def withTimestamp(t: Long): InProgress

    def nextTimeState()(implicit time: TimeProvider): Either[Foul.Timeout, InProgress] = {
      val now: Long = time.currentTime
      if (now >= finishTime) Left(Foul.Timeout(player))
      else Right(withTimestamp(now))
    }
  }

  case class Turn(player: Player, board: Board, currentSubBoard: Point, finishTime: Long, currentTime: Long)extends InProgress {
    override def apply(point: Point)(implicit time: TimeProvider): Either[Foul, WopState] = {
      val sb = board(currentSubBoard)
      sb(point) match {
        case XO.Empty =>
          val updatedSb = sb.set(point, player.xo)
          val updatedBoard = board.set(currentSubBoard, updatedSb)
          updatedBoard.status match {
            case Status.Draw =>
              Right(Draw(updatedBoard))
            case Status.NotFinished if updatedBoard(point).status.finished =>
              Right(Select(player.next, updatedBoard, time.currentTime + TurnTime, time.currentTime))
            case Status.NotFinished =>
              Right(Turn(player.next, updatedBoard, point, time.currentTime + TurnTime, time.currentTime))
            case Status.Finished(_) =>
              Right(Win(player, updatedBoard))
          }
        case _ => Left(Foul.CellIsNotEmpty)
      }
    }

    override def equals(rhs: Any): Boolean = rhs match {
      case turn: Turn => turn.player == player && turn.currentSubBoard == currentSubBoard && turn.board == board
      case _ => false
    }

    def withTimestamp(t: Long) = copy(currentTime = t)
  }

  case class Select(player: Player, board: Board, finishTime: Long, currentTime: Long) extends InProgress {
    override def apply(point: Point)(implicit time: TimeProvider): Either[Foul, WopState] = board(point).status match {
      case Status.NotFinished => Right(Turn(player, board, point, time.currentTime + TurnTime, time.currentTime))
      case _ => Left(Foul.AlreadyFinished)
    }

    override def equals(rhs: Any): Boolean = rhs match {
      case sel: Select => sel.player == player && sel.board == board
      case _ => false
    }

    def withTimestamp(t: Long) = copy(currentTime = t)
  }

  case class Win(player: Player, board: Board) extends WopState

  case class Draw(board: Board) extends WopState

  val emptyBoard: Board = {
    val subBoard = TicTacToe(Size, XO.empty)
    TicTacToe(Size, subBoard)
  }

  def initial(implicit time: TimeProvider): WopState.InProgress =
    Select(Player.P1, emptyBoard, time.currentTime + TurnTime, time.currentTime)
}

sealed trait WopState {
  override def toString: String = {
    val sizeRange = 0 until WopState.Size
    def linesString(subBoards: Seq[WopState.SubBoard], y: Int): String = {
      val lines = subBoards map { sb =>
        val xs = for (x <- sizeRange)
          yield
            sb.status match {
              case Status.Finished(p) => p
              case _ => sb(x, y)
            }
        xs.mkString(" ")
      }
      lines.mkString(" | ")
    }
    def boardString(board: WopState.Board, topLine: String): String = {
      val Seq(y0, y1, y2) = board.matrix.sliding(WopState.Size, WopState.Size).toSeq
      s""" $topLine
         | +-------+-------+-------+
         | | ${linesString(y0, 0)} |
         | | ${linesString(y0, 1)} |
         | | ${linesString(y0, 2)} |
         | +-------+-------+-------+
         | | ${linesString(y1, 0)} |
         | | ${linesString(y1, 1)} |
         | | ${linesString(y1, 2)} |
         | +-------+-------+-------+
         | | ${linesString(y2, 0)} |
         | | ${linesString(y2, 1)} |
         | | ${linesString(y2, 2)} |
         | +-------+-------+-------+
         |""".stripMargin
    }
    this match {
      case WopState.Turn(player, board, currentSubBoard, dl, ct) =>
        boardString(board, s"$player turn in $currentSubBoard.")
      case WopState.Select(player, board, dl, ct) =>
        boardString(board, s"$player select field.")
      case WopState.Win(player, board) =>
        boardString(board, s"$player wins.")
      case WopState.Draw(board) =>
        boardString(board, "Draw")
    }
  }
}
