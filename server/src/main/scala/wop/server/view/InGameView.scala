package wop.server.view

import korolev.{Korolev, Shtml, VDom}
import wop.game._
import wop.server.UserState
import wop.server.controller.InGameController

import scala.concurrent.duration.Duration

/**
  * @author Aleksey Fomkin <aleksey.fomkin@gmail.com>
  */
class InGameView(controller: InGameController) extends Shtml {

  import wop.server.components.default._

  trait BoardStyle {
    def tableStyle: String
    def trStyle: String
    def tdStyle: String
  }

  object boardStyle extends BoardStyle {
    val tableStyle = style(
      "border" -> "2px solid",
      "border-collapse" -> "collapse"
    )
    val tdStyle = style(
      "border" -> "1px solid",
      "margin" -> "0px",
      "padding" -> "0px",
      "vertical-align" -> "middle",
      "width" -> "90px",
      "height" -> "90px",
      "text-align" -> "center"
    )
    val trStyle = style("margin" -> "0px", "padding" -> "0px")
  }

  class SubboardStyle extends BoardStyle {
    val tableStyle = style(
      "border" -> "none",
      "border-collapse" -> "collapse"
    )
    val tdStyle = style(
      "border" -> "1px solid",
      "margin" -> "0px",
      "padding" -> "0px",
      "width" -> "30px",
      "height" -> "30px",
      "text-align" -> "center"
    )
    val trStyle = style(
      "margin" -> "0px",
      "padding" -> "0px"
    )
  }

  object defaultSubBoardStyle extends SubboardStyle

  object selectedSubBoardStyle extends SubboardStyle  {
    override val tableStyle: String = style(
      "border" -> "none",
      "border-collapse" -> "collapse",
      "background-color" -> "#eee"
    )
  }

  def renderXO(xo: WopState.XO): VDom = xo match {
    case WopState.XO.X => icon("android-close")
    case WopState.XO.O => icon("android-radio-button-off")
    case WopState.XO.Empty => <>
  }

  def renderTicTacToe[T](ttt: TicTacToe[T],
                         style: BoardStyle,
                         eventFactory: Option[Point => korolev.Event],
                         cellRenderer: (Point, T) => VDom) = {
    val `2d` = ttt.matrix.sliding(WopState.Size, WopState.Size)
    val rows = `2d`.zipWithIndex map { case (cells, y) =>
      val cols = cells.zipWithIndex map { case (cell, x) =>
        val p = (x, y)
        val active = eventFactory.map(f => f(p))
        'td(
          'style /= style.tdStyle,
          cellRenderer(p, cell),
          'class /= active
            .map(_ => "highlight-cell")
            .getOrElse("disabled"),
          active.getOrElse(<>)
        )
      }
      'tr('style /= style.trStyle, cols)
    }
    'table('style /= style.tableStyle, rows.toSeq)
  }

  def renderTimer(you: WopState.Player, current: WopState.Player, t: Duration) = {
    if (current == you) 'span(s", Time remaining: $t")
    else VDom.Empty
  }


  def renderSubBoard(ttt: TicTacToe[WopState.XO], eventFactory: Option[Point => korolev.Event]) = {
    val boardStyle =
      if (eventFactory.isEmpty) defaultSubBoardStyle
      else selectedSubBoardStyle
    ttt.status match {
      case TicTacToe.Status.Draw => <>
      case TicTacToe.Status.Finished(xo) => renderXO(xo)
      case TicTacToe.Status.NotFinished =>
        renderTicTacToe[WopState.XO](
          ttt, boardStyle, eventFactory, (_, xo) => renderXO(xo))
    }
  }

  val findOpponentButton = 'button(
    'style /= "margin-top: 10px",
    controller.startMatchmakingClick(()),
    "Find new opponent"
  )

  val render: Korolev.Render[UserState] = {
    case UserState.GameAborted(reason) =>
      'body(
        'h1("Game was aborted"),
        'p(reason),
        findOpponentButton
      )
    case UserState.InGame(yourRole, yourName, enemyName, wopState, timeRemaining) =>
      val enemyRole = yourRole.next

      def renderInProgress(inProgress: WopState.InProgress, boardEl: VDom) = {
        val title = inProgress match {
          case _: WopState.Select if inProgress.player == yourRole =>
            "Your turn. Select global field"
          case _: WopState.Turn if inProgress.player == yourRole =>
            "Your turn. Select local field"
          case _ => "Enemy turn"
        }
        'body(
          'h1(title),
          'div(
            'span('span(s"You: $yourName, Your figure: "),
              renderXO(yourRole.xo))
            //renderTimer(yourRole, inProgress.player, t)
          ),
          boardEl,
          'div(
            'span(s"Enemy: $enemyName, His figure: "),
            renderXO(enemyRole.xo)
            //renderTimer(enemyRole, inProgress.player, t)
          )
        )
      }

      wopState match {
        case inProgress @ WopState.Turn(_, board, currentSubBoard) =>
          renderInProgress(inProgress,
            renderTicTacToe[WopState.SubBoard](
              ttt = board,
              style = boardStyle,
              eventFactory = None,
              cellRenderer = {
                case (`currentSubBoard`, sb) => renderSubBoard(sb, Some(controller.fieldClick))
                case (_, sb) => renderSubBoard(sb, None)
              }
            )
          )
        case inProgress @ WopState.Select(_, board) =>
          renderInProgress(inProgress,
            renderTicTacToe[WopState.SubBoard](
              ttt = board,
              style = boardStyle,
              eventFactory = Some(controller.fieldClick),
              cellRenderer = (_, sb) => renderSubBoard(sb, None)
            )
          )
        case WopState.Win(winner, board) =>
          'body(
            'h1(
              if (yourRole == winner) "You win!"
              else "You lose!"
            ),
            renderTicTacToe[WopState.SubBoard](
              ttt = board,
              style = boardStyle,
              eventFactory = Some(controller.fieldClick),
              cellRenderer = (_, sb) => renderSubBoard(sb, None)
            ),
            findOpponentButton
          )
        case WopState.Draw(board) =>
          'body(
            'h1("Draw!"),
            renderTicTacToe[WopState.SubBoard](
              ttt = board,
              style = boardStyle,
              eventFactory = Some(controller.fieldClick),
              cellRenderer = (_, sb) => renderSubBoard(sb, None)
            ),
            findOpponentButton
          )
      }
  }
}
