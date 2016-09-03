import org.scalatest._
import wop.game.TicTacToe
import wop.game.WopState
import wop.game.WopState.SubBoard
import wop.game.ai.WopSolver

class WopSolverSpec extends FlatSpec with Matchers {

  it should "be has eval" in {
    val field = Field(
      "xox",
      "_x_",
      "___"
    )
    WopSolver.heuristicEvalSubBoard(field) should be (13)
  }

  def Field(xs: String*): SubBoard = {
    TicTacToe(size = 3,
      matrix = xs.mkString.toVector map {
        case 'x' => WopState.XO.X
        case 'o' => WopState.XO.O
        case _ => WopState.XO.Empty
      }
    )
  }

}