import org.scalatest._
import wop.game.TicTacToe

class TicTacToeSpec extends FlatSpec with Matchers {

  "Field" should "be finished when it has horizontal line" in {
    val field = Field(
      "oxo",
      "xxx",
      "o_o"
    )
    field.status should be (TicTacToe.Status.Finished(X))
  }

  it should "be finished when it has vertical line" in {
    val field = Field(
      "_xo",
      "xxo",
      "o_o"
    )
    field.status should be (TicTacToe.Status.Finished(O))
  }

  it should "be finished when it has right to left diagonal line" in {
    val field = Field(
      "oxx",
      "xxo",
      "x_o"
    )
    field.status should be (TicTacToe.Status.Finished(X))
  }

  it should "be finished when it has left to right diagonal line" in {
    val field = Field(
      "x__",
      "_x_",
      "__x"
    )
    field.status should be (TicTacToe.Status.Finished(X))
  }

  it should "be not finished when contains no line" in {
    val field = Field(
      "oxx",
      "x_o",
      "x_o"
    )
    field.status should be (TicTacToe.Status.NotFinished)
  }

  it should "be draw when contains no line and no empty cells" in {
    val field = Field(
      "oxo",
      "xxo",
      "xox"
    )
    field.status should be (TicTacToe.Status.Draw)
  }

  val NA = 0
  val X = 1
  val O = 2

  def Field(xs: String*): TicTacToe[Int] = {
    implicit object Int extends TicTacToe.Item[Int] {
      def compare(a: Int, b: Int): Boolean = a == b
      def nonEmpty(v: Int): Boolean = v != 0
    }
    TicTacToe(size = 3,
      matrix = xs.mkString.toVector map {
        case 'x' => 1
        case 'o' => 2
        case _ => 0
      }
    )
  }
}
