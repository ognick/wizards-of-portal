import org.scalatest._
import scala.concurrent.duration._
import wop.game.{TicTacToe, WopState}

class WOPStateSpec extends FlatSpec with Matchers {

  import WopState._

  implicit case object Time extends TimeProvider {
    def currentTime: Long = 0
    val stub: Long = 0
  }

  "Initial state of WOP" should "be `Select`" in {
    initial should be(Select(Player.P1, emptyBoard, Time.stub, Time.stub))
  }

  "Select" should "transit to Turn" in {
    val selectState = Select(Player.P1, emptyBoard, Time.stub, Time.stub)
    selectState((0, 0)) should be(Right(Turn(Player.P1, emptyBoard, (0, 0), Time.stub, Time.stub)))
  }

  "First player" should "should make turn and give turn to second player" in {
    val p00 = (0, 0)
    val p11 = (1, 1)
    val player1TurnState = Turn(Player.P1, emptyBoard, p00, Time.stub, Time.stub)
    player1TurnState(p11) should be {
      val subBoard = emptyBoard(p00).set(p11, WopState.XO.X)
      val board = emptyBoard.set(p00, subBoard)
      Right(Turn(Player.P2, board, p11, Time.stub, Time.stub))
    }
  }

  it should "should make turn to finished field and give second player to select new field" in {
    val p00 = (0, 0)
    val p11 = (1, 1)
    val boardWithFinishedCenter = emptyBoard.set(p11, TicTacToe(WopState.Size, WopState.XO.X))
    val player1TurnState = Turn(Player.P1, boardWithFinishedCenter, p00, Time.stub, Time.stub)
    player1TurnState(p11) should be {
      val subBoard = boardWithFinishedCenter(p00).set(p11, WopState.XO.X)
      val board = boardWithFinishedCenter.set(p00, subBoard)
      Right(Select(Player.P2, board, Time.stub, Time.stub))
    }
  }

}
