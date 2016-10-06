package wop.server.actors

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Terminated}
import scala.concurrent.duration._
import wop.game.WopState

/**
  * @author Aleksey Fomkin <aleksey.fomkin@gmail.com>
  */
class GameActor(playerA: ActorRef, playerB: ActorRef, aName: String, bName: String)
  extends Actor with ActorLogging {

  import context.dispatcher

  implicit case object Time extends WopState.TimeProvider {
    def currentTime: Long = System.currentTimeMillis()
  }

  var state = WopState.initial
  val vsString = s"$aName vs $bName"
  val playersMap = Map(
    playerA -> WopState.Player.P1,
    playerB -> WopState.Player.P2
  )

  log.info(s"$vsString: started")

  context.watch(playerA)
  context.watch(playerB)

  playerA ! PlayerActor.EnterGame(self, state, playersMap(playerA), bName)
  playerB ! PlayerActor.EnterGame(self, state, playersMap(playerB), aName)

  case object Tick

  context.system.scheduler.schedule(500 milliseconds, 500 milliseconds) {
    self ! Tick
  }

  def receive: Receive = {
    case Tick => state.nextTimeState() match {
        case Left(foul: WopState.Foul.Timeout) =>
          broadcast(foul)
          broadcast(PlayerActor.GameFinished)
          context.stop(self)
        case Right(s: WopState.InProgress) =>
          state = s
          broadcast(PlayerActor.TickGame(s))
      }
    case (x: Int, y: Int) =>
      if (state.player == playersMap(sender())) {
        state((x, y)) match {
          case Left(foul: WopState.Foul) =>
            // Send foul to player
            sender() ! foul
          case Right(newState: WopState.InProgress) =>
            state = newState
            broadcast(newState)
          case Right(newState) =>
            newState match {
              case WopState.Win(WopState.Player.P1, _) => log.info(s"$vsString: $aName wins")
              case WopState.Win(WopState.Player.P2, _) => log.info(s"$vsString: $bName wins")
              case WopState.Draw(_) => log.info(s"$vsString: draw")
              case _ =>
            }
            broadcast(newState)
            broadcast(PlayerActor.GameFinished)
            context.stop(self)
        }
      } else {
        sender() ! WopState.Foul.NotYourTurn
      }
    case Terminated(`playerA`) =>
      log.info(s"$vsString: game was aborted cause $aName goes offline")
      playerB ! PlayerActor.GameAborted
      context.stop(self)
    case Terminated(`playerB`) =>
      log.info(s"$vsString: game was aborted cause $bName goes offline")
      playerA ! PlayerActor.GameAborted
      context.stop(self)
  }

  def broadcast(message: Any) = {
    playerA ! message
    playerB ! message
  }
}

object GameActor {
  def props(playerA: ActorRef, playerB: ActorRef, aName: String, bName: String) =
    Props(classOf[GameActor], playerA, playerB, aName, bName)
}
