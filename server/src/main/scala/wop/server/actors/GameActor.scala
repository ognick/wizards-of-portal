package wop.server.actors

import akka.actor.{Actor, ActorRef, Props, Terminated}
import wop.game.WopState

/**
  * @author Aleksey Fomkin <aleksey.fomkin@gmail.com>
  */
class GameActor(playerA: ActorRef, playerB: ActorRef, aName: String, bName: String) extends Actor {

  var state = WopState.initial

  val playersMap = Map(
    playerA -> WopState.Player.P1,
    playerB -> WopState.Player.P2
  )

  context.watch(playerA)
  context.watch(playerB)

  playerA ! PlayerActor.EnterGame(self, state, playersMap(playerA), bName)
  playerB ! PlayerActor.EnterGame(self, state, playersMap(playerB), aName)

  def receive: Receive = {
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
            broadcast(newState)
            broadcast(PlayerActor.GameFinished)
            context.stop(self)
        }
      } else {
        sender() ! WopState.Foul.NotYourTurn
      }
    case Terminated(`playerA`) =>
      playerB ! PlayerActor.GameAborted
      context.stop(self)
    case Terminated(`playerB`) =>
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
