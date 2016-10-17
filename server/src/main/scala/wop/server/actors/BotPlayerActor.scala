package wop.server.actors

import akka.actor.{Actor, ActorRef, PoisonPill, Props, ActorLogging}
import wop.game.WopState
import wop.game.ai.WopSolver

import scala.concurrent.duration._
import scala.language.postfixOps

/**
  * @author Aleksey Fomkin <aleksey.fomkin@gmail.com>
  */
class BotPlayerActor(matchMaking: ActorRef) extends Actor with ActorLogging {

  import PlayerActor._
  import context.dispatcher

  val receive: Receive = {
    case EnterGame(game, wopState, yourRole, enemyName) =>
      tryMakeTurn(game, wopState, yourRole)
      context.become(inGameReceive(game, yourRole))
  }

  def inGameReceive(game: ActorRef, yourRole: WopState.Player): Receive = {
    case updatedState: WopState.InProgress => tryMakeTurn(game, updatedState, yourRole)
    case _: WopState.Foul.Timeout =>
    case _: TickGame =>
    case reason =>
      log.info(s"was killed because: $reason")
      self ! PoisonPill
  }

  def tryMakeTurn(game: ActorRef, state: WopState.InProgress, yourRole: WopState.Player) = {
    if (state.player == yourRole) {
      context.system.scheduler.scheduleOnce(1 second) {
        WopSolver.minMax(state) match {
          case Some(point) => game ! point
          case _ =>
        }
      }
    }
  }
}

object BotPlayerActor {
  def props(matchMaking: ActorRef) =
    Props(classOf[BotPlayerActor], matchMaking)
}
