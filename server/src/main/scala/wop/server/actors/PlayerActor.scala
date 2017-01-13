package wop.server.actors

import akka.actor.{Actor, ActorLogging, ActorRef, PoisonPill, Props, Terminated}

import scala.concurrent.duration._
import wop.game.WopState
import akka.pattern.ask

import scala.language.postfixOps

/**
  * @author Aleksey Fomkin <aleksey.fomkin@gmail.com>
  */
class PlayerActor(matchMaking: ActorRef, callback: PlayerActor.Notification => Unit)
  extends Actor with ActorLogging {

  import PlayerActor._
  import context.dispatcher

  val receive: Receive = {
    // Commands from UI
    case Command.SetName(name) =>
      log.info(s"$name was enter the server")
      context.become(hasNameReceive(name))
  }

  def hasNameReceive(name: String): Receive = {
    // Command from another actors
    case NotifyDisconnect => log.info(s"$name was disconnected")
    case init: EnterGame =>
      callback(Notification.GameStated(init.state, init.yourRole, name, init.enemyName))
      context.become(inGameReceive(name, init), discardOld = false)
    // Commands from UI
    case Command.PlayWithBot => matchMaking ! MatchMakingActor.WantToPlayWithBot(self, name)
    case Command.StartMatchMaking =>
      matchMaking.ask(MatchMakingActor.WantToPlay(self, name))(5 seconds) foreach {
        case MatchMakingActor.WantToPlayResponse(online) =>
          callback(Notification.MatchMakingStarted(name, online))
      }
  }

  def inGameReceive(name: String, init: EnterGame): Receive = {
    case Command.Point(point) =>
      init.game ! point
    case TickGame(updatedState) =>
      callback(Notification.StateUpdated(updatedState))
    case updatedState: WopState =>
      callback(Notification.StateUpdated(updatedState))
    case timeout: WopState.Foul.Timeout =>
      log.info(s"$name was disconnected by timeout")
      callback(Notification.Timeout(timeout.player))
    case foul: WopState.Foul =>
      callback(Notification.Error(foul.toString))
    case GameFinished =>
      context.unbecome()
    case GameAborted =>
      callback(Notification.GameAborted)
      context.unbecome()
  }

  matchMaking ! MatchMakingActor.NotifyOnline
}

object PlayerActor {

  sealed trait Command

  object Command {

    case class Point(point: (Int, Int)) extends Command

    case class SetName(name: String) extends Command

    case object StartMatchMaking extends Command

    case object PlayWithBot extends Command
  }

  sealed trait Notification

  object Notification {

    case class MatchMakingStarted(name: String, playersOnline: Int) extends Notification

    case class GameStated(initialState: WopState, yourRole: WopState.Player, yourName: String, enemyName: String)
      extends Notification

    case object GameAborted extends Notification

    case class StateUpdated(state: WopState) extends Notification

    case class Error(message: String) extends Notification

    case class Timeout(player: WopState.Player) extends Notification
  }

  case class EnterGame(game: ActorRef, state: WopState.InProgress, yourRole: WopState.Player, enemyName: String)

  case class TickGame(state: WopState.InProgress)

  case object GameAborted

  case object GameFinished

  case object NotifyDisconnect

  def props(matchMakingRef: ActorRef)(callback: PlayerActor.Notification => Unit) =
    Props(classOf[PlayerActor], matchMakingRef, callback)
}
