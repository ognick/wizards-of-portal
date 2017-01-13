package wop.server

import korolev.Effects
import wop.game.WopState
import wop.server.actors.PlayerActor

import scala.concurrent.Future
import korolev.blazeServer.defaultExecutor

sealed trait UserState

object UserState {

  val effects = Effects[Future, UserState, PlayerActor.Command]

  case class EnterNickName(loading: Boolean) extends UserState

  case class FatalError(message: String) extends UserState

  case class Matchmaking(nickname: String, playersOnline: Int) extends UserState

  case class InGame(yourRole: WopState.Player,
                    yourName: String,
                    enemyName: String,
                    wopState: WopState)
      extends UserState

  case class GameAborted(reason: String) extends UserState
}
