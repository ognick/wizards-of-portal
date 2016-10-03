package wop.server

import wop.game.WopState

import scala.concurrent.duration.Duration

sealed trait UserState

object UserState {

  case class EnterNickName(loading: Boolean) extends UserState

  case class FatalError(message: String) extends UserState

  case class Matchmaking(nickname: String, playersOnline: Int) extends UserState

  case class InGame(yourRole: WopState.Player,
                    yourName: String,
                    enemyName: String,
                    wopState: WopState,
                    timeRemaining: Duration)
      extends UserState

  case class GameAborted(reason: String) extends UserState
}
