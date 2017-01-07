package wop.server.controller

import wop.server.UserState
import wop.server.actors.PlayerActor

/**
  * @author Aleksey Fomkin <aleksey.fomkin@gmail.com>
  */
class MatchMakingController() {
  import UserState.effects._
  val playWithBotClick: Event = eventWithAccess('click) { access =>
    access.publish(PlayerActor.Command.PlayWithBot)
    noTransition
  }
}
