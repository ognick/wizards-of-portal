package wop.server.controller

import akka.actor.ActorRef
import korolev.EventResult._
import korolev.Korolev.{EventFactory, KorolevAccess}
import wop.server.UserState
import wop.server.actors.PlayerActor

/**
  * @author Aleksey Fomkin <aleksey.fomkin@gmail.com>
  */
class MatchMakingController(access: KorolevAccess[UserState], actor: ActorRef) {
  val playWithBotClick: EventFactory[Unit] = access.event("click") { _ =>
    actor ! PlayerActor.Command.PlayWithBot
    noTransition
  }
}
