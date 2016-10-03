package wop.server.controller

import akka.actor.ActorRef
import korolev.EventResult._
import korolev.Korolev._
import wop.game._
import wop.server.UserState
import wop.server.actors.PlayerActor

/**
  * @author Aleksey Fomkin <aleksey.fomkin@gmail.com>
  */
class InGameController(access: KorolevAccess[UserState], playerActor: ActorRef) {

  val fieldClick: EventFactory[Point] = access.event("click") { point =>
    playerActor ! point
    noTransition
  }

  val startMatchmakingClick: EventFactory[Unit] = access.event("click") { point =>
    playerActor ! PlayerActor.Command.StartMatchMaking
    noTransition
  }
}
