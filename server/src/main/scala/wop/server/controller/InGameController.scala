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
class InGameController() {

  import UserState.effects._

  val fieldClick: EventFactory[Point] = point => eventWithAccess('mouseup) { access =>
    access.publish(PlayerActor.Command.Point(point))
    noTransition
  }

  val startMatchmakingClick: EventFactory[Unit] = point => eventWithAccess('click) { access =>
    access.publish(PlayerActor.Command.StartMatchMaking)
    noTransition
  }
}
