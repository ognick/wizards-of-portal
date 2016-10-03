package wop.server.controller

import akka.actor.ActorRef
import korolev.EventResult._
import korolev.Korolev._
import wop.server.UserState
import wop.server.actors.PlayerActor

import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.postfixOps

/**
  * @author Aleksey Fomkin <aleksey.fomkin@gmail.com>
  */
class EnterNickNameController(access: KorolevAccess[UserState], playerActor: ActorRef) {

  val inputId = access.id(())

  val buttonClick: EventFactory[Unit] = access.event("click") { _ =>
    inputId[String]('value) foreach { value =>
      playerActor ! PlayerActor.Command.SetName(value)
      playerActor ! PlayerActor.Command.StartMatchMaking
    }
    immediateTransition {
      case enterNickNameState: UserState.EnterNickName =>
        enterNickNameState.copy(loading = true)
    }
  }

}
