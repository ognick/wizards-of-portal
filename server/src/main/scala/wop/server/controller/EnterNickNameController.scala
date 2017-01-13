package wop.server.controller

import wop.server.UserState
import wop.server.actors.PlayerActor

import scala.language.postfixOps
import korolev.blazeServer.defaultExecutor

/**
  * @author Aleksey Fomkin <aleksey.fomkin@gmail.com>
  */
class EnterNickNameController() {

  import UserState.effects._

  val inputId = elementId

  val formSubmit: Event = eventWithAccess('submit) { access =>
    access.property[String](inputId, 'value) foreach { value =>
      access.publish(PlayerActor.Command.SetName(value))
      access.publish(PlayerActor.Command.StartMatchMaking)
    }
    immediateTransition {
      case enterNickNameState: UserState.EnterNickName =>
        enterNickNameState.copy(loading = true)
    }
  }

}
