package wop.server.view

import korolev.{Korolev, Shtml}
import wop.server.UserState
import wop.server.controller.EnterNickNameController

/**
  * @author Aleksey Fomkin <aleksey.fomkin@gmail.com>
  */
class EnterNickNameView(controller: EnterNickNameController) extends Shtml {
  val render: Korolev.Render[UserState] = {
    case UserState.EnterNickName(loading) =>
      'body(
        'form(controller.formSubmit(()),
          'input(controller.inputId, 'type /= "text"),
          'button("Enter game")
        )
      )
  }
}
