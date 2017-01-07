package wop.server.view

import korolev._
import wop.server.UserState
import wop.server.controller.MatchMakingController

/**
  * @author Aleksey Fomkin <aleksey.fomkin@gmail.com>
  */
class MatchMakingView(controller: MatchMakingController) {

  import wop.server.components.default._

  val render: Render[UserState] = {
    case UserState.Matchmaking(nickname, playersOnline) =>
      'body(
        'div(
          'div(icon("load-c"), 'class /= "loading"),
          'strong(s"Looking for opponent.", 'style /= "padding-left: 10px"),
          'style /= "padding-bottom: 10px"
        ),
        'div(s"Players online: $playersOnline."),
        'button('style /= "margin-top: 10px",
          "Play with bot",
          controller.playWithBotClick
        )
      )
  }
}
