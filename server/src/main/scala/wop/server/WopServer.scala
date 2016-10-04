package wop.server

import java.util.concurrent.Executors

import akka.actor._
import korolev.{KorolevServer, Shtml}
import org.slf4j.LoggerFactory
import wop.server.actors.PlayerActor.Notification._
import wop.server.actors.{MatchMakingActor, PlayerActor}
import wop.server.controller.{EnterNickNameController, InGameController, MatchMakingController}
import wop.server.view.{EnterNickNameView, InGameView, MatchMakingView}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.language.postfixOps

object WopServer extends App with Shtml {

  val logger = LoggerFactory.getLogger(WopServer.getClass)
  logger.info("Starting Wizards of Portal")

  implicit val ec = ExecutionContext.fromExecutorService(Executors.newCachedThreadPool())
  implicit val system = ActorSystem("wop-server")
  val matchmaking = system.actorOf(MatchMakingActor.props)

  KorolevServer[UserState](
    host = "0.0.0.0",
    initialState = UserState.EnterNickName(false),
    initRender = { access =>
      val actor = {
        val props = PlayerActor.props(matchmaking) {
          case MatchMakingStarted(name, online) =>
            println("Callback matchmaking stated")
            access.dux { case _ => UserState.Matchmaking(name, online) }
          case GameStated(state, role, name, enemyName) =>
            access.dux { case _ => UserState.InGame(role, name, enemyName, state, 30 seconds) }
          case Error(message) =>
          case GameAborted =>
            access.dux {
              case _ =>
                UserState.GameAborted("Enemy has left the game")
            }
          case StateUpdated(wopState) =>
            access.dux {
              case userState: UserState.InGame =>
                userState.copy(wopState = wopState)
            }
        }
        system.actorOf(props)
      }

      access.dux onDestroy { () =>
        println("On destroy")
        actor ! PoisonPill
      }

      val enterNickNameController = new EnterNickNameController(access, actor)
      val enterNickNameView = new EnterNickNameView(enterNickNameController)
      val inGameController = new InGameController(access, actor)
      val inGameView = new InGameView(inGameController)
      val matchMakingContoller = new MatchMakingController(access, actor)
      val matchMakingView = new MatchMakingView(matchMakingContoller)

      enterNickNameView.render orElse
        inGameView.render orElse
        matchMakingView.render
    },
    head = 'head (
      'link ('href /= "https://fonts.googleapis.com/css?family=Open+Sans", 'rel /= "stylesheet"),
      'link ('href /= "http://code.ionicframework.com/ionicons/2.0.1/css/ionicons.min.css", 'rel /= "stylesheet"),
      'style (
        """

        * {
            /* Disable selection/Copy of UIWebView */
            -webkit-touch-callout: none;
            -webkit-user-select: none;
            -webkit-tap-highlight-color: rgba(0,0,0,0);
        }

        input,textarea {
            /* Exception for input areas */
            -webkit-touch-callout: default !important;
            -webkit-user-select: text !important;
        }

        html, body {
          font-family: 'Open Sans', sans-serif;
          height: 100%;
          display: flex;
          justify-content: center;
          align-items: center;
          flex-direction: column;
        }

        @keyframes rotating {
          100% { transform: rotate(360deg); }
        }

        .loading {
          display: inline-block;
          animation: rotating 2s linear infinite;
        }

        .highlight-cell {
          transition: background-color 0.3s;
        }

        .highlight-cell:hover {
          background-color: #ffb3b3;
        }
        """
      )
    )
  )
}
