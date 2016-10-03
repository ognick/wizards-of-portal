package wop.server.actors

import akka.actor.{Actor, ActorRef, Props, Terminated}

import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.util.Random

/**
  * @author Aleksey Fomkin <aleksey.fomkin@gmail.com>
  */
class MatchMakingActor extends Actor {

  import MatchMakingActor._

  case class Player(ref: ActorRef, name: String)
  type Competitors = (ActorRef, ActorRef)
  type Game = ActorRef

  var online = mutable.HashSet.empty[ActorRef]
  var waitingList = List.empty[Player]
  val gamesMap = TrieMap.empty[Game, Competitors]

  val receive: Receive = {
    case NotifyOnline =>
      online.add(sender())
      context.watch(sender())
    case WantToPlay(player, name) =>
      waitingList = Player(player, name) :: waitingList
      sender() ! WantToPlayResponse(online.size)
      println(waitingList)
      matchMake()
    case WantToPlayWithBot(player, name) =>
      // Remove player from waiting list
      removeFromWaitingList(player)
      val botName = "bot_" + Random.alphanumeric.take(5).mkString
      val botRef = context.actorOf(BotPlayerActor.props(self))
      val game = context.actorOf(GameActor.props(player, botRef, name, botName))
      gamesMap.put(game, (player, botRef))
    case Terminated(ref) =>
      removeFromWaitingList(ref)
      online.remove(ref)
      gamesMap.remove(ref)
  }

  def matchMake() = Random.shuffle(waitingList) match {
    case Player(aRef, aName) :: Player(bRef, bName) :: xs =>
      val game = context.actorOf(GameActor.props(aRef, bRef, aName, bName))
      gamesMap.put(game, (aRef, bRef))
      waitingList = xs
    case _ =>
  }

  def removeFromWaitingList(player: ActorRef) = {
    waitingList = waitingList.filter(_.ref != player)
  }
}

object MatchMakingActor {

  case object NotifyOnline
  case class WantToPlay(userActor: ActorRef, userName: String)
  case class WantToPlayResponse(playersOnline: Int)
  case class WantToPlayWithBot(userActor: ActorRef, userName: String)

  val props = Props(classOf[MatchMakingActor])
}
