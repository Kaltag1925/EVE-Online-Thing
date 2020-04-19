package mud

import java.io.{BufferedReader, PrintStream}
import java.net.Socket

import akka.actor.{Actor, ActorRef, PoisonPill, Props}
import akka.util.Timeout
import akka.pattern._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration._
import scala.util.{Failure, Success}

object PlayerManager {

  case object CheckInput

  case class NewPlayer(name: String, socket: Socket)

  case class RoomPackage(room: ActorRef)

  case class IsNameTaken(name: String)

  case class NameTaken(isTaken: Boolean)

  case class SetStartRoom(startRoom: ActorRef)

  case class SetStartRoomResponse(didSetStartRoom: Boolean)

  case class Whisper(playerNameToTell: String, message: String)
}

class PlayerManager extends Actor {

  import mud.PlayerManager._

  def receive = {

    case IsNameTaken(name) =>
      sender ! NameTaken(players.exists(_.path.name == name))

    case NewPlayer(name, socket) =>
      val player = context.actorOf(Props(new Player(ListBuffer[Item](), Main.startRoom, socket, name, 50)), name)
      println(s"Player $name joined")

    case CheckInput =>
      players.foreach(_ ! CheckInput)

    case Whisper(playerNameToTell, message) =>
      players.find(_.path.name == playerNameToTell) match {
        case Some(player) =>
          val senderName = sender.path.name
          sender ! CharacterCommon.Output(s"[You -> $playerNameToTell]: $message")
          player ! CharacterCommon.Output(s"[$senderName -> You]: $message")
          println(s"[$senderName -> $playerNameToTell]: $message")
        case None =>
          sender ! CharacterCommon.Output(s"Could not find a player named $playerNameToTell")
      }

    case m => println("Unproccessed message in PlayerManager " + self + ": " + m)
  }

  def players: Iterable[ActorRef] = {
   context.children
  }
}
