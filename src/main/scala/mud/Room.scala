package mud

import adt.BinarySearchTree
import akka.actor.{Actor, ActorRef}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

object Room {

  case object Description
  case class GetExit(dir: Int)
  case class GetExitResponse(room: Option[ActorRef])
  case class TakeItem(itemName: String)
  case class TakeItemResponse(itemName: String, item: Option[Item])
  case class DropItem(item: Item)

  case object CharacterEntered
  case object CharacterExit

  case class AssignExits(map: mutable.Map[String, ActorRef])

  case class CharacterSpeak(message: String)
  case class CharacterDoAction(action: String)

  case class CharacterAvailableForAttack(target: String)
  case class CharacterAvailableForAttackResponse(target: Option[ActorRef], targetName: String)
  case class TargetStillInRoom(target: ActorRef)
  case class TargetStillInRoomResponse(inRoom: Boolean)
  case class CharacterKilledByCharacter(weapon: Item, killer: ActorRef)

  case object CanFlee
  case class CanFleeResponse(canFlee: Boolean)
  case object FleeRandomExit
  case class FleeRandomExitResponse(room: ActorRef)


  private val Directions = List("north", "east", "south", "west")
  val North = 0
  val East = 1
  val South = 2
  val West = 3
}

class Room(
  val name:          String,
  private val desc:  String, //Private description cause description needs build
  private var items: mutable.Buffer[Item],
  exitNames: Array[String]) extends Actor {

  private var exits: Array[Option[ActorRef]] = Array.fill(4)(None)
  private var charactersInRoom = new ListBuffer[ActorRef] //TODO: Change to array buffer

  import Room._
  import RoomManager._

  def receive = {
    case Description => sender ! CharacterCommon.Output(description)
    case TakeItem(itemName) =>
      val item = getItem(itemName)
      if (item.nonEmpty) {
        removeItem(item.get)
      }
      sender ! TakeItemResponse(itemName, item)

    case AssignExits(map) =>
      for (i <- exits.indices) {
        exits(i) = map.get(exitNames(i))
      }
      sender ! RoomManager.RoomIsReady(exits)

    case GetExit(dir) => sender ! GetExitResponse(exits(dir))
    case DropItem(item) => dropItem(item)

    case CharacterEntered =>
      val enteringCharacterName = sender.path.name
      charactersInRoom.foreach(_ ! CharacterCommon.Output(s"$enteringCharacterName arrives at the jump gate."))
      charactersInRoom += sender
      sender ! CharacterCommon.Output(description)

    case CharacterExit =>
      val exitingCharacterName = sender.path.name
      charactersInRoom.foreach(_ ! CharacterCommon.Output(s"$exitingCharacterName jumps out of the system."))
      charactersInRoom -= sender

    case CharacterSpeak(message) =>
      val speakerName = sender.path.name
      for (character <- charactersInRoom) {
        if (character == sender) {
          character ! CharacterCommon.Output(s"You: $message")
        } else {
          character ! CharacterCommon.Output(s"$speakerName: $message")
        }
      }

    case CharacterDoAction(action) =>
      val actionerName = sender.path.name
      for (character <- charactersInRoom; if character != sender) {
        character ! CharacterCommon.Output(s"$actionerName $action")
      }

    // Attacking //

    case CharacterAvailableForAttack(targetPartialName) =>
      val (targetOpt, targetName) = findCharacterRefPartial(targetPartialName) match {
        case Some(targetRef) => Some(targetRef) -> targetRef.path.name
        case None => None -> targetPartialName
      }
      sender ! CharacterAvailableForAttackResponse(targetOpt, targetName)

    case TargetStillInRoom(target) =>
      val inRoom = charactersInRoom.contains(target)
      sender ! TargetStillInRoomResponse(inRoom)

    case CharacterKilledByCharacter(weapon, killer) =>
      charactersInRoom -= sender
      for (character <- charactersInRoom; if character != killer) {
        character ! CharacterCommon.Output(s"${killer.path.name} kills ${sender.path.name} with their ${weapon.name}.")
      }

    // Fleeing //

    case CanFlee =>
      sender ! Room.CanFleeResponse(exits.exists(_.nonEmpty))

    case FleeRandomExit =>
      val actualExits = exits.filter(_.nonEmpty)
      val randomExit = actualExits(Random.nextInt(actualExits.length)).get
      sender ! FleeRandomExitResponse(randomExit)

    case m => println("Unproccessed message in Room " + self + ": " + m)
  }


  def description: String = {
    val itemList = if (items.nonEmpty) items.map(_.name).mkString(", ") else "None"

    val validExits = (for ((option, dir) <- exits zip Room.Directions; if option.nonEmpty) yield dir).mkString(", ")

    val characterNames = charactersInRoom.map(_.path.name)

    s"$name\n$desc\nExits: $validExits\nItems: $itemList\n\nIn System:\n${characterNames.mkString("\n")}"
  }

  def findCharacterRefPartial(name: String): Option[ActorRef] = {
    charactersInRoom.find(_.path.name.take(name.length).toLowerCase() == name.toLowerCase())
  }

  def getItem(itemName: String): Option[Item] = {
    items.find(_.lowerName.take(itemName.length) == itemName)
  }
  def dropItem(item: Item): Unit = {
    items += item
  }
  def removeItem(item: Item): Unit = {
    items -= item
  }
}