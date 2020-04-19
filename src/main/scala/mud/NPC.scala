package mud

import java.io.{BufferedReader, InputStreamReader, PrintStream}
import java.net.Socket

import akka.actor.{Actor, ActorRef, PoisonPill}
import akka.util.Timeout
import mud.PlayerManager.CheckInput
import mud.Room.TakeItem

import scala.collection.mutable
import scala.util.Random

object NPC {
  case object ChangeRooms
}

class NPC(private var inventory: mutable.Buffer[Item], private var _room: ActorRef, name: String, private var _health: Int) extends Actor {

  _room ! Room.CharacterEntered

  import NPC._

  Main.activityManager ! ActivityManager.Schedule(200, (ChangeRooms, self))

  def receive = {
    case CharacterCommon.Output(_) =>
    /* Do nothing but here so that I don't have to differentiate
    between NPC and Player when sending output to the entire room */

    case CharacterCommon.MoveToRoom(room) =>
      _room ! Room.CharacterExit
      _room = room
      room ! Room.CharacterEntered

    case CharacterCommon.AddItem(item) =>
      addToInventory(item)

    case CharacterCommon.EnterGame =>
      println(s"$name has spawned.")

    case Room.TakeItemResponse(itemName, itemOption) =>
      itemOption match {
        case Some(item) =>
          addToInventory(item)
          _room ! Room.CharacterDoAction(s"scoops up the $itemName")
        case None =>
      }

    case ChangeRooms =>
      move(Random.nextInt(4))

    case Room.GetExitResponse(nextRoomOption) => nextRoomOption match {
      case Some(nextRoom) =>
        moveToRoom(nextRoom)
        Main.activityManager ! ActivityManager.Schedule(Random.nextInt(30)+100, (ChangeRooms, self))
      case None =>
        move(Random.nextInt(4))
    }

    // Combat //

    case CharacterCommon.AttackHit(item, room) =>
      if (_room == room) {
        if (takeDamage(item)) {
          _room ! Room.CharacterKilledByCharacter(item, sender)
          sender ! CharacterCommon.AttackKilled
          kill()
        } else {
          sender ! CharacterCommon.AttackSuccess
        }
      } else {
        sender ! CharacterCommon.AttackFailed
      }

    // Debug //

    case m => println("Unproccessed message in NPC " + self + ": " + m)
  }

  //// Health ////

  def health = _health

  def takeDamage(weapon: Item): Boolean = {
    health -= weapon.damage
    _health <= 0
  }

  def health_=(newHealth: Int) = {
    _health = newHealth
  }


  def kill(): Unit = {
    context.stop(self) // So sad, cri ever time
  }

  //// Combat ////

  private var _inCombat = false

  //// Inventory ////

  def getFromInventory(itemName: String): Option[Item] = {
    inventory.find(_.lowerName == itemName)
  }

  def addToInventory(item: Item): Unit = {
    inventory += item
  }

  def removeFromInventory(item: Item): Unit = {
    inventory -= item
  }

  def grabItem(itemName: String): Unit = {
    _room ! TakeItem(itemName.toLowerCase())
  }

  def dropItem(itemName: String): Unit = {
    getFromInventory(itemName) match {
      case Some(item) =>
        removeFromInventory(item)
        _room ! Room.DropItem(item)
        _room ! Room.CharacterDoAction(s"jettisons the $itemName")
      case None =>
    }
  }

  //// Movement ////

  def move(dir: Int): Unit = {
    _room ! Room.GetExit(dir)
  }

  def moveToRoom(room: ActorRef): Unit = {
    _room ! Room.CharacterExit
    _room = room
    room ! Room.CharacterEntered
  }
}