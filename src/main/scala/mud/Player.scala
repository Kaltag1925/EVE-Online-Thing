package mud

import java.io.{BufferedReader, InputStreamReader, OutputStream, PrintStream}
import java.net.Socket

import akka.actor.{Actor, ActorRef, PoisonPill}
import mud.CharacterCommon.AttackIncoming
import mud.PlayerManager.CheckInput
import mud.Room.TakeItem

import scala.collection.mutable
import scala.util.Random

object Player {

  case class PlayerDisconnected()
}

class Player(private var inventory: mutable.Buffer[Item], private var _room: ActorRef, socket: Socket, name: String, private var _health: Int) extends Actor {

  private val inStream = new BufferedReader(new InputStreamReader(socket.getInputStream))
  private val outStream = new PrintStream(socket.getOutputStream)

  _room ! Room.CharacterEntered

  import Player._

  def receive = {
    case CheckInput =>
      if (!socket.isClosed) {
        if (inStream.ready()) processCommand(inStream.readLine)
      }

    case CharacterCommon.Output(out) =>
      printOutput(out)

    case CharacterCommon.MoveToRoom(room) =>
      _room ! Room.CharacterExit
      _room = room
      room ! Room.CharacterEntered

    case CharacterCommon.AddItem(item) =>
      addToInventory(item)

    case CharacterCommon.EnterGame =>
      _room ! Room.Description
      println(s"$name has entered the game.")

    case Room.TakeItemResponse(itemName, itemOption) =>
      itemOption match {
        case Some(item) =>
          addToInventory(item)
          printOutput(s"You pick up the ${item.name}")
          _room ! Room.CharacterDoAction(s"scoops up the ${item.name}")
        case None =>
          printOutput(s"Could not find item named '$itemName'.")
      }

    case Room.GetExitResponse(nextRoomOption) =>
      nextRoomOption match {
        case Some(nextRoom) =>
          moveToRoom(nextRoom)
        case None =>
          printOutput("You cannot move in that direction.")
      }

    // Attacking //

    case Room.CharacterAvailableForAttackResponse(targetOpt, targetName) =>
      targetOpt match {
        case Some(targetRef) =>
          if (equippedItem.damage != 0) {
            _target = targetOpt
            targetRef ! CharacterCommon.AttackIncoming(equippedItem)
            Main.activityManager ! ActivityManager.Schedule(equippedItem.speed, (CharacterCommon.FinalAttackCheck, self))
            printOutput(s"You begin to attack $targetName.")
            _inCombat = true
          } else {
            printOutput(s"Your equipped item does no damage.")
          }

        case None => printOutput(s"You cannot attack $targetName")
      }

    case CharacterCommon.FinalAttackCheck =>
      if (!_fleeing) {
        _room ! Room.TargetStillInRoom(_target.get)
      } else {
        printOutput("You cannot attack while fleeing.")
      }

    case Room.TargetStillInRoomResponse(inRoom) =>
      if (inRoom) {
        _target.get ! CharacterCommon.AttackHit(equippedItem, _room)
      } else {
        _target match {
          case Some(target) =>
            printOutput(s"You fail to attack ${target.path.name}.")
            _target = None
            _inCombat = false
          case None =>
            println(s"ERROR: Player actor $self received message " + "\"CharacterCommon.AttackFailed\"" + s" from actor $sender but target was empty.")
        }
      }

    case CharacterCommon.AttackHit(item, room) =>
      if (_room == room) {
        printOutput(s"You are hit by ${sender.path.name} with their ${item.name} for ${item.damage} damage.")

        if (takeDamage(item)) {
          _room ! Room.CharacterKilledByCharacter(item, sender)
          sender ! CharacterCommon.AttackKilled
          printOutput(s"You were killed by ${sender.path.name} with their ${item.name}.")
          kill()
        } else {
          sender ! CharacterCommon.AttackSuccess
        }
      } else {
        sender ! CharacterCommon.AttackFailed
      }

    case CharacterCommon.AttackSuccess =>
      printOutput(s"You hit ${_target.get.path.name} with your ${equippedItem.name} for ${equippedItem.damage} damage.")
      println(s"$name hits ${_target.get.path.name} with their ${equippedItem.name} for ${equippedItem.damage} damage.")
      _target.get ! CharacterCommon.AttackIncoming(equippedItem)
      Main.activityManager ! ActivityManager.Schedule(equippedItem.speed, (CharacterCommon.FinalAttackCheck, self))
      printOutput(s"You begin to attack ${_target.get.path.name}.")

    case CharacterCommon.AttackKilled =>
      printOutput(s"You kill ${_target.get.path.name}")
      _target = None
      _inCombat = false

    case CharacterCommon.AttackFailed =>
      _target match {
        case Some(target) =>
          printOutput(s"You fail to attack ${target.path.name}.")
          _target = None
          _inCombat = false
        case None =>
          println(s"ERROR: Player actor $self received message " + "\"CharacterCommon.AttackFailed\"" + s" from actor $sender but target was empty.")
      }

    // Fleeing //

    case Room.CanFleeResponse(canFlee) =>
      if (!canFlee) {
        printOutput("There is no escape...")
      } else {
        printOutput("Warp drive active in 5 seconds.")
        Main.activityManager ! ActivityManager.Schedule(50, (CharacterCommon.Flee, self))
        _fleeing = true
      }

    case CharacterCommon.Flee =>
      _room ! Room.FleeRandomExit

    case Room.FleeRandomExitResponse(room) => {
      moveToRoom(room)
      printOutput(s"You successfully escape to ${room.path.name}")
      _fleeing = false
    }
      

    case m => println("Unproccessed message in Player " + self + ": " + m)
  }

  def printOutput(message: String): Unit = {
    if (!socket.isClosed) {
      message.split('\n').foreach(outStream.println)
    }
  }

  def processCommand(command: String): Unit = {
    command.split(" ").head match {
      // Movement //
      case "n" | "north" => moveCommand(Room.North)
      case "e" | "east" => moveCommand(Room.East)
      case "s" | "south" => moveCommand(Room.South)
      case "w" | "west" => moveCommand(Room.West)
      case "shortestPath" => Main.roomManager ! RoomManager.GetShortestPath(_room, command.drop(13))

      // Items //
      case "inv" => printOutput(inventoryListing)
      case "inventory" => printOutput(inventoryListing)
      case "drop" => dropItem(command.drop(5))
      case "get" => grabItem(command.drop(4))
      case "equip" => equipItem(command.drop(6))

      // Misc //
      case "look" => _room ! Room.Description

      // Chat //
      case "say" =>
        val message = command.drop(4)
        println(s"$name @ ${_room.path.name}: $message")
        _room ! Room.CharacterSpeak(command.drop(4))
      case "tell" =>
        val (recipient, message) = {
          val args = command.drop(5)
          args.splitAt(args.indexOf(' '))
        }
        if (recipient != name) {
          context.parent ! PlayerManager.Whisper(recipient, message)
        } else {
          printOutput("It is much more efficient to talk to yourself in person than via the chat system.")
        }

      // Combat //
      case "kill" =>
        if (!_fleeing) {
          val target = command.drop(5)
          if (target != name) {
            _room ! Room.CharacterAvailableForAttack(target)
          } else {
            printOutput("You can't kill yourself! Go get some help!")
          }
        } else {
          printOutput("You cannot attack while fleeing.")
        }

      case "flee" => // TODO: Add to commands.xml
        if (!_inCombat) {
          printOutput("Why are you running!?")
        }
        _room ! Room.CanFlee

      // Meta //
      case "help" =>
        printOutput(Command.commands.mkString("\n\n"))
      case "exit" =>
        printOutput("Goodbye")
        _room ! Room.CharacterExit
        context.stop(self)
      case _ => printOutput("Unrecognized Command")
    }
  }

  //// Attacking ////

  private var _target: Option[ActorRef] = None
  private var _inCombat = false
  private var _fleeing = false

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
    socket.close()
    context.stop(self) // So sad, cri ever time
  }

  //// Inventory ////

  private var _equippedItem: Item = NoItem

  def equippedItem = {
    _equippedItem
  }

  def equippedItem_=(newItem: Item) = {
    _equippedItem = newItem
  }

  def equipItem(itemName: String): Unit = {
    getFromInventory(itemName) match {
      case Some(item) =>
        equippedItem = item
        printOutput(s"You equip your ${item.name}")
      case None =>
        printOutput(s"You don't have an item named $itemName.")
    }
  }

  def unequipItem(): Unit = {
    if (_equippedItem != NoItem) {
      printOutput("You cannot equip nothing.")
    } else {
      printOutput(s"You unequip your ${_equippedItem.name}.")
      _equippedItem = NoItem
    }
  }

  def getFromInventory(itemName: String): Option[Item] = {
    inventory.find(_.lowerName.take(itemName.length) == itemName.toLowerCase())
  }

  def addToInventory(item: Item): Unit = {
    inventory += item
  }

  def removeFromInventory(item: Item): Unit = {
    inventory -= item
  }

  def inventoryListing: String = {
    val header = "Inventory:\n"
    val itemString = inventory.map(item => s"\t${item.name} - ${item.desc}").mkString("\n")
    header + itemString
  }

  def grabItem(itemName: String): Unit = {
    _room ! TakeItem(itemName.toLowerCase())
  }

  def dropItem(itemName: String): Unit = {
    getFromInventory(itemName) match {
      case Some(item) =>
        removeFromInventory(item)
        printOutput(s"You jettison your ${item.name}")
        _room ! Room.DropItem(item)
        _room ! Room.CharacterDoAction(s"jettisons the $itemName")
      case None =>
        printOutput(s"You do not have an item named $itemName")
    }
  }

  //// Movement ////

  def moveCommand(dir: Int): Unit = {
//    if (!_inCombat) {
      move(dir)
//    } else {
//      printOutput(s"Dread it. Run from it. Destiny arrives all the same. And now, it's here. Or should I say, ${_target.get.path.name} is.")
//    }
  }

  def move(dir: Int): Unit = {
    _room ! Room.GetExit(dir)
  }

  def moveToRoom(room: ActorRef): Unit = {
    _room ! Room.CharacterExit
    _room = room
    room ! Room.CharacterEntered
  }
}