package mud

import adt.BinarySearchTree
import akka.actor.{Actor, ActorRef, Props}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.xml.{Node, XML}

object RoomManager {

  case object RoomsReady

  case class RoomsReadyResponse(ready: Boolean)

  case class RoomIsReady(exits: Array[Option[ActorRef]])

  case class GetRoom(name: String)

  case class GetRoomResponse(room: Option[ActorRef])
  
  case class GetShortestPath(start: ActorRef, end: String)

  val NoRoom = "-1"
}

class RoomManager extends Actor {
  private var roomsReady = false
  private var numRoomsReady = 0
  private val map = readRooms()
  private var exits: mutable.Map[ActorRef, Array[Option[ActorRef]]] = mutable.Map[ActorRef, Array[Option[ActorRef]]]()
  context.children.foreach(_ ! Room.AssignExits(map))

  import RoomManager._

  def receive = {
    case RoomsReady => sender ! RoomsReadyResponse(roomsReady)
    case RoomIsReady(roomExits) =>
      exits += sender -> roomExits
      numRoomsReady += 1
      roomsReady = numRoomsReady == context.children.size
    case GetRoom(name) => sender ! GetRoomResponse(map.get(name))
    
    case GetShortestPath(start, end) =>
      sender ! CharacterCommon.Output(shortestPathDirections(start, end).dropRight(1).mkString(" "))
    
    case m => println("Unproccessed message in RoomManager " + self + ": " + m)
  }

  def readRooms(): BinarySearchTree[String, ActorRef] = {
    val xml = XML.load("map.xml")

    def parseRoom(node: Node): (String, ActorRef) = {
      val name = (node \ "Name").text
      val desc = (node \ "Description").text

      def parseItem(node: Node): Item = {
        val name = (node \ "Name").text
        val desc = (node \ "Description").text

        val damage = {
          val raw = node \ "Damage"
          if (raw.nonEmpty) raw.text.toInt else 0
        }

        val speed = {
          val raw = node \ "Speed"
          if (raw.nonEmpty) raw.text.toInt * 10 else 0
        }

        Item(name, desc, damage, speed)
      }

      val items = (node \\ "Item").map(parseItem).to[ListBuffer]

      def parseExits(node: Node): Array[String] = {

        def readExit(dir: String): String = {
          val raw = (node \ dir).text
          if (raw != "") raw else NoRoom
        }

        val north = readExit("North")
        val east = readExit("East")
        val south = readExit("South")
        val west = readExit("West")

        Array(north, east, south, west)
      }

      val exitsNode = (node \ "Exits")
      val exits = if (exitsNode.nonEmpty) parseExits(exitsNode.head) else Array.fill(4)(NoRoom)

      name -> context.actorOf(Props(new Room(name, desc, items, exits)), name)
    }

    val ret = new BinarySearchTree[String, ActorRef](_ < _)
    (xml \\ "Room").map(parseRoom).foreach(ret += _)
    ret
  }

  def shortestPath(start: ActorRef, end: String): Option[ActorRef] = {
    def visitRoom(room: ActorRef, endRoom: ActorRef, visited: Set[ActorRef] = Set.empty): Option[ActorRef] = {
      if (!visited(room)) {
        if (endRoom == room) {
          Some(room)
        } else {
          exits.get(room) match {
            case Some(roomExits) =>
              val newVisited = visited + room
              var ret: Option[ActorRef] = None
              var i = 0
              while (ret.isEmpty && i < 4) {
                ret = roomExits(i) match {
                  case Some(exit) => visitRoom(exit, endRoom, newVisited)
                  case None => None
                }
              }
              ret
            case None => None
          }
        }
      } else {
        None
      }
    }

    map.get(end) match {
      case Some(endRoom) => visitRoom(start, endRoom)
      case None => None
    }
  }
  
  val dirs = Array("north", "east", "south", "west")
  
  def shortestPathDirections(start: ActorRef, end: String): List[String] = {
    def visitRoom(room: ActorRef, endRoom: ActorRef, visited: Set[ActorRef] = Set.empty): List[String]  = {
      if (!visited(room)) {
        if (endRoom == room) {
          List("end")
        } else {
          exits.get(room) match {
            case Some(roomExits) =>
              val newVisited = visited + room
              var ret: List[String] = Nil
              var i = 0
              while (ret.isEmpty && i < 4) {
                ret = roomExits(i) match {
                  case Some(exit) => 
                    val path = visitRoom(exit, endRoom, newVisited)
                    if (path.nonEmpty) {
                      dirs(i) :: path
                    } else {
                      Nil
                    }
                  case None => Nil
                }
                i += 1
              }
              ret
            case None => Nil
          }
        }
      } else {
        Nil
      }
    }

    map.get(end) match {
      case Some(endRoom) => visitRoom(start, endRoom)
      case None => Nil
    }
  }
}
