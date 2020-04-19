package mud

import java.io.{BufferedReader, InputStreamReader, PrintStream}
import java.net.ServerSocket

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern._
import akka.util.Timeout

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success}

object Main extends App {
  val system = ActorSystem("Game")
  val playerManager = system.actorOf(Props[PlayerManager], "PlayerManager")
  val activityManager = system.actorOf(Props[ActivityManager], "ActivityManager")
  val roomManager = system.actorOf(Props[RoomManager], "RoomManager")
  val npcManager = system.actorOf(Props[NPCManager], "NPCManager")

  val startRoomName = "Yulai"

  implicit val timeout = Timeout(1.second)
  implicit val ec = system.dispatcher

  def findStartRoom(): ActorRef = {
    def waitForRoomsReady(): ActorRef = {
      val roomsReady = roomManager ? RoomManager.RoomsReady
      val response = Await.result(roomsReady, 1.second)
      response match {
        case RoomManager.RoomsReadyResponse(areRoomsReady) =>
          if (areRoomsReady) {
            println("Rooms ready; getting start room.")
            getRoom()
          } else {
            println("Rooms are not ready.")
            Thread.sleep(250)
            waitForRoomsReady()
          }
        case m => println("Unexpected Response from RoomsReady"); waitForRoomsReady()
      }

    }

    def getRoom(): ActorRef = {
      val startRoomResponse = roomManager ? RoomManager.GetRoom(startRoomName)
      val response = Await.result(startRoomResponse, 1.second)
      response match {
        case RoomManager.GetRoomResponse(startRoomOpt) =>
          startRoomOpt match {
            case Some(room) =>
              println("Got start room; setting start room.")
              room
            case None =>
              println("Could not get start room")
              Thread.sleep(250)
              getRoom()
          }
        case m => println("Unexpected Response from GetRoom"); getRoom()
      }
    }

    waitForRoomsReady()
  }

  println("Waiting for rooms to ready.")
  val startRoom = findStartRoom()
  println("Finished set up, starting server on port 3621")
  val server = new ServerSocket(3621)

  system.scheduler.schedule(100.milli, 100.milli)(playerManager ! PlayerManager.CheckInput)
  system.scheduler.schedule(100.milli, 100.milli)(activityManager ! ActivityManager.CheckQueue)
  system.scheduler.schedule(1.second, 25.second)(npcManager ! NPCManager.CreateNPC)

  //// Admin Commands ////

  Future {
    while (true) {
      val input = readLine()
      input.split(" ").head.toLowerCase match {
        case "spawnnpc" => npcManager ! NPCManager.CreateNPC
        case "help" => "To be implemented"
        case unknownCommand => println(s"Unknown command $unknownCommand, type " + "\"help\"" + " for commands.")
      }
    }
  }

  while (true) {
    println("Server started, listening for players")
    val playerSocket = server.accept()
    Future {
      println(s"${playerSocket.getInetAddress} is connecting")
      val in = new BufferedReader(new InputStreamReader(playerSocket.getInputStream))
      val out = new PrintStream(playerSocket.getOutputStream)

      def askForName(): Unit = {
        out.println("What is your name?")
        var name = in.readLine()

        val nameStatus = playerManager ? PlayerManager.IsNameTaken(name)
        nameStatus.onComplete {
          case Success(response) => response match {
            case PlayerManager.NameTaken(isTaken) =>
              if (!isTaken) {
                playerManager ! PlayerManager.NewPlayer(name, playerSocket)
              } else {
                out.println(s"$name is already taken. Please choose another name.")
                askForName()
              }
            case o => println("Unexpected response in Main from PlayerManager ? NewPlayer(name: " + name + ", sock: " + playerSocket + "). Got: " + o)
          }
          case Failure(ex) => println(ex)
        }
      }

      askForName()
    }
  }
}
