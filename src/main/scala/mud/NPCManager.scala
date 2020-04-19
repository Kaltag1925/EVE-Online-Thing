package mud

import akka.actor.{Actor, Props}

import scala.collection.mutable.ListBuffer
import scala.util.Random

object NPCManager {
  case object CreateNPC
}

class NPCManager extends Actor {

  import NPCManager._

  def receive = {
    case CreateNPC =>
      val name = createNPCName
      val a = context.actorOf(Props(new NPC(ListBuffer[Item](), Main.startRoom, name, 50)), name)
      println(s"NPC $name spawned")

    case m => println("Unproccessed message in NPCManager " + self + ": " + m)
  }

  def loadNames(): Array[String] = {
    val path = "src/main/resources/names.txt"
    val res = getClass.getResource(path)
    (if(res == null) {
      io.Source.fromFile(path)
    } else {
      io.Source.fromFile(res.toExternalForm())
    }).getLines().toArray
  }

  val names = loadNames()

  def createNPCName: String = {
    def RandomName: String = {
      names(Random.nextInt(names.length))
    }

    val name = RandomName

    if (!context.children.exists(_.path.name == name)) {
      name
    } else {
      createNPCName
    }
  }
}
