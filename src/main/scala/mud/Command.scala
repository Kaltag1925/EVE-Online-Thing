package mud

import scala.xml.XML
import scala.xml.Node

case class Command(name: String,
              desc: String,
              aliases: List[String],
              params: List[String]) {
  
  override def toString: String = {
    s"$name - $desc\nUsage: $name ${params.mkString(" ")}\nAliases: ${aliases.mkString(",")}"
  }
  
}

object Command {
  val commands = readCommands
  
  def readCommands: List[Command] = {
    val xml = XML.load("commands.xml")

    def parseCommand(node: Node): Command = {
      val name = (node \ "Name").text
      val desc = (node \ "Description").text
      val aliases = (node \\ "Alias").map(_.text).toList
      val params = (node \\ "Parameter").map(_.text).toList

      Command(name, desc, aliases, params)
    }

    (xml \\ "Command").map(parseCommand).toList
  }
}