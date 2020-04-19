package mud

import akka.actor.ActorRef

object CharacterCommon {
  case class AddItem(item: Item)

  case class MoveToRoom(room: ActorRef)

  case object EnterGame

  case class Output(message: String)

  case class AttackIncoming(weapon: Item)

  case class AttackCharacter(weapon: Item, target: String, room: ActorRef)

  case class AttackHit(weapon: Item, room: ActorRef)

  case object AttackFailed

  case object FinalAttackCheck

  case class IsAlive(isAlive: Boolean)

  case object AttackSuccess

  case object AttackKilled

  case object Flee

}
