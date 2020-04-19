package mud

import adt.{BinaryHeapPQ, LLRearEntryPriorityQueue}
import akka.actor.{Actor, ActorRef}
import mud.ActivityManager.CheckQueue

object ActivityManager {

  // Activity Managing //
  case object CheckQueue
  case class Schedule(delay: Int, send: (Any, ActorRef)*)

}

class ActivityManager extends Actor {

  private var updateCount = 0
  case class Event(time: Int, send: Seq[(Any, ActorRef)]) {
    def fire(): Unit = {
      for ((message, actor) <- send) {
        actor ! message
      }
    }
  }
  private var eventQueue = new BinaryHeapPQ[Event](_.time <= _.time)

  import ActivityManager._
  def receive = {
    case CheckQueue =>
      while (!eventQueue.isEmpty && eventQueue.peek.time <= updateCount) {
        eventQueue.dequeue().fire()
      }
      updateCount += 1

    case Schedule(delay, send) =>
      schedule(delay, send)

    case m =>
      println("Unproccessed message in ActivityManager " + self + ": " + m)
  }

  def schedule(delay: Int, send: (Any, ActorRef)*): Unit = {
    eventQueue.enqueue(Event(delay + updateCount, send))
  }
}
