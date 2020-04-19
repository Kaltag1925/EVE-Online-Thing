package adt

class LLRearEntryPriorityQueue[A](p: (A, A) => Boolean) extends PriorityQueue[A] {
  import LLRearEntryPriorityQueue._
  private var end: Node[A] = new Node(null.asInstanceOf[A], null, null)
  end.next = end
  end.prev = end

  override def enqueue(a: A): Unit = {
    var rover = end.prev
    while (rover != end && p(a, rover.data)) rover = rover.prev
    val newNode = new Node(a, rover, rover.next)
    rover.next.prev = newNode
    rover.next = newNode
  }

  override def dequeue(): A = {
    val ret = end.next.data
    end.next.next.prev = end
    end.next = end.next.next
    ret
  }

  override def peek: A = end.next.data

  override def isEmpty: Boolean = end.next == end
}

object LLRearEntryPriorityQueue {
  class Node[A] (val data: A, var prev: Node[A], var next: Node[A])
}