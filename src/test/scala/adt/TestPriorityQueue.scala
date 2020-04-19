package adt

import org.junit._
import org.junit.Assert._

import scala.util.Random

class TestPriorityQueue {
  var pq: BinaryHeapPQ[Int] = _

  @Before def createPriorityQueue = {
    pq = new BinaryHeapPQ[Int](_ <= _)
  }

  @Test def emptyOnCreation = {
    assertTrue(pq.isEmpty)
  }

  @Test def addOneRemoveOne = {
    pq.enqueue(3)
    assertFalse(pq.isEmpty)
    assertEquals(3, pq.peek)
    assertEquals(3, pq.dequeue())
    assertTrue(pq.isEmpty)
  }

  @Test def addThreeCheckOrderedRemoveThree = {
    pq.enqueue(2)
    pq.enqueue(3)
    assertTrue(pq.peek == 2)
    assertFalse(pq.isEmpty)
    pq.enqueue(1)
    assertTrue(pq.peek == 1)
    assertFalse(pq.isEmpty)
    assertEquals(1, pq.dequeue())
    assertEquals(2, pq.dequeue())
    assertEquals(3, pq.dequeue())
    assertTrue(pq.isEmpty)
  }

  @Test def addManyRemoveMany = {

  }
}
