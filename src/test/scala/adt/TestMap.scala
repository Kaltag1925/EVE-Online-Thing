package adt

import org.junit._
import org.junit.Assert._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Random

class TestMap {
  var map: mutable.Map[Int, Int] = _

  @Before def createMap = {
    map = new BinarySearchTree(_ < _)
  }

  @Test def isEmptyOnCreate = {
    assertTrue(map.isEmpty)
  }

  @Test def addOneGetRemoveEmpty = {
    map += 3 -> 3
    assertFalse(map.isEmpty)
    assertEquals(3, map(3))
    map -= 3
    assertEquals(None, map.get(3))
    assertTrue(map.isEmpty)
  }

  @Test def addThreeCheckOrderAndRemove = {
    map += 3 -> 3
    map += 2 -> 2
    map += 4 -> 4
    assertFalse(map.isEmpty)
    val nums = Array(2, 3, 4)
    assertArrayEquals(nums, map.values.toArray)
    map -= 4
    assertEquals(None, map.get(4))
    map -= 3
    assertEquals(None, map.get(3))
    map -= 2
    assertEquals(None, map.get(2))
    assertTrue(map.isEmpty)
  }

  @Test def addManyCheckOrderRemoveRandom = {
    val nums = Array.fill(30)(Random.nextInt(1000)).distinct.map(_ -> Random.nextInt(1000))
    for (num <- nums) map += num
    assertFalse(map.isEmpty)

    def checkOrder(arr: Array[(Int, Int)]): Unit = {
      val orderedNums = arr.sortBy(_._1)
      assertArrayEquals(orderedNums.map(_._1), map.toArray.map(_._1))
    }

    checkOrder(nums)

    def getRandomAndRemove(expected: ListBuffer[(Int, Int)]): Unit = {
      if (expected.nonEmpty) {
        map -= expected.remove(Random.nextInt(expected.length))._1
        checkOrder(expected.toArray)
        getRandomAndRemove(expected)
      }
    }

    val list = ListBuffer[(Int, Int)]()
    list.insertAll(0, nums)
    getRandomAndRemove(list)

    assertTrue(map.isEmpty)
  }
}
