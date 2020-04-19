package adt

import scala.reflect.ClassTag

class BinaryHeapPQ[A: ClassTag](higherPriority: (A, A) => Boolean) extends PriorityQueue[A] {

  private var arr = new Array[A](10)
  private var end = 1

  override def enqueue(a: A): Unit = {
    var bubble = end
    while (bubble > 1 && higherPriority(a, arr(bubble/2))) {
      arr(bubble) = arr(bubble/2)
      bubble /= 2
    }
    arr(bubble) = a

    end += 1
  }

  override def dequeue(): A = {
    if (arr.length >= end) {
      arr = arr.padTo(arr.length*2, null.asInstanceOf[A])
    }
    val ret = arr(1)
    var stone = 1
    var flag = false
    while (stone*2 < end && !flag) {
      var lesserChild = stone*2
      if (stone*2+1 < end && higherPriority(arr(stone*2+1), arr(lesserChild))) {
        lesserChild = stone*2+1
      }
      if (higherPriority(arr(stone), arr(lesserChild))) {
        arr(stone) = arr(lesserChild)
        stone = lesserChild
      } else {
        flag = true
      }
    }

    end -= 1
    ret
  }

  override def peek: A = arr(1)

  override def isEmpty: Boolean = end == 1
}
