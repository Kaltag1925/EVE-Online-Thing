package adt

import scala.collection.mutable

class BinarySearchTree[K, V](lessThan: (K, K) => Boolean) extends mutable.Map[K, V] {
  
  import BinarySearchTree._
  // Members declared in scala.collection.MapLike
  
  private var root = null.asInstanceOf[Node[K, V]]
  
  def get(key: K): Option[V] = {
    def findNode(n: Node[K, V]): Option[V] = {
      if (n == null)
        None
      else if (lessThan(key, n.key))
        findNode(n.left)
      else if (lessThan(n.key, key))
        findNode(n.right)
      else 
        Some(n.value) 
    }
    
    findNode(root)
  }
  
  def iterator: Iterator[(K, V)] = {
    new Iterator[(K, V)] {
      var stack = List[Node[K, V]]()
      private def pushAllLeft(n: Node[K, V]): Unit = {
        if (n != null) {
          stack ::= n
          pushAllLeft(n.left)
        }
      }
      pushAllLeft(root)
      def hasNext: Boolean = stack.nonEmpty

      def next(): (K, V) = {
        val (ret :: nStack) = stack
        stack = nStack
        pushAllLeft(ret.right)
        ret.key -> ret.value
      }
    }
  }
  
  // Members declared in scala.collection.mutable.MapLike
  def -=(key: K): this.type = {
    def findNode(n: Node[K, V]): Node[K, V] = {
      if (n != null) {
        if (lessThan(key, n.key)) {
          n.left = findNode(n.left) 
          n
        } else if (lessThan(n.key, key)) {
          n.right = findNode(n.right)
          n
        } else {
          if (n.left == null) {
            n.right
          } else if (n.right == null) {
            n.left
          } else {
            if (n.left.right == null) {
              n.left.right = n.right
              n.left
            } else {
              def removeRightestChild(n: Node[K, V]): Node[K, V] = {
                if (n.right.right == null) {
                  val temp = n.right
                  if (n.right.left == null) {
                    n.right = null
                  } else {
                    n.right = n.left
                  }
                  temp
                } else {
                  removeRightestChild(n.right)
                }
              }
              val rightest = removeRightestChild(n.left)
              rightest.left = n.left
              rightest.right = n.right
              rightest
            }
          }
        }
      } else {
        n
      }
    }
    
    root = findNode(root)
    
    this
  }
  
  
  def +=(kv: (K, V)): this.type = {
    val key = kv._1
    val value = kv._2
    def findNode(n: Node[K, V]): Node[K, V] = {
      if (n == null) {
        new Node(key, value, null, null)
      } else {
        if (lessThan(key, n.key)) {
          n.left = findNode(n.left) 
        } else if (lessThan(n.key, key)) {
          n.right = findNode(n.right)
        } else {
          n.value = value
        }
        n
      }
    }
    
    root = findNode(root)
    
    this
  }
}

object BinarySearchTree {
  class Node[K, V](val key: K, var value: V, var left: Node[K, V], var right: Node[K, V])
}