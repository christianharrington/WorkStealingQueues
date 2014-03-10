package dk.itu.wsq.cases.quicksort

import scala.util.Random

sealed abstract class Tree()
object Root extends Tree
case class LeftTree(parent: QuickSortNode)  extends Tree
case class RightTree(parent: QuickSortNode) extends Tree

class QuickSortNode(val arr: Array[Int], val role: Tree) {
  import java.util.concurrent.atomic._

  private var _pivot = 0
  var left:  Option[Array[Int]] = None
  var right: Option[Array[Int]] = None
  var leftCounter  = 0
  var rightCounter = 0

  def pivot = _pivot

  private val hasBeenQueuedForCombining = new AtomicInteger(0)
  private val length = arr.length

  private def med3(a: Int, b: Int, c: Int) = {
    if (arr(a) < arr(b)) {
      if (arr(b) < arr(c)) b else if (arr(a) < arr(c)) c else a
    } else {
      if (arr(b) > arr(c)) b else if (arr(a) > arr(c)) c else a
    }
  }

  def divide: (QuickSortNode, QuickSortNode) = {
    // Stolen from https://github.com/scala/scala/blob/v2.10.2/src/library/scala/util/Sorting.scala#L1
    var m = (length >> 1)        // Small arrays, middle element
    if (length > 7) {
      var l = 0
      var n = length - 1
      if (length > 40) {        // Big arrays, pseudomedian of 9
        val s = length / 8
        l = med3(l, l+s, l+2*s)
        m = med3(m-s, m, m+s)
        n = med3(n-2*s, n-s, n)
      }
      m = med3(l, m, n) // Mid-size, med of 3
    }

    _pivot = arr(m)
    
    import scala.collection.mutable.ArrayBuilder

    val leftSide  = new ArrayBuilder.ofInt()
    val rightSide = new ArrayBuilder.ofInt()
    leftSide.sizeHint(arr.length / 2)
    rightSide.sizeHint(arr.length / 2)

    var v = 0
    var i = 0

    while (i < m) {
      v = arr(i)
      if (v < _pivot) {
        leftSide += v
        leftCounter += 1
      }
      else {
        rightSide += v
        rightCounter += 1
      }
      i += 1
    }

    i += 1
    
    while (i < arr.length) {
      v = arr(i)
      if (v < _pivot) {
        leftSide += v
        leftCounter += 1
      }
      else {
        rightSide += v
        rightCounter += 1
      }
      i += 1
    }

    val leftNode = new QuickSortNode(leftSide.result, LeftTree(this))
    val rightNode = new QuickSortNode(rightSide.result, RightTree(this))

    (leftNode, rightNode) 
  }

  def combine: Option[Array[Int]] = {
    (left, right) match {
      case (Some(l), Some(r)) => {
        val combinedArr = new Array[Int](arr.length)

        var i = 0

        while (i < leftCounter) {
          combinedArr(i) = l(i)
          i += 1
        }

        combinedArr(i) = _pivot
        i += 1

        var ri = 0
        while (ri < rightCounter) {
          combinedArr(i) = r(ri)
          ri += 1
          i += 1
        }

        Some(combinedArr)
      }
      case _                  => None
    }
  }

  def addToQueueForCombining(): Boolean = {
    hasBeenQueuedForCombining.incrementAndGet() == 2
  }
}
