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

  def divide: (QuickSortNode, QuickSortNode) = {
    val pivotPoint = Random.nextInt(arr.length)
    _pivot = arr(pivotPoint)
    
    import scala.collection.mutable.ArrayBuilder

    val leftSide  = new ArrayBuilder.ofInt()
    val rightSide = new ArrayBuilder.ofInt()
    leftSide.sizeHint(arr.length / 2)
    rightSide.sizeHint(arr.length / 2)

    var v = 0
    var i = 0

    while (i < pivotPoint) {
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
