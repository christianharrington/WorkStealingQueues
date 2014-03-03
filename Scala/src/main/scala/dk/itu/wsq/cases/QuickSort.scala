package dk.itu.wsq.cases

import dk.itu.wsq.{WorkerPool, WorkStealingQueue}
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

sealed abstract class Tree()
object Root extends Tree
case class LeftTree(parent: QuickSortNode)  extends Tree
case class RightTree(parent: QuickSortNode) extends Tree

class QuickSortNode(val list: ArrayBuffer[Int], val role: Tree) {
  import java.util.concurrent.atomic._

  var pivot = 0
  var left:  Option[ArrayBuffer[Int]] = None
  var right: Option[ArrayBuffer[Int]] = None

  private val hasBeenQueuedForCombining = new AtomicInteger(0)

  def divide: (QuickSortNode, QuickSortNode) = {
    val pivotPoint = Random.nextInt(list.length)
    pivot = list(pivotPoint)
    
    val leftSide  = new ArrayBuffer[Int](list.length / 2)
    val rightSide = new ArrayBuffer[Int](list.length / 2)
    
    var v = 0

    for (i <- 0 until pivotPoint) {
      v = list(i)
      (if (v < pivot) leftSide else rightSide) += v
    }
    
    for (i <- pivotPoint + 1 until list.length) {
      v = list(i)
      (if (v < pivot) leftSide else rightSide) += v
    }

    val leftNode = new QuickSortNode(leftSide, LeftTree(this))
    val rightNode = new QuickSortNode(rightSide, RightTree(this))

    (leftNode, rightNode) 
  }

  def combine: Option[ArrayBuffer[Int]] = {
    (left, right) match {
      case (Some(l), Some(r)) => Some ((l += pivot) ++= r)
      case _                  => None
    }
  }

  def addToQueueForCombining(): Boolean = {
    val v = hasBeenQueuedForCombining.get

    if(hasBeenQueuedForCombining.compareAndSet(v, v + 1)) {
      v + 1 == 2
    } else {
      true
    }
  }
}

object QuickSortWorker {
  def insertionSort(arr: ArrayBuffer[Int]): ArrayBuffer[Int] = {
    //println("Insertion sort")
    var x = 0
    var j = 0

    for (i <- 0 until arr.length) {
      x = arr(i)
      j = i
      while (j > 0 && arr(j - 1) > x) {
        arr(j) = arr(j -1)
        j = j - 1
      }
      arr(j) = x
    }

    arr
  }
}

class QuickSortWorker(val id: Int, val workerPool: WorkerPool) extends Runnable {
  import QuickSortWorker._
  import java.lang.Thread

  private val queue = new WorkStealingQueue[QuickSortNode]()
  var currentNode: Option[QuickSortNode] = None
  private val threshold = 7

  def run(): Unit = {
    while(workerPool.result.isEmpty) {
      currentNode match {
        case Some(node) => {
          execute(node) match {
            case Right(list) => workerPool.result = Some(list)
            case Left(q) => q match {
              case Some(q) => currentNode = Some(q)
              case None    => currentNode = queue.take()
            }
          }
        }
        case None => {
          currentNode = workerPool.steal(id)
        }
      }
    }
  }

  def execute(node: QuickSortNode): Either[Option[QuickSortNode], ArrayBuffer[Int]] = {
    val combine = node.combine

    // We couldn't combine our nodes, and the remaining list is too long
    // to insertion sort, so we split the work up
    if (combine.isEmpty && node.list.length > threshold) {
      //println("Dividing...")
      val (leftNode, rightNode) = node.divide
      queue.push(rightNode)

      Left(Some(leftNode))
    }
    // Either we can combine two nodes to get our result, or we can insertion
    // sort the remaining list
    else {
      val list = combine getOrElse insertionSort(node.list)
      node.role match {
        case Root => {
          //println("Done!")
          Right(list)
        }
        case LeftTree(parent) => {
          parent.left = Some(list)

          if (parent.right.isDefined) {
            //println("Left: Pushing parent")
            queue.push(parent)
          }

          Left(None)
        }
        case RightTree(parent) => {
          parent.right = Some(list)

          if (parent.left.isDefined) {
            //println("Right: Pushing parent")
            queue.push(parent)
          }

          Left(None)
        }
      }
    }
  }

  def steal(): Option[QuickSortNode] = queue.steal()
}
