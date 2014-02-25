package dk.itu.wsq.cases

import dk.itu.wsq.{WorkerPool, WorkStealingQueue}
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

sealed abstract class Tree()
object Root extends Tree
case class LeftTree(parent: QuickSortNode)  extends Tree
case class RightTree(parent: QuickSortNode) extends Tree

class QuickSortNode(val list: ArrayBuffer[Int], val role: Tree) {
  var pivot: Int = _
  var left: Option[ArrayBuffer[Int]] = None
  var right: Option[ArrayBuffer[Int]] = None
  
  def divide: (QuickSortNode, QuickSortNode) = {
    val pivotPoint = Random.nextInt(list.length)
    pivot = list(pivotPoint)
    
    val leftSide  = new ArrayBuffer[Int]()
    val rightSide = new ArrayBuffer[Int]()
    
    for (i <- 0 until pivotPoint) {
      (if (list(i) < pivot) leftSide else rightSide) += list(i)
    }
    
    for (i <- pivotPoint + 1 until list.length) {
      (if (list(i) < pivot) leftSide else rightSide) += list(i)
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
}

class QuickSortWorker(val id: Int, val workerPool: WorkerPool) extends Runnable {
  private val queue = new WorkStealingQueue[QuickSortNode]()
  var currentNode: Option[QuickSortNode] = None
  private val threshold = 100

  def run(): Unit = {
    while(workerPool.result.isEmpty) {
      currentNode match {
        case Some(node) => {
          execute(node) match {
            case Some(list) => workerPool.result = Some(list)
            case None => {}
          }
        }
        case None => currentNode = workerPool.steal(id)
      }
    }
  }

  def execute(node: QuickSortNode): Option[ArrayBuffer[Int]] = {
    node.combine match {
      case Some(list) => {
        node.role match {
          case Root => Some (list)
          case LeftTree(p) => {
            p.left = Some(list)
            currentNode = queue.take()
            None
          }
          case RightTree(p) => {
            p.right = Some(list)
            currentNode = queue.take()
            None      
          }
        }
      }
      case None => {
        if (node.list.length <= threshold) { // insertion sort
          val list = insertionSort(node.list)
          node.role match {
            case Root => Some(list)
            case LeftTree(p) => {
              p.left = Some(list)
              currentNode = queue.take()
              None
            }
            case RightTree(p) => {
              p.right = Some(list)
              currentNode = queue.take()
              None
            }
          }
        } else {
          val (leftNode, rightNode) = node.divide
          currentNode = Some(leftNode)
          queue.push(rightNode)

          None
        }
      }
    }
  }

  def steal(): Option[QuickSortNode] = queue.steal()

  def insertionSort(arr: ArrayBuffer[Int]): ArrayBuffer[Int] = {
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
