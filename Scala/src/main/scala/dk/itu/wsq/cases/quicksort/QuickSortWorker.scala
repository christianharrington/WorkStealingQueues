package dk.itu.wsq.cases.quicksort

import dk.itu.wsq.queue._
import dk.itu.wsq.{ WorkerPool }

object QuickSortWorker {
  def insertionSort(arr: Array[Int]): Unit = {
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
  }
}

class QuickSortWorker(val id: Int, val workerPool: WorkerPool) extends Runnable {
  import QuickSortWorker._
  import java.lang.Thread

  private val queue: WorkStealingQueue[QuickSortNode] = new ABPQueue[QuickSortNode]()
  var currentNode: Option[QuickSortNode] = None
  private val threshold = 100

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

  def execute(node: QuickSortNode): Either[Option[QuickSortNode], Array[Int]] = {
    val combine = node.combine

    // We couldn't combine our nodes, and the remaining list is too long
    // to insertion sort, so we split the work up
    if (combine.isEmpty && node.arr.length > threshold) {
      //println("Dividing...")
      val (leftNode, rightNode) = node.divide
      queue.push(rightNode)

      Left(Some(leftNode))
    }
    // Either we can combine two nodes to get our result, or we can insertion
    // sort the remaining list
    else {
      val arr = combine getOrElse {insertionSort(node.arr); node.arr}
      node.role match {
        case Root => {
          //println("Done!")
          Right(arr)
        }
        case LeftTree(parent) => {
          parent.left = Some(arr)

          if (parent.addToQueueForCombining()) {
            //println("Left: Pushing parent")
            queue.push(parent)
          }

          Left(None)
        }
        case RightTree(parent) => {
          parent.right = Some(arr)

          if (parent.addToQueueForCombining()) {
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
