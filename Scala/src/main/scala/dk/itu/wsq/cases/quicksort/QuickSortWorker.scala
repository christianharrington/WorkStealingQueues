package dk.itu.wsq.cases.quicksort

import dk.itu.wsq.queue._
import dk.itu.wsq.{ WorkerPool }

class QuickSortWorker(val id: Int, val workerPool: WorkerPool, val threshold: Int = 100) extends Runnable {
  import java.lang.Thread

  private val queue: WorkStealingQueue[QuickSortNode] = new ChaseLevNaiveShrinkingQueue[QuickSortNode]()
  var currentNode: Option[QuickSortNode] = None

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
      val arr = combine getOrElse {node.insertionSort(); node.arr}
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
