package dk.itu.wsq.cases.spanningtree

import dk.itu.wsq._
import dk.itu.wsq.queue._

class SpanningTreeWorker(val id: Int, val workerPool: SpanningTreeWorkerPool, private val queue: WorkStealingQueue[SpanningTreeNode]) extends Worker[SpanningTreeNode] {
  
  var currentNode: Option[SpanningTreeNode] = None

  val color = id

  def run(): Unit = {
    while (workerPool.result.isEmpty) {
      currentNode match {
        case Some(node) => {
          execute(node) match {
            case Some(root) => workerPool.result = Some(root)
            case None       => currentNode = queue.take()
          }
        }
        case None       => {
          currentNode = workerPool.steal(id)
        }
      }
    }
  }

  def execute(node: SpanningTreeNode): Option[SpanningTreeNode] = {
    node.paint(color)
    node.traverse(this)

    if (node.hasVisitedAllNeighbors) {
      node.parent match {
        case Some(p) => { // Enqueue parent
          queue.push(p)
          None
        }
        case None => Some(node) // Node is root
      }
    } else {
      None // Get more work
    }
  }

  def addToQueue(node: SpanningTreeNode): Unit = queue.push(node)

  def steal(): Option[SpanningTreeNode] = queue.steal()
}