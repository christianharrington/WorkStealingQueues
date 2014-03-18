package dk.itu.wsq.cases.spanningtree

import dk.itu.wsq._
import dk.itu.wsq.queue._

class SpanningTreeWorker(val id: Int, val workerPool: SpanningTreeWorkerPool, private val queue: WorkStealingQueue[SpanningTreeNode]) extends Worker[SpanningTreeNode] {
  
  var currentNode: Option[SpanningTreeNode] = None

  val color = id

  @volatile var visitCounter = 0

  def run(): Unit = {
    while (!workerPool.isFinished) {
      currentNode match {
        case Some(node) => {
          execute(node)
          currentNode = queue.take()
        }
        case None       => {
          currentNode = workerPool.steal(id)
        }
      }
    }
  }

  def execute(node: SpanningTreeNode): Unit = {
    node.traverse(this)
  }

  def addToQueue(node: SpanningTreeNode): Unit = queue.push(node)

  def steal(): Option[SpanningTreeNode] = queue.steal()
}