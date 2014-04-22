package dk.itu.wsq.cases.raw

import dk.itu.wsq._
import dk.itu.wsq.queue._

class RawWorker(
  val id: Int,
  private val queue: WorkStealingQueue[RawNode],
  val workerPool: RawWorkerPool) extends Worker[RawNode] {

  var currentNode: Option[RawNode] = None

  def run(): Unit = {
    while(workerPool.result.isEmpty) {
      currentNode match {
        case Some(node) => node.children.nonEmpty match {
          // If the node has any children, set the first child as the current
          // node, and enqueue the rest.
          case true => {
            currentNode = Some(node.children.head)
            node.children.tail foreach queue.push
          }
          case false => currentNode = queue.take()
        }
        // No nodes, try to steal one
        case None => currentNode = workerPool.steal(id)
      }
    }
  }

  def steal(): Option[RawNode] = {
    queue.steal()
  }
}
