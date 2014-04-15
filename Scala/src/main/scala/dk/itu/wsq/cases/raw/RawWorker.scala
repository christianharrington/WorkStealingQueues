package dk.itu.wsq.cases.raw

import dk.itu.wsq._
import dk.itu.wsq.queue._

class RawWorker(
  val id: Int,
  private val queue: WorkStealingQueue[RawNode],
  val workerPool: RawWorkerPool) extends Worker[RawNode] {

  var currentNode: Option[RawNode] = None

  @volatile var counter = 0
  @volatile var stealCounter = 0

  def run(): Unit = {
    while(workerPool.result.isEmpty) {
      if (workerPool.total >= workerPool.goal) {
        workerPool.result = Some(true)
      }
      else {
        currentNode match {
          case Some(node) => {
            stealCounter = 0
            if (node.visited.compareAndSet(false, true)) {
              counter += 1
              var first = true
              node.children foreach { c =>
                if (first) {
                  currentNode = Some(c)
                  first = false
                } else {
                  queue.push(c)
                }
              }
            }
            else {
              currentNode = queue.take()
            }
          }
          case None => {
            currentNode = workerPool.steal(id)
          }
        }
      }
    }
  }

  def steal(): Option[RawNode] = {
    stealCounter += 1
    if (stealCounter >= 1000) {
      println(s"Worker $id tried to steal $stealCounter times. Local counter: $counter. Global counter: ${workerPool.total}. Goal: ${workerPool.goal}")
    }
    queue.steal()
  }
}
