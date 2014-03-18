package dk.itu.wsq.cases.spanningtree

import dk.itu.wsq._
import dk.itu.wsq.queue._

class SpanningTreeWorkerPool(val workerNumber: Int, val graphSize: Int, val queueImpl: QueueImplementation)
    extends WorkerPool[SpanningTreeNode, SpanningTreeWorker, SpanningTreeNode]
    with QueueHelper {

  private val _workers = (for (i <- 0 until workerNumber) yield {
    new SpanningTreeWorker(i, this, queueImplToQueue(queueImpl))
  }).toList

  private val _threads = for (w <- workers) yield {
    new Thread(w)
  }

  def isFinished: Boolean = {
    val nodesVisited = workers.foldRight(0)( (worker, acc) => worker.visitCounter + acc )

    nodesVisited == graphSize
  }

  def workers = _workers
  def threads = _threads
}