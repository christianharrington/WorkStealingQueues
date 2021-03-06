package dk.itu.wsq.cases.spanningtree

import dk.itu.wsq._
import dk.itu.wsq.queue._
import dk.itu.wsq.queue.stm._

class SpanningTreeWorkerPool(val workerNumber: Int, val graphSize: Int, val queueImpl: QueueImpl)
  extends WorkerPool[SpanningTreeNode, SpanningTreeWorker, SpanningTreeNode]
  with QueueHelper {

  private val _workers = (for (i <- 0 until workerNumber) yield {
    val q: WorkStealingQueue[SpanningTreeNode] = queueImpl match {
      case ABPQueueImpl            => new ABPQueue(graphSize)
      case ABPSTMQueueImpl         => new ABPSTMQueue(graphSize)
      case DuplicatingQueueImpl    => new DuplicatingQueue(graphSize)
      case DuplicatingSTMQueueImpl => new DuplicatingSTMQueue(graphSize)
      case _                       => queueImplToQueue(queueImpl)
    }
    new SpanningTreeWorker(i, this, q)
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
