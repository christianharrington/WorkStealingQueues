package dk.itu.wsq.cases.spanningtree

import dk.itu.wsq._
import dk.itu.wsq.queue._

class SpanningTreeWorkerPool(val workerNumber: Int, val queueImpl: QueueImplementation) extends WorkerPool[SpanningTreeNode, SpanningTreeWorker, SpanningTreeNode] {

  private val _workers = (for (i <- 0 until workerNumber) yield {
    val q = queueImpl match {
      case ABPQueue => new ABPQueue[SpanningTreeNode]()
      case ChaseLevQueue => new ChaseLevQueue[SpanningTreeNode]()
      case ChaseLevNaiveShrinkingQueue => new ChaseLevNaiveShrinkingQueue[SpanningTreeNode]()
      case LifoIWSQueue => new LifoIWSQueue[SpanningTreeNode]()
    }
    new SpanningTreeWorker(i, this, q)
  }).toList

  private val _threads = for (w <- workers) yield {
    new Thread(w)
  }

  def workers = _workers
  def threads = _threads
}