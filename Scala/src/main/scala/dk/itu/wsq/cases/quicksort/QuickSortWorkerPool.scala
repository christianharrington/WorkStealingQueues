package dk.itu.wsq.cases.quicksort

import dk.itu.wsq._
import dk.itu.wsq.queue._

class QuickSortWorkerPool(val workerNumber: Int, val queueImpl: QueueImplementation) extends WorkerPool[QuickSortNode, QuickSortWorker, Array[Int]] {
  private val _workers = (for (i <- 0 until workerNumber) yield {
    val q = queueImpl match {
      case ABPQueue => new ABPQueue[QuickSortNode]()
      case ChaseLevQueue => new ChaseLevQueue[QuickSortNode]()
      case ChaseLevNaiveShrinkingQueue => new ChaseLevNaiveShrinkingQueue[QuickSortNode]()
      case LifoIWSQueue => new LifoIWSQueue[QuickSortNode]()
    }
    new QuickSortWorker(i, q, this)
  }).toList

  private val _threads = for (w <- workers) yield {
    new Thread(w)
  }

  def workers = _workers
  def threads = _threads
}
