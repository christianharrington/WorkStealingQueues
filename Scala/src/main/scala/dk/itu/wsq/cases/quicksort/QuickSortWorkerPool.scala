package dk.itu.wsq.cases.quicksort

import dk.itu.wsq._
import dk.itu.wsq.queue._

class QuickSortWorkerPool(val workerNumber: Int, val queueImpl: QueueImplementation)
    extends WorkerPool[QuickSortNode, QuickSortWorker, Array[Int]] 
    with QueueHelper {
  private val _workers = (for (i <- 0 until workerNumber) yield {
    new QuickSortWorker(i, queueImplToQueue(queueImpl), this)
  }).toList

  private val _threads = for (w <- workers) yield {
    new Thread(w)
  }

  def workers = _workers
  def threads = _threads
}
