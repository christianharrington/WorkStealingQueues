package dk.itu.wsq.cases.quicksort

import dk.itu.wsq._

class QuickSortWorkerPool(val workerNumber: Int) extends WorkerPool[QuickSortNode, QuickSortWorker, Array[Int]] {
  private val _workers = (for (i <- 0 until workerNumber) yield {
    new QuickSortWorker(i, this)
  }).toList

  private val _threads = for (w <- workers) yield {
    new Thread(w)
  }

  def workers = _workers
  def threads = _threads
}
