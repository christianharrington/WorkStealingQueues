package dk.itu.wsq.cases.raw

import dk.itu.wsq._
import dk.itu.wsq.queue._

class RawWorkerPool(val workerNumber: Int, val queueImpl: QueueImpl, val goal: Int, val seed: Long)
  extends WorkerPool[RawNode, RawWorker, Boolean] 
  with QueueHelper {
  
  private val _workers = (for (i <- 0 until workerNumber) yield {
    new RawWorker(i, queueImplToQueue(queueImpl), this)
  }).toList

  private val _threads = for (w <- workers) yield {
    new Thread(w)
  }

  def workers = _workers
  def threads = _threads

  def total = {
    workers.foldLeft(0)((t, w) => t + w.counter)
  }
}
