package dk.itu.wsq.cases.raw

import dk.itu.wsq._
import dk.itu.wsq.queue._

class RawWorkerPool(val workerNumber: Int, val queueImpl: QueueImpl, val goal: Int, val seed: Long)
  extends WorkerPool[RawNode, RawWorker, Boolean] 
  with QueueHelper {

  import scala.util.Random
  
  private val _workers = (for (i <- 0 until workerNumber) yield {
    new RawWorker(i, queueImplToQueue(queueImpl), this)
  }).toList

  private val _threads = for (w <- workers) yield {
    new Thread(w)
  }

  private val unsuccesfulSteal = new Array[Boolean](workerNumber)

  def workers = _workers
  def threads = _threads

  override def steal(id: Int): Option[RawNode] = {
    workers(Random.nextInt(workers.length)).steal() match {
      case Some(n) => {
        unsuccesfulSteal(id) = false
        Some(n)
      }
      case None => {
        unsuccesfulSteal(id) = true

        if (unsuccesfulSteal.forall(b => b)) {
          result = Some(true)
        }

        None
      }
    }
  }
}
