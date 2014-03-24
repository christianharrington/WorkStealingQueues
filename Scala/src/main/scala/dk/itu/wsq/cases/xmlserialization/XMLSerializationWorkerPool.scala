package dk.itu.wsq.cases.xmlserialization

import dk.itu.wsq._
import dk.itu.wsq.queue._

class XMLSerializationWorkerPool(val workerNumber: Int, val queueImpl: QueueImpl) 
  extends WorkerPool[XMLNode, XMLSerializationWorker, String]
  with QueueHelper {

  private val _workers = (for (i <- 0 until workerNumber) yield {
    new XMLSerializationWorker(i, this, queueImplToQueue(queueImpl))
  }).toList

  private val _threads = for (w <- workers) yield {
    new Thread(w)
  }

  def workers = _workers
  def threads = _threads

}