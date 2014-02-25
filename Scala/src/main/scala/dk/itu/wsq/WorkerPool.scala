package dk.itu.wsq

import scala.util.Random

class WorkerPool(val workerNumber: Int = 1) {
  import dk.itu.wsq.cases._
  import scala.collection.mutable.ArrayBuffer

  private val workers = for (i <- 0 until workerNumber) yield {
    new QuickSortWorker(i, this)
  }
  private val threads = for (i <- 0 until workerNumber) yield {
    new Thread(workers(i))
  }

  @volatile var result: Option[ArrayBuffer[Int]] = None 

  def run(initial: ArrayBuffer[Int]): Option[ArrayBuffer[Int]] = {
    workers.head.currentNode = Some(new QuickSortNode(initial, Root))
    threads.foreach(t => t.start())
    threads.foreach(t => t.join())

    result
  }

  def steal(id: Int, next: Int = 1): Option[QuickSortNode] = {
    val r = Random.nextInt(workerNumber)
    if (r == id) steal(id) else workers(r).steal()
    /*val q = (id + next) % workerNumber
    val w = if (q == id) (q + 1) % workerNumber else q

    workers(w).steal match {
      case Some(w) => Some(w)
      case None    => if (next > workerNumber) None else steal(id, next + 1)
    }*/
  }
}
