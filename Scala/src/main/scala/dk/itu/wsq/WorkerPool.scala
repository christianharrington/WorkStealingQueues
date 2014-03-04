package dk.itu.wsq

import scala.util.Random

class WorkerPool(val workerNumber: Int = 1) {
  import dk.itu.wsq.cases.quicksort._

  private val workers = for (i <- 0 until workerNumber) yield {
    new QuickSortWorker(i, this)
  }
  private val threads = for (i <- 0 until workerNumber) yield {
    new Thread(workers(i))
  }

  @volatile var result: Option[Array[Int]] = None 

  def run(initial: Array[Int]): Option[Array[Int]] = {
    workers.head.currentNode = Some(new QuickSortNode(initial, Root))
    threads.foreach(t => t.start())
    threads.foreach(t => t.join())

    result
  }

  def steal(id: Int, next: Int = 1): Option[QuickSortNode] = {
    workers(Random.nextInt(workerNumber)).steal()
    /* match {
      case Some(a) => {
        println(s"$id stealing from $r, got some")
        Some(a) 
      }
      case None => {
        println(s"$id stealing from $r, got none")
        None
      }
    }*/
    /*val q = (id + next) % workerNumber
    val w = if (q == id) (q + 1) % workerNumber else q

    workers(w).steal match {
      case Some(w) => Some(w)
      case None    => if (next > workerNumber) None else steal(id, next + 1)
    }*/
  }
}
