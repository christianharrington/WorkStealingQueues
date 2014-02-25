package dk.itu.wsq

class WorkerPool[T, R](val task: Task[T, R], val workerNumber: Int = 1) {
  import java.util.Random

  private val random = new Random()

  private val workers = for (i <- 0 until workerNumber) yield {
    new Worker(i, this, task)
  }
  private val threads = for (i <- 0 until workerNumber) yield {
    new Thread(workers(i))
  }
  @volatile var result: Option[R] = None 

  def run(initial: Option[WorkUnit[T, R]]): Option[R] = {
    workers.head.setNextWorkUnit(initial)
    threads.foreach(t => t.start())
    threads.foreach(t => t.join())

    result
  }

  def steal(id: Int, next: Int = 1): Option[WorkUnit[T, R]] = {
    val r = random.nextInt(workerNumber)
    if (r == id) steal(id) else workers(r).steal()
    /*val q = (id + next) % workerNumber
    val w = if (q == id) (q + 1) % workerNumber else q

    workers(w).steal match {
      case Some(w) => Some(w)
      case None    => if (next > workerNumber) None else steal(id, next + 1)
    }*/
  }
}
