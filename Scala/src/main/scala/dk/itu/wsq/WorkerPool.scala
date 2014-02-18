package dk.itu.wsq

class WorkerPool[T, R](val task: Task[T, R], val workerNumber: Int = 1) {
  import java.util.Random

  private val random = new Random()

  private val workers = for (i <- 0 until workerNumber) yield {
    new Worker(this, task)
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

  def steal(): Option[WorkUnit[T, R]] = {
    val r = random.nextInt(workerNumber)
    workers(r).steal()
  }
}
