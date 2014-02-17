package dk.itu.wsq

class WorkerPool[T, R](val task: Task[T, R], val workerNumber: Int = 1) {
  import java.util.Random

  private val random = new Random()

  private val workers = new Array[Worker[T, R]](workerNumber)
  private val threads = for (i <- 0 until workerNumber) yield {
    new Thread(workers(i))
  }
  @volatile var result: Option[R] = None 

  def run(initial: Option[T]): Option[R] = {
    workers(0).work = initial
    threads.foreach(t => t.start())
    threads.foreach(t => t.join())

    result
  }

  def steal(): Option[T] = {
    val r = random.nextInt(workerNumber)
    workers(r).steal()
  }
}
