package dk.itu.wsq.cases.quicksort

import dk.itu.wsq._
import dk.itu.wsq.queue._

object QuickSortBenchmark {
  import com.typesafe.config._

  def apply(workers: Int, seed: Long, conf: Config): Benchmark = {
    val length = conf.getInt("benchmarks.quicksort.length")

    QuickSortBenchmark(workers, length, seed)
  }
}

case class QuickSortBenchmark(workers: Int, length: Int, seed: Long)
  extends Benchmark with QueueHelper {

  def name = s"Quick Sort with $workers workers and array of length $length"

  private val random = new java.util.Random(seed)

  def run(queueImpl: QueueImpl): Double = {
    val in = Array.fill(length)(random.nextInt(length))
    val wp = new QuickSortWorkerPool(workers, queueImpl)

    val (t, _) = time(wp.run(QuickSortNode(in, Root[QuickSortNode]())))
    
    t
  }

  def worksWith: Seq[QueueImpl] = {
    everyQueueExcept(
      IdempotentFIFOImpl, 
      IdempotentLIFOImpl, 
      IdempotentDEImpl, 
      DuplicatingQueueImpl)
  }

  override def toString(): String = "Quick sort" 
}
