package dk.itu.wsq.cases.quicksort

import dk.itu.wsq._
import dk.itu.wsq.queue._

case class QuickSortBenchmark(workers: Int, length: Int, queueImpl: QueueImplementation, seed: Long) extends Benchmark {
  def name = s"Quick Sort with $workers workers and array of length $length, using $queueImpl"

  private val random = new java.util.Random(seed)

  def run(): Double = {
    val in = Array.fill(length)(random.nextInt(length))
    val wp = new QuickSortWorkerPool(workers, queueImpl)

    val (t, _) = time(wp.run(QuickSortNode(in, Root[QuickSortNode]())))
    
    t
  }
}
