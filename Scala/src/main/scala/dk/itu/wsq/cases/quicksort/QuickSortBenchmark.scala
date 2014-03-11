package dk.itu.wsq.cases.quicksort

import dk.itu.wsq._
import scala.util.Random

case class QuickSortBenchmark(workers: Int, length: Int) extends Benchmark {
  def name = s"Quick Sort with $workers workers and array of length $length"

  def run(): Double = {
    val testArray = Array.fill(length)(Random.nextInt(length))
    val wp = new QuickSortWorkerPool(workers)

    val (t, _) = time(wp.run(QuickSortNode(testArray, Root[QuickSortNode]())))
    
    t
  }
}
