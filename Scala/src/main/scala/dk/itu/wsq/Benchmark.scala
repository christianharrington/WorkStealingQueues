package dk.itu.wsq

object Benchmark extends App {
  import dk.itu.wsq._
  import dk.itu.wsq.cases.quicksort._
  import scala.util.Random
  import scala.collection.immutable.Vector

  def time[A](f: => A) = {
    val s = System.nanoTime
    val ret = f
    val t = (System.nanoTime - s)/1e6

    println(s"Time: $t ms")
    (t, ret)
  }

  val l = 1000000

  println(s"Generating Vector with $l number from 0 to $l ...")
  val testArray = Array.fill(l)(Random.nextInt(l))
  println("Done")
  
  val workers = scala.Console.readInt()
  println(s"Running with $workers workers")

  val tries = 5

  val times = for (i <- 0 until tries) yield {
    val wp = new QuickSortWorkerPool(workers)

    println("Starting...")
    val (t0, r0) = time(wp.run(QuickSortNode(testArray, Root[QuickSortNode]())))
    println("Done")

    val a = testArray.clone()

    println("Starting...")
    val (t1, r1) = time(scala.util.Sorting.quickSort(a))
    println("Done\n\n")

    (t0, t1)
  }

  val (times0, times1) = times.unzip

  val avg0 = times0.sum / tries
  val avg1 = times1.sum / tries
  println(s"Avgs: $avg0, $avg1")
}
