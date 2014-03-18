package dk.itu.wsq

import dk.itu.wsq.queue._

trait Benchmark {
  def time[A](f: => A) = {
    val s = System.nanoTime
    val ret = f
    val t = (System.nanoTime - s)/1e6

    (t, ret)
  }

  def run(queueImpl: QueueImplementation): Double

  def name: String

  def worksWith: Seq[QueueImplementation]
}

object BenchmarkApp extends App with QueueHelper {
  import dk.itu.wsq.cases.raw.RawBenchmark
  import dk.itu.wsq.cases.quicksort.QuickSortBenchmark
  import dk.itu.wsq.cases.spanningtree.SpanningTreeBenchmark
  import dk.itu.wsq.queue._
  import scala.util.Random

  val tries = 10
  val workers = 4

  val seed = Random.nextLong()
  val benchmarks: Seq[Benchmark] = Seq(
    QuickSortBenchmark(workers, 50000000, seed),
    RawBenchmark(workers, 9, 8, seed),
    SpanningTreeBenchmark(workers, 10000, 100, seed)
  )

  System.in.read()

  println("Starting benchmarks")

  val results: Map[String, Seq[Double]] = (for (b <- benchmarks) yield {
    for (q <- b.worksWith) yield {
      // Run the benchmark twice for fun
      println(s"\nWarming up for ${b.name} with $q...")
      b.run(q)
      b.run(q)
      println("Starting.")

      val bs = s"${b.name} with $q" -> (for (i <- 0 until tries) yield {
        //Thread.sleep(500)
        print(s"${i+1}/$tries ")
        b.run(q)
      })

      println("\nDone")
      bs
    }
  }).flatten.toMap

  println("\nReport:")

  val keys = results.keys.toList.sorted

  keys foreach { k =>
    println(s"\n$k")

    print("Times: ")
    results(k).foreach { t =>
      print("%.2f ".format(t))
    }

    val avg = results(k).fold(0.0)((a, b) => a + b) / results(k).length

    println("\nAvg: %.2f".format(avg))
  }
}
