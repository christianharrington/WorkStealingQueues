package dk.itu.wsq

import dk.itu.wsq.queue._

trait Benchmark {
  import com.typesafe.config.Config

  def time[A](f: => A) = {
    val s = System.nanoTime
    val ret = f
    val t = (System.nanoTime - s)/1e6

    (t, ret)
  }

  def run(queueImpl: QueueImpl): Double

  def name: String

  def worksWith: Seq[QueueImpl]
}

object BenchmarkApp extends App with QueueHelper {
  import com.typesafe.config._
  import dk.itu.wsq.cases.quicksort.QuickSortBenchmark
  import dk.itu.wsq.cases.raw.RawBenchmark
  import dk.itu.wsq.cases.spanningtree.SpanningTreeBenchmark
  import dk.itu.wsq.queue._
  import scala.util.Random

  private val conf = ConfigFactory.load()

  print("Loading config... ")
  val tries   = conf.getInt("benchmarks.tries")
  val workers = conf.getInt("benchmarks.workers")
  val seed    = if (conf.getBoolean("benchmarks.randomSeed")) {
    scala.util.Random.nextLong()
  } else {
    conf.getLong("benchmarks.seed")
  }
  val waitForInput = conf.getBoolean("benchmarks.wait")
  println("Done.")

  println("Loading benchmarks... ")
  val benchmarks: Seq[Benchmark] = Seq(
    QuickSortBenchmark(workers, seed, conf),
    RawBenchmark(workers, seed, conf),
    SpanningTreeBenchmark(workers, seed, conf)
  )
  println("Done.")

  if (waitForInput) {
    println("Hit enter to start")
    System.in.read()
  }

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
