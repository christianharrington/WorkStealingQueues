package dk.itu.wsq

trait Benchmark {
  def time[A](f: => A) = {
    val s = System.nanoTime
    val ret = f
    val t = (System.nanoTime - s)/1e6

    (t, ret)
  }

  def run(): Double

  def name: String
}

object BenchmarkApp extends App {
  import dk.itu.wsq.cases.quicksort.QuickSortBenchmark
  import dk.itu.wsq.queue._
  import scala.util.Random

  val tries = 10
  val workers = 4

  val queueImpls: List[QueueImplementation] = List(
    ABPQueue,
    ChaseLevQueue,
    ChaseLevNaiveShrinkingQueue
  )

  val seed = Random.nextLong()

  val benchmarks: List[QueueImplementation => Benchmark] = List(
    q => QuickSortBenchmark(workers, 100000, q, seed)
  )

  println("Starting benchmarks")

  val results: Map[String, Seq[Double]] = (for (b <- benchmarks) yield {
    for (q <- queueImpls) yield {
      val bq = b(q)
      // Run the benchmark twice for fun
      println(s"\nWarming up for ${bq.name}...")
      bq.run()
      bq.run()
      println("Starting.")

      val bs = bq.name -> (for (i <- 0 until tries) yield {
        Thread.sleep(500)
        print(s"${i+1}/$tries ")
        bq.run()
      })

      println("\nDone")
      bs
    }
  }).flatten.toMap

  println("\nReport:")

  results foreach { case (b, ts) =>
    println(s"\n$b")

    print("Times: ")
    ts.foreach { t =>
      print("%.2f ".format(t))
    }

    val avg = ts.fold(0.0)((a, b) => a + b) / ts.length

    println("\nAvg: %.2f".format(avg))
  }
}
