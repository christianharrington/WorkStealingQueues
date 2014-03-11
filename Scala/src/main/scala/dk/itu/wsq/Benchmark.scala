package dk.itu.wsq

trait Benchmark {
  type Input

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

  val tries = 10
  val workers = 4

  val benchmarks: List[Benchmark] = List(
    QuickSortBenchmark(workers, 100000)
  )

  val results: Map[String, Seq[Double]] = (for (b <- benchmarks) yield {
    b.name -> (for (i <- 0 until tries) yield {
      Thread.sleep(500)
      b.run()
    })
  }).toMap

  results foreach { case (b, ts) =>
    println(b)

    ts.zipWithIndex.foreach { case (t, i) =>
      println(s"Try $i: $t")
    }

    val avg = ts.fold(0.0)((a, b) => a + b) / ts.length

    println(s"Avg: $avg")
  }
}
