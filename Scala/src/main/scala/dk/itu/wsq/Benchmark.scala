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
  import dk.itu.wsq.cases.xmlserialization.XMLSerializationBenchmark
  import dk.itu.wsq.queue._
  import java.io._
  import scala.util.Random

  private val conf = args.length >= 1 match {
    case true => {
      val path = args(0)
      println(s"Using config file at $path")
      val globalConfFile = new File(path)
      System.setProperty("config.file", globalConfFile.getPath())
      ConfigFactory.load()
    }
    case false => ConfigFactory.load()
  }

  type Times   = Map[QueueImpl, Seq[Double]]
  type Results = Map[Benchmark, Times]

  print("Loading config... ")
  val tries   = conf.getInt("benchmarks.tries")
  val workers = conf.getInt("benchmarks.workers")
  val seed    = if (conf.getBoolean("benchmarks.randomSeed")) {
    scala.util.Random.nextLong()
  } else {
    conf.getLong("benchmarks.seed")
  }
  println(s"Using seed $seed")
  val waitForInput = conf.getBoolean("benchmarks.wait")

  val log          = conf.getBoolean("benchmarks.log")
  val logDir       = conf.getString("benchmarks.logDir")
  println("Done.")

  println("Loading benchmarks... ")
  val benchmarks: Seq[Benchmark] = Seq(
    QuickSortBenchmark(workers, seed, conf),
    SpanningTreeBenchmark(workers, seed, conf),
    XMLSerializationBenchmark(workers, seed, conf),
    RawBenchmark(workers, seed, conf)
  )
  println("Done.")

  if (waitForInput) {
    println("Hit enter to start")
    System.in.read()
  }

  println("Starting benchmarks")

  val results = runBenchmarks

  println("\nReport:")
  println(report(results))

  if (log) {
    val f = new File(logDir)
    f.mkdirs()
    logAverages()
    logTimes()
  }

  def logAverages(): Unit = {
    val path = s"$logDir/log.$seed.w$workers.averages.csv"
    println(s"Writing avereages to $path")
    val writer = new BufferedWriter(new FileWriter(path))
    writer.write(averagesAsCSV(results))
    writer.close()
  }

  def logTimes(): Unit = {
    for (b <- benchmarks) {
      val path = s"$logDir/log.$seed.w$workers.$b.csv"
      println(s"Writing times for $b to $path")
      val writer = new BufferedWriter(new FileWriter(path))
      writer.write(timesAsCSV(b, results(b)) + "\n")
      writer.close()
    }
  }

  def runBenchmarks: Results = {
    (for (benchmark <- benchmarks) yield {
      val queues = for (queue <- benchmark.worksWith) yield {
        // Run the benchmark twice for fun
        println(s"\nWarming up for ${benchmark.name} with $queue...")
        benchmark.run(queue)
        benchmark.run(queue)
        println("Starting.")

        val bs = queue -> (for (i <- 0 until tries) yield {
          print(s"${i+1}/$tries ")
          benchmark.run(queue)
        })

        println("\nDone")
        bs
      }

      benchmark -> queues.toMap
    }).toMap
  }

  def report(results: Results): String = {
    val str = new StringBuilder()

    results.keys foreach { benchmark => 
      results(benchmark) foreach { case (queue, times) => 
        str ++= s"\n${benchmark.name} with $queue\n"

        str ++= "Times: "
        times.foreach { t =>
          str ++= "%.2f ".format(t)
        }

        val avg = times.fold(0.0)((a, b) => a + b) / times.length

        str ++= "\nAvg: %.2f\n".format(avg)
      }
    }

    str.toString()
  }

  def timesAsCSV(benchmark: Benchmark, times: Times): String = {
    val str = new StringBuilder()

    str ++= "Queue;"
    str ++= (1 to times.values.head.size).mkString(";")
    str ++= "\n"

    for (queue <- benchmark.worksWith) {
      val ts = for (t <- times(queue)) yield {
        "%.2f".format(t)
      }
      str ++= s"$queue;"
      str ++= ts.mkString(";")
      str ++= "\n"
    }

    str.toString()
  }

  def averagesAsCSV(results: Results): String = {
    val str = new StringBuilder()

    str ++= "Benchmark;" 
    str ++= allQueueImpls.mkString(";")
    str ++= "\n"

    for ((benchmark, qs) <- results) {
      str ++= benchmark.toString()
      for (queue <- allQueueImpls) {
        if (benchmark.worksWith.contains(queue)) {
          val times = qs(queue)
          val avg = times.fold(0.0)((a, b) => a + b) / times.length
          str ++= s";%.2f".format(avg)
        }
        else {
          str ++= ";"
        }
      }
      str ++= "\n"
    }

    str.toString()
  }
}
