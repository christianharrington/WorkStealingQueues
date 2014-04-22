package dk.itu.wsq.cases.raw

import dk.itu.wsq._
import dk.itu.wsq.queue._

object RawBenchmark {
  import com.typesafe.config._

  def apply(workers: Int, seed: Long, conf: Config): Benchmark = {
    val depth = conf.getInt("benchmarks.raw.depth")
    val branching = conf.getInt("benchmarks.raw.branching")

    RawBenchmark(workers, depth, branching, seed)
  }
}

case class RawBenchmark(workers: Int, depth: Int, branching: Int, seed: Long)
  extends Benchmark {
  import scala.util.Random

  def name = s"Raw with $workers workers and a depth of $depth"

  def run(queueImpl: QueueImpl): Double = {
    val tree    = RawTreeBuilder.build(depth, branching)(new Random(seed))
    val total   = RawTreeBuilder.nodes(tree)

    val wp = new RawWorkerPool(workers, queueImpl, total, seed)

    val (t, _) = time(wp.run(tree))
    
    t
  }

  def worksWith: Seq[QueueImpl] = allQueueImpls

  override def toString(): String = "Raw" 
}
