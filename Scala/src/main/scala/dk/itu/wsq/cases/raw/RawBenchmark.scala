package dk.itu.wsq.cases.raw

import dk.itu.wsq._
import dk.itu.wsq.queue._

case class RawBenchmark(workers: Int, depth: Int, branching: Int, seed: Long)
  extends Benchmark {

  def name = s"Raw with $workers workers and a depth of $depth"

  def run(queueImpl: QueueImpl): Double = {
    val builder = RawTreeBuilder(seed)
    val tree    = RawNode(0)
    builder.build(tree, depth, branching)
    val total   = builder.nodes(tree)

    val wp = new RawWorkerPool(workers, queueImpl, total, seed)

    val (t, _) = time(wp.run(tree))
    
    t
  }

  def worksWith: Seq[QueueImpl] = allQueueImpls
}
