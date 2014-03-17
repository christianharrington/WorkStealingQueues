package dk.itu.wsq.cases.raw

import dk.itu.wsq._
import dk.itu.wsq.queue._

case class RawBenchmark(workers: Int, depth: Int, queueImpl: QueueImplementation, seed: Long) extends Benchmark {
  def name = s"Raw with $workers workers and a depth of $depth, using $queueImpl"

  def run(): Double = {
    val builder = RawTreeBuilder(seed)
    val tree    = RawNode(0)
    builder.build(tree, depth)
    val total   = builder.nodes(tree)

    val wp = new RawWorkerPool(workers, queueImpl, total, seed)

    val (t, _) = time(wp.run(tree))
    
    t
  }
}
