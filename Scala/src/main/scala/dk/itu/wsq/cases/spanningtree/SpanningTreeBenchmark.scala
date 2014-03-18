package dk.itu.wsq.cases.spanningtree

import dk.itu.wsq._
import dk.itu.wsq.queue._

case class SpanningTreeBenchmark(workers: Int, nodes: Int, branchingFactor: Int, queueImpl: QueueImplementation, seed: Long) extends Benchmark {
  def name = s"Spanning tree with $workers workers and graph with $nodes nodes and branching factor $branchingFactor, using $queueImpl"

  def run(): Double = {
    val in = GraphBuilder(nodes, branchingFactor, seed)
    val wp = new SpanningTreeWorkerPool(workers, in.size, queueImpl)

    val (t, _) = time(wp.run(in.head))
    
    t
  }
}
