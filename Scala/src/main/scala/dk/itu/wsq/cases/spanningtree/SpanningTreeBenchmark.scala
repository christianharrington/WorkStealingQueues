package dk.itu.wsq.cases.spanningtree

import dk.itu.wsq._
import dk.itu.wsq.queue._

case class SpanningTreeBenchmark(workers: Int, nodes: Int, branching: Int, seed: Long)
  extends Benchmark {

  def name = s"Spanning tree with $workers workers and graph with $nodes nodes and branching factor $branching"

  def run(queueImpl: QueueImpl): Double = {
    val in = GraphBuilder(nodes, branching, seed)
    val wp = new SpanningTreeWorkerPool(workers, in.size, queueImpl)

    val (t, _) = time(wp.run(in.head))
    
    t
  }

  def worksWith: Seq[QueueImpl] = AllQueueImpls()
}
