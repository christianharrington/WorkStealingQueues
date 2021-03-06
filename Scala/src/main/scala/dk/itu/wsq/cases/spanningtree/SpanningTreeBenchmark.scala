package dk.itu.wsq.cases.spanningtree

import dk.itu.wsq._
import dk.itu.wsq.queue._

object SpanningTreeBenchmark {
  import com.typesafe.config._

  def apply(workers: Int, seed: Long, conf: Config): Benchmark = {
    val nodes = conf.getInt("benchmarks.spanning.nodes")
    val branching = conf.getInt("benchmarks.spanning.branching")

    SpanningTreeBenchmark(workers, nodes, branching, seed)
  }
}

case class SpanningTreeBenchmark(workers: Int, nodes: Int, branching: Int, seed: Long)
  extends Benchmark {

  private val in = GraphBuilder(nodes, branching, seed)

  private def resetGraph(nodes: Set[SpanningTreeNode]) = {
    nodes.foreach(n => n.reset())
    nodes
  }

  def name = s"Spanning tree with $workers workers and graph with $nodes nodes and branching factor $branching"

  def run(queueImpl: QueueImpl): Double = {
    resetGraph(in)

    val wp = new SpanningTreeWorkerPool(workers, in.size, queueImpl)

    val (t, _) = time(wp.run(in.head))
    
    t
  }

  def worksWith: Seq[QueueImpl] = allQueueImpls

  override def toString(): String = "Spanning Tree" 
}
