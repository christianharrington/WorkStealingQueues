package dk.itu.wsq.test

import org.scalatest._
import dk.itu.wsq.cases.spanningtree._
import scala.util.Random

class SpanningTreeTests extends FlatSpec with Matchers {
  "Traversal of a graph" should "find a spanning tree" in {
    val graph = GraphBuilder(100, Random.nextLong())

    //val workerPool = new SpanningTreeWorkerPool(2)

    //val result = 

    //graph.traverse()

  }
}