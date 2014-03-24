package dk.itu.wsq.test

import dk.itu.wsq.cases.spanningtree._
import dk.itu.wsq.queue._
import org.scalatest._
import org.scalatest.concurrent.Timeouts
import org.scalatest.time.SpanSugar._
import scala.collection.immutable.HashSet
import scala.collection.mutable.Queue
import scala.util.Random

class SpanningTreeTests 
  extends FlatSpec 
  with Matchers 
  with QueueHelper 
  with Timeouts {

  private def spanningTree(root: SpanningTreeNode): Set[SpanningTreeNode] = {
    var result = new HashSet[SpanningTreeNode]()

    val queue = new Queue[SpanningTreeNode]()
    queue.enqueue(root)

    while (!queue.isEmpty) {
      val node = queue.dequeue
      result = result + node
      for (n <- node.children) {
        if (!result.contains(n)) queue.enqueue(n)
      }
    }

    result
  }

  private def printNeighbors(node: SpanningTreeNode): Unit = {
    val neighbors = (node.neighbors map (n => n.id)).mkString(" ")
    println(s"\tNode ${node.id} -----> $neighbors")
  }

  private def printChildren(node: SpanningTreeNode): Unit = {
    if (node.children.size > 0) {
      val children = (node.children map { n => n.id }).mkString(" ")
      println(s"\tNode ${node.id} -----> $children")
    }
    else {
      println(s"\tNode ${node.id} has no children")
    }
  }

  "Traversal of a graph" should "find a spanning tree" in runWithEveryQueueImpl {
    failAfter(5 seconds) { q: QueueImplementation =>
      val size = 10000
      val maxNumberOfNeighbors = 20
      val printGraph = false

      val graph = GraphBuilder(size, maxNumberOfNeighbors, Random.nextLong())
      assert(graph.size == size)
      // All nodes have at least one neighbor
      assert(
        graph forall (node => node.neighbors.size > 0), 
        "Some node has no neighbours")

      val workerPool = new SpanningTreeWorkerPool(2, graph.size, q)
      val root = graph.head

      if (printGraph) {
        println("GRAPH:")
        println(s"\tRoot is ${root.id}")
        for (node <- graph) {
          printNeighbors(node)
        }
      } 

      workerPool.run(root)

      if (printGraph) {
        println("\nSPANNING TREE:")
        for (node <- graph) {
          printChildren(node)
        }
      }

      // All nodes have been colored
      assert(
        graph forall (node => node.color.isDefined ), 
        "Some node has not been visited")
      // All nodes except one have a parent
      assert(
        (graph count (node => node.parent.isDefined)) == (size - 1), 
        "Some non-root node has no parent")

      val isSpanning = spanningTree(root).diff(graph).isEmpty
      assert(isSpanning, "Resulting tree is not spanning")
    }
  }
}
