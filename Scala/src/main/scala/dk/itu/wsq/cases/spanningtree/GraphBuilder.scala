package dk.itu.wsq.cases.spanningtree

object GraphBuilder {
  import scala.util.Random

  private def randomNeighbor(thisNode: Int, nodes: IndexedSeq[SpanningTreeNode], random: Random): SpanningTreeNode = {
    val nextNeighbour = random.nextInt(nodes.length)
    if (nextNeighbour != thisNode) {
      nodes(nextNeighbour)
    } else {
      randomNeighbor(thisNode, nodes, random)
    }
  }

  private def buildGraph(numberOfNodes: Int, maxNumberOfNeighbors: Int, seed: Long): Set[SpanningTreeNode] = {
    // Create nodes
    val nodes = (for (i <- 1 to numberOfNodes) yield new SpanningTreeNode(i))

    val random = new Random(seed)

    // Create edges
    for (nodeIndex <- 0 until numberOfNodes) {
      val numberOfNeighbors = random.nextInt(maxNumberOfNeighbors)+1
      val node = nodes(nodeIndex)
      for (i <- 1 to numberOfNeighbors) {
        val newNeighbor = randomNeighbor(nodeIndex, nodes, random)
        node.neighbors += newNeighbor
        newNeighbor.neighbors += node
      }
    }

    nodes.toSet
  }

  private def resetGraph(nodes: Set[SpanningTreeNode]) = {
    nodes.foreach(n => n.reset())
    nodes
  }

  def apply(numberOfNodes: Int, maxNumberOfNeighbors: Int, seed: Long): Set[SpanningTreeNode] = {
    print("SpanningTree: Building graph... ")
    val g = buildGraph(numberOfNodes, maxNumberOfNeighbors, seed)
    println("Done.")
    g
  }
}
