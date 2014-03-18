package dk.itu.wsq.cases.spanningtree

object GraphBuilder {
  import scala.util.Random
  import scala.collection.mutable

  private val graphCache = mutable.Map[Long, Set[SpanningTreeNode]]()

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

    val n = nodes.toSet
    graphCache += seed -> n
    n
  }

  private def resetGraph(nodes: Set[SpanningTreeNode]) = {
    nodes.par.foreach(n => n.reset())
    nodes
  }

  def apply(numberOfNodes: Int, maxNumberOfNeighbors: Int, seed: Long): Set[SpanningTreeNode] = {
    if (graphCache.contains(seed)) {
      resetGraph(graphCache(seed))
    }
    else {
      buildGraph(numberOfNodes, maxNumberOfNeighbors, seed)
    }
  }
}
