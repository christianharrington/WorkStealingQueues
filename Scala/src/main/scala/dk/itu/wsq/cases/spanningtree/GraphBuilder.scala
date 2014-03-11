package dk.itu.wsq.cases.spanningtree

object GraphBuilder {
  import scala.util.Random

  private val maxNumberOfNeighbors = 25

  private def randomNeighbor(thisNode: Int, nodes: IndexedSeq[SpanningTreeNode], random: Random): SpanningTreeNode = {
    val nextNeighbour = random.nextInt(nodes.length)
    if (nextNeighbour != thisNode) {
      nodes(nextNeighbour)
    } else {
      randomNeighbor(thisNode, nodes, random)
    }
  }

  def apply(numberOfNodes: Int, seed: Long): SpanningTreeNode = {
    // Create nodes
    val nodes = (for (node <- 1 to numberOfNodes) yield {
      new SpanningTreeNode()
    }).toIndexedSeq

    val random = new Random(seed)

    // Create edges
    for (nodeIndex <- 0 until numberOfNodes) {
      val numberOfNeighbors = random.nextInt(maxNumberOfNeighbors)+1
      for (i <- 1 to numberOfNeighbors) {
        val node = nodes(nodeIndex)
        node.neighbors += randomNeighbor(nodeIndex, nodes, random)
      }
    }

    nodes.head
  }
}