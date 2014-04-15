package dk.itu.wsq.cases.raw

import dk.itu.wsq._

case class RawTreeBuilder(seed: Long) {
  import scala.annotation.tailrec

  val random = new java.util.Random(seed)

  def build(node: RawNode, maxDepth: Int, branching: Int): Unit = {
    val fanout = random.nextInt(branching) + 1

    if (node.depth < maxDepth) {
      val children = for (i <- 0 to fanout) yield {
        RawNode(node.depth + 1)
      }

      node.children ++= children
    }

    node.children foreach (n => build(n, maxDepth, branching))
  }

  def nodes(node: RawNode): Int = {
    if (node.children.size == 0) 1
    else node.children.foldLeft(1)((t, n) => t + nodes(n))
  }
}
