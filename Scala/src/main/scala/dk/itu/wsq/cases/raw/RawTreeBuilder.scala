package dk.itu.wsq.cases.raw

import dk.itu.wsq._

object RawTreeBuilder {
  import scala.util.Random

  def build(depth: Int, branching: Int)(random: Random): RawNode = {
    val fanout = random.nextInt(branching) + 1

    val children = depth match {
      case 0 => Seq()
      case d => for (i <- 0 to fanout) yield {
        build(d - 1, branching)(random)
      }
    }

    RawNode(children)
  }

  def nodes(node: RawNode): Int = {
    if (node.children.size == 0) 1
    else node.children.foldLeft(1)((t, n) => t + nodes(n))
  }
}
