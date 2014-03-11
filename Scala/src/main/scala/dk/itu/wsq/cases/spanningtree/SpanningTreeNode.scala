package dk.itu.wsq.cases.spanningtree

import dk.itu.wsq._
import scala.collection.mutable.{ HashSet, Set }

case class SpanningTreeNode() extends Node {

  private var color: Option[Int] = None
  private var _hasVisitedAllNeighbors: Boolean = false

  private def adopt(child: SpanningTreeNode): Unit = {
    child.parent = Some(this)
  }

  var neighbors: Set[SpanningTreeNode] = new HashSet[SpanningTreeNode]()

  var parent: Option[SpanningTreeNode] = None

  def paint(brush: Int): Unit = {
    color = Some(brush)
  }

  def hasVisitedAllNeighbors: Boolean = _hasVisitedAllNeighbors

  def traverse(worker: SpanningTreeWorker): Unit = {
    if (!hasVisitedAllNeighbors) {
      var neighborsColored = 0
      for (n <- neighbors) { // BFS
        if (n.color.isEmpty) {
          n.paint(worker.color)

          adopt(n)

          worker.addToQueue(n)
          neighborsColored += 1
        }
      }

      if (neighborsColored == 0) _hasVisitedAllNeighbors = true
    }
  }
  
}