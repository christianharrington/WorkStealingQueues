package dk.itu.wsq.cases.spanningtree

import dk.itu.wsq._
import scala.collection.mutable
import java.util.concurrent.atomic.AtomicBoolean

case class SpanningTreeNode(val id: Int) extends Node {
  private var _color: Option[Int] = None

  private def adopt(child: SpanningTreeNode): Unit = {
    child.parent = Some(this)
  }

  var visited: AtomicBoolean = new AtomicBoolean(false)

  def color = _color

  def children: mutable.HashSet[SpanningTreeNode] = {    
    neighbors filter { n => n.parent == Some(this) }
  }

  val neighbors: mutable.HashSet[SpanningTreeNode] = mutable.HashSet[SpanningTreeNode]()

  var parent: Option[SpanningTreeNode] = None

  def paint(brush: Int): Unit = {
    _color = Some(brush)
  }

  def visit(worker: SpanningTreeWorker): Unit = {
    if (visited.compareAndSet(false, true)) {
      worker.visitCounter += 1
    }
    paint(worker.color)
  }

  def traverse(worker: SpanningTreeWorker): Unit = {
    visit(worker)
    for (n <- neighbors) { // BFS
      if (n.color.isEmpty) {
        n.visit(worker)
        adopt(n)
        
        worker.addToQueue(n)
      }
    }
  }

  override def equals(that: Any): Boolean = {
    that match {
      case other: SpanningTreeNode => this.id == other.id
      case _ => false
    }
  }

  def reset(): Unit = {
    _color = None
    parent = None
    visited.set(false)
  }
}
