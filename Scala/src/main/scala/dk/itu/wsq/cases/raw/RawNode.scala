package dk.itu.wsq.cases.raw

import dk.itu.wsq._

case class RawNode(val depth: Int) extends Node {
  import scala.collection.mutable
  import java.util.concurrent.atomic.AtomicBoolean

  val visited = new AtomicBoolean(false)

  val children = new mutable.ArrayBuffer[RawNode]()
}
