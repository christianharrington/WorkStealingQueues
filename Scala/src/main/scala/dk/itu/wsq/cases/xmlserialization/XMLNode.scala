package dk.itu.wsq.cases.xmlserialization

import dk.itu.wsq._
import scala.collection.mutable.StringBuilder
import scala.collection.immutable.HashSet
import java.util.concurrent.atomic.AtomicReference
import scala.xml.{ Elem, Node }

class XMLNode(val elem: Node, val parent: Option[XMLNode]) extends dk.itu.wsq.Node {

  var children: Seq[XMLNode] = Seq()

  var processedChildren: AtomicReference[Set[XMLNode]] = new AtomicReference(new HashSet[XMLNode]())

  var stringValue: Option[String] = None

  def divide: Seq[XMLNode] = {
    children = elem.child map { c => new XMLNode(c, Some(this)) }

    children
  }

  def serialize: Option[String] = {
    if (processedChildren.get.size == elem.child.length) {
      elem match {
        case el: Elem => {
          val builder: StringBuilder = new StringBuilder()
          val prefix = if (el.prefix != null) el.prefix + ":" else "" 
          val label = el.label
          val attributes = el.attributes.mkString(" ")
          val headerClose = if (el.child.isEmpty) "/>\n" else ">\n"

          builder += '<' ++= prefix ++= label += ' ' ++= attributes ++= headerClose

          var failure = false

          for (child <- children) {
            child.stringValue match {
              case Some(s) => builder ++= s += '\n'
              case None    => failure = true
            }
          }

          builder ++= "</" ++= label ++= ">\n"
          if (!failure) stringValue = Some(builder.toString)

          stringValue
        }
        case n: Node => {
          stringValue = Some(n.toString())

          stringValue
        }
      }
    } else None
  }

  def notifyParent(): Unit = {
    parent match {
      case Some(p) => p.processedChildren.set(p.processedChildren.get + this)
      case None    => ()
    }
  }

}