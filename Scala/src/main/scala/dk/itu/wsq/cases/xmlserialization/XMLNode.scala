package dk.itu.wsq.cases.xmlserialization

import dk.itu.wsq._
import scala.collection.mutable.StringBuilder
import java.util.concurrent.atomic.AtomicInteger
import scala.xml.{ Node => XNode, Elem, MetaData, PCData, Text, Null }

class XMLNode(val elem: XNode, val parent: Option[XMLNode]) extends Node {

  var children: Seq[XMLNode] = Seq()

  var processedChildren: AtomicInteger = new AtomicInteger(0)

  var stringValue: Option[String] = None

  def divide: Seq[XMLNode] = {
    children = elem.child map { c => new XMLNode(c, Some(this)) }

    children
  }

  private def attributesAsString(attr: MetaData): List[String] = {
    attr match {
      case scala.xml.Null => Nil
      case _ => (attr.key + "=\"" + attr.value.mkString + "\"") :: attributesAsString(attr.next)
    }
  }

  def serialize: Option[String] = {
    if (processedChildren.get == elem.child.length) {
      elem match {
        case el: Elem => {
          val builder: StringBuilder = new StringBuilder()
          val prefix = if (el.prefix != null) el.prefix + ":" else "" 
          val label = el.label
          val attributes = if (!el.attributes.isEmpty) " " + attributesAsString(el.attributes).mkString(" ") else ""
          val headerClose = if (el.child.isEmpty) "/>" else ">"
          //println(label + " : " + attributes)

          builder += '<' ++= prefix ++= label ++= attributes ++= headerClose

          var failure = false

          for (child <- children) {
            child.stringValue match {
              case Some(s) => builder ++= s
              case None    => failure = true
            }
          }

          if (!el.child.isEmpty) builder ++= "</" ++= label ++= ">"
          val s = builder.toString
          if (!failure) stringValue = Some(s)
          //println(s)
          
          stringValue
        }
        case p: PCData => {
          stringValue = Some(p.text)

          stringValue
        }
        case t: Text => {
          stringValue = Some(t.text)

          stringValue
        }
        case n: XNode => {
          stringValue = Some(n.toString)

          stringValue
        }
      }
    } else None
  }

  def readyToSerialize(): Boolean = {
    processedChildren.incrementAndGet() == elem.child.length
  }

}