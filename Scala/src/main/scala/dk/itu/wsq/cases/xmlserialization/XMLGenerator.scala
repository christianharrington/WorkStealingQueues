package dk.itu.wsq.cases.xmlserialization

import scala.xml._
import scala.util.Random
import scala.annotation.tailrec

object XMLGenerator {

  private val namespace = NamespaceBinding(null, null, null)

  def apply(seed: Long, depth: Int, children: Int, attributes: Int): Elem = {
    val random = new Random(seed)

    val childs = for (i <- 0 to random.nextInt(children)) yield generateXML(depth, children, attributes, random)
    val label = randomString(random)
    val attrs = randomAttributes(random, attributes)

    new Elem(null, label, attrs, TopScope, true, childs: _*)
  }

  private def randomString(random: Random): String = {
    val label = "a" + (random.alphanumeric take 5).mkString
    label
  }

  private def randomAttributes(random: Random, attributes: Int): MetaData = {
    attributes match {
      case 0 => scala.xml.Null
      case n => {
        val key = randomString(random)
        val value = randomString(random)

        new UnprefixedAttribute(key, value, randomAttributes(random, n-1))
      }
    }
  }

  private def generateXML(depth: Int, children: Int, attributes: Int, random: Random): Node = {
    depth match {
      case 0 => new Text(randomString(random))
      case d => {
        val childs = for (i <- 0 to random.nextInt(children)) yield generateXML(d-1, children, attributes, random)
        val label = randomString(random)
        val attrs = randomAttributes(random, random.nextInt(attributes))

        new Elem(null, label, attrs, TopScope, true, childs: _*)
      }
    }


  }

}