package dk.itu.wsq.test

import dk.itu.wsq._
import dk.itu.wsq.queue._
import dk.itu.wsq.cases.xmlserialization._
import org.scalatest._
import org.scalatest.concurrent.Timeouts
import org.scalatest.time.SpanSugar._

class XMLSerializationTests 
extends FlatSpec 
with Matchers 
with QueueHelper
with Timeouts {

  "Traversing a graph of XMLNodes without idempotentQueueImpls" should "serialize to a correct XML output" in failAfter(10 seconds) {
    runWithQueueImpls(everyQueueExcept(idempotentQueueImpls: _*)) { q: QueueImpl => 

      val seed = 3012125
      val depth = 6
      val maxNoOfChildren = 10
      val noOfAttributes = 5
      val inputXML = XMLGenerator(seed, depth, maxNoOfChildren, noOfAttributes)

      val workerPool = new XMLSerializationWorkerPool(4, q)

      val result = workerPool.run(new XMLNode(inputXML, None))
      //println("Input:\n" + inputXML)
      result match {
        case Some(r) => {
          val resultXML = scala.xml.XML.loadString(r)
          //println(r)
          assert(r == inputXML.toString, "BAD XML")
        }
        case None => assert(false, "Result missing")
      }
    }
  }
/*
  "Traversing a graph of XMLNodes with all queues" should "serialize to a correct XML output" in failAfter(10000 seconds) {
    runWithEveryQueueImpl { q: QueueImpl =>  
      val seed = 3012125
      val depth = 6
      val maxNoOfChildren = 10
      val noOfAttributes = 5
      val inputXML = XMLGenerator(seed, depth, maxNoOfChildren, noOfAttributes)

      val workerPool = new XMLSerializationWorkerPool(4, q)

      val result = workerPool.run(new XMLNode(inputXML, None, 0))
      //println("Input:\n" + inputXML)
      result match {
        case Some(r) => {
          val resultXML = scala.xml.XML.loadString(r)
          //println(r)
          assert(r == inputXML.toString, "BAD XML")
        }
        case None => assert(false, "Result missing")
      }
    }
  }*/
}