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

  "Traversing a graph of XMLNodes" should "serialize to a correct XML output" in failAfter(5 seconds) {
    runWithQueueImpls(everyQueueExcept(ABPQueueImpl, IdempotentLIFOImpl, IdempotentFIFOImpl, IdempotentDEImpl)) { q: QueueImpl =>  
      val booksElem = scala.xml.XML.loadFile("src/test/resources/books.xml")

      val workerPool = new XMLSerializationWorkerPool(4, q)

      val result = workerPool.run(new XMLNode(booksElem, None))

      result match {
        case Some(r) => {
          val resultXML = scala.xml.XML.loadString(r)
          assert(resultXML == booksElem, "BAD XML")
        }
        case None => assert(false, "Result missing")
      }
    }
  }
}