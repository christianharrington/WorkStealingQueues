package dk.itu.wsq.test

import dk.itu.wsq._
import dk.itu.wsq.queue._
import scala.util.Random
import org.scalatest._

class QueueTests extends FlatSpec with Matchers with QueueHelper {
  import scala.collection.mutable.ArrayBuffer

  type TestQ = WorkStealingQueue[Int]

  "Taking from a WSQ" should "be LIFO" in runWithQueues(everyQueueExcept(IdempotentFIFOImpl): _*) { q: TestQ =>
    val items = (1 to 100)
    items.foreach(i => q.push(i))

    items.reverse.foreach(i => Some(i) should be (q.take))
  }

  "Stealing from a WSQ" should "be FIFO" in runWithQueues(everyQueueExcept(IdempotentLIFOImpl): _*) { q: TestQ =>
    val items = (1 to 100)
    items.foreach(i => q.push(i))
    
    items.foreach(i => Some(i) should be (q.steal))   
  }

  "Taking from an empty WSQ" should "return None" in runWithEveryQueue { q: TestQ =>
    q.take() should be (None)
  }

  "Stealing from an empty WSQ" should "return None" in runWithEveryQueue { q: TestQ =>
    q.steal() should be (None)
  }
}
