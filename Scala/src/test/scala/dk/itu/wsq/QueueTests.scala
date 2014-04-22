package dk.itu.wsq.test

import dk.itu.wsq._
import dk.itu.wsq.queue._
import org.scalatest._
import org.scalatest.concurrent.Timeouts
import org.scalatest.time.SpanSugar._

class QueueTests 
  extends FlatSpec 
  with Matchers 
  with QueueHelper 
  with Timeouts {

  type TestQ = WorkStealingQueue[Int]

  "Taking from a WSQ" should "be LIFO" in failAfter(5 seconds) {
    runWithQueues(everyQueueExcept(IdempotentFIFOImpl, IdempotentFIFOSTMImpl)) { q: TestQ =>
      val items = (1 to 100)
      items.foreach(i => q.push(i))

      items.reverse.foreach(i => Some(i) should be (q.take))
    }
  }

  "Stealing from a WSQ" should "be FIFO" in failAfter(5 seconds) {
    runWithQueues(everyQueueExcept(IdempotentLIFOImpl, IdempotentLIFOSTMImpl)) { q: TestQ =>
      val items = (1 to 100)
      items.foreach(i => q.push(i))
      
      items.foreach(i => Some(i) should be (q.steal))   
    }
  }

  "Taking from an empty WSQ" should "return None" in failAfter(5 seconds) {
    runWithEveryQueue { q: TestQ =>
      q.take() should be (None)
    }
  }

  "Stealing from an empty WSQ" should "return None" in failAfter(5 seconds) {
    runWithEveryQueue { q: TestQ =>
      q.steal() should be (None)
    }
  }
}
