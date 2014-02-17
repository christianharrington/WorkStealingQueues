package dk.itu.wsq.test

import org.scalatest._

class QueueTests extends FlatSpec with Matchers {
  import dk.itu.wsq.WorkStealingQueue

  "Taking from a WSQ" should "be LIFO" in {
    val queue = new WorkStealingQueue[Int]()
    val items = (1 to 100)
    items.foreach(i => queue.push(i))
    
    items.reverse.foreach(i => Some(i) should be (queue.take))
  }

  "Stealing from a WSQ" should "be FIFO" in {
    val queue = new WorkStealingQueue[Int]()
    val items = (1 to 100)
    items.foreach(i => queue.push(i))
    
    items.foreach(i => Some(i) should be (queue.steal))
  }

  "Taking from an empty WSQ" should "return None" in {
    val queue = new WorkStealingQueue[Int]()
    queue.take() should be (None)
  }

  "Stealing from an empty WSQ" should "return None" in {
    val queue = new WorkStealingQueue[Int]()
    queue.steal() should be (None)
  }
}
