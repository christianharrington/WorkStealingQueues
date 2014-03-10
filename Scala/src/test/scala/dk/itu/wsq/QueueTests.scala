package dk.itu.wsq.test

import dk.itu.wsq._
import dk.itu.wsq.queue._
import scala.util.Random
import org.scalatest._

class QueueTests extends FlatSpec with Matchers {
  import scala.collection.mutable.ArrayBuffer

  private def takingFromAWSQ(queue: WorkStealingQueue[Int]) = {
    val items = (1 to 100)
    items.foreach(i => queue.push(i))

    items.reverse.foreach(i => Some(i) should be (queue.take))    
  }

  "Taking from a WSQ" should "be LIFO" in {
    val abp = new ABPQueue[Int]()
    takingFromAWSQ(abp)
  }

  private def stealingFromAWSQ(queue: WorkStealingQueue[Int]) = {
    val items = (1 to 100)
    items.foreach(i => queue.push(i))
    
    items.foreach(i => Some(i) should be (queue.steal))    
  }

  "Stealing from a WSQ" should "be FIFO" in {
    val abp = new ABPQueue[Int]()
    stealingFromAWSQ(abp)
  }

  private def takingFromAnEmptyWSQ(queue: WorkStealingQueue[Int]) = {
    queue.take() should be (None)
  }

  "Taking from an empty WSQ" should "return None" in {
    val abp = new ABPQueue[Int]()
    takingFromAnEmptyWSQ(abp)
  }

  private def stealingFromAnEmptyWSQ(queue: WorkStealingQueue[Int]) = {
    queue.steal() should be (None)
  }

  "Stealing from an empty WSQ" should "return None" in {
    val abp = new ABPQueue[Int]()
    stealingFromAnEmptyWSQ(abp)
  }
}
