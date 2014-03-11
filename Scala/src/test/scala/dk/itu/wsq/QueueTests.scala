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
    val cl = new ChaseLevQueue[Int]()
    val cls = new ChaseLevNaiveShrinkingQueue[Int]()
    val ilifo = new LifoIWSQueue[Int]()
    takingFromAWSQ(abp)
    takingFromAWSQ(cl)
    takingFromAWSQ(cls)
    takingFromAWSQ(ilifo)
  }

  private def stealingFromAWSQ(queue: WorkStealingQueue[Int]) = {
    val items = (1 to 100)
    items.foreach(i => queue.push(i))
    
    items.foreach(i => Some(i) should be (queue.steal))    
  }

  "Stealing from a WSQ" should "be FIFO" in {
    val abp = new ABPQueue[Int]()
    val cl = new ChaseLevQueue[Int]()
    val cls = new ChaseLevNaiveShrinkingQueue[Int]()
    stealingFromAWSQ(abp)
    stealingFromAWSQ(cl)
    stealingFromAWSQ(cls)
  }

  private def takingFromAnEmptyWSQ(queue: WorkStealingQueue[Int]) = {
    queue.take() should be (None)
  }

  "Taking from an empty WSQ" should "return None" in {
    val abp = new ABPQueue[Int]()
    val cl = new ChaseLevQueue[Int]()
    val cls = new ChaseLevNaiveShrinkingQueue[Int]()
    val ilifo = new LifoIWSQueue[Int]()
    takingFromAnEmptyWSQ(abp)
    takingFromAnEmptyWSQ(cl)
    takingFromAnEmptyWSQ(cls)
    takingFromAnEmptyWSQ(ilifo)
  }

  private def stealingFromAnEmptyWSQ(queue: WorkStealingQueue[Int]) = {
    queue.steal() should be (None)
  }

  "Stealing from an empty WSQ" should "return None" in {
    val abp = new ABPQueue[Int]()
    val cl = new ChaseLevQueue[Int]()
    val cls = new ChaseLevNaiveShrinkingQueue[Int]()
    val ilifo = new LifoIWSQueue[Int]()
    stealingFromAnEmptyWSQ(abp)
    stealingFromAnEmptyWSQ(cl)
    stealingFromAnEmptyWSQ(cls)
    stealingFromAnEmptyWSQ(ilifo)
  }
}
