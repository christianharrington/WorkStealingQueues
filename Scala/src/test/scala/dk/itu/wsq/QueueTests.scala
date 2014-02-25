package dk.itu.wsq.test

import dk.itu.wsq._
import java.util.Random
import org.scalatest._

object Test extends App {
  import dk.itu.wsq.cases.QuickSort
  import scala.collection.mutable.ArrayBuffer

  val l = 100000
  val r = new Random()

  val arr = new ArrayBuffer[Int](l)

  for (i <- 0 until l) {
    arr += r.nextInt(1000)
  }

  val wp = new WorkerPool(QuickSort)
  val initial = new WorkUnit[ArrayBuffer[Int], ArrayBuffer[Int]](0, None, Seq(arr))

  val result = wp.run(Some(initial))
  
  println(s"Result: $result")
}

class QueueTests extends FlatSpec with Matchers {
  import scala.collection.mutable.ArrayBuffer

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

  "InsertionSort" should "sort" in {
    import dk.itu.wsq.cases.QuickSort 
    val l = 100
    val r = new Random()

    val arr = new ArrayBuffer[Int](l)

    for (i <- 0 until l) {
      arr += r.nextInt(1000)
    }

    val result = QuickSort.insertionSort(arr)
    for (i <- 0 until (l - 1)) {
      assert(result(i) <= result(i + 1), s"Failed at index $i: ${result(i)}, ${result(i+1)}")
    }
  }

  "Sorting using QuickSort" should "sort" in {
    import dk.itu.wsq.cases.QuickSort 

    val l = 10
    val r = new Random()

    val arr = new ArrayBuffer[Int](l)

    for (i <- 0 until l) {
      arr += r.nextInt(1000)
    }

    val wp   = new WorkerPool(QuickSort, 1)
    val initial = new WorkUnit[ArrayBuffer[Int], ArrayBuffer[Int]](0, None, Seq(arr))
    
    val result = wp.run(Some(initial))

    result match {
      case Some(r) => {
        for (i <- 0 until (l - 1)) {
          assert(r(i) <= r(i + 1), s"Failed at index $i: ${r(i)}, ${r(i+1)}\n $r")
        }
        assert(r.length == l)
      }
      case None => assert(false)
    }
  }

  "Sorting using Scala's quicksort implementation" should "sort" in {
    import dk.itu.wsq.cases.QuickSort 

    val l = 1000
    val r = new Random()

    val arr = new ArrayBuffer[Int](l)

    for (i <- 0 until l) {
      arr += r.nextInt(1000)
    }
    
    val result = arr.sorted

    for (i <- 0 until (l - 1)) {
      assert(result(i) <= result(i + 1), s"Failed at index $i: ${result(i)}, ${result(i+1)}\n $result")
    }
    assert(result.length == l)
  }
}
