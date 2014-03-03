package dk.itu.wsq.test

import dk.itu.wsq._
import dk.itu.wsq.cases._
import scala.util.Random
import org.scalatest._

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
    val l = 100
    val r = new Random()

    val arr = new ArrayBuffer[Int](l)

    for (i <- 0 until l) {
      arr += r.nextInt(1000)
    }

    val result = QuickSortWorker.insertionSort(arr)
    for (i <- 0 until (l - 1)) {
      assert(result(i) <= result(i + 1), s"Failed at index $i: ${result(i)}, ${result(i+1)}")
    }
  }

  "Sorting using QuickSort" should "sort" in {
    val l = 1000

    val arr = new ArrayBuffer[Int](l)

    for (i <- 0 until l) {
      arr += Random.nextInt(1000)
    }

    val wp = new WorkerPool(2)
    
    val result = wp.run(arr)

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
    val l = 1000

    val arr = new ArrayBuffer[Int](l)

    for (i <- 0 until l) {
      arr += Random.nextInt(1000)
    }
    
    val result = arr.sorted

    for (i <- 0 until (l - 1)) {
      assert(result(i) <= result(i + 1), s"Failed at index $i: ${result(i)}, ${result(i+1)}\n $result")
    }
    assert(result.length == l)
  }
}
