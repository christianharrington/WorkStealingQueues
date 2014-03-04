package dk.itu.wsq.test

import dk.itu.wsq._
import dk.itu.wsq.cases.quicksort._
import scala.util.Random
import org.scalatest._

class QuickSortWorkerTests extends FlatSpec with Matchers {
  "Sorting using QuickSort" should "sort" in {
    val l = 1000

    val arr = Array.fill(l)(Random.nextInt(l))

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

  "InsertionSort" should "sort" in {
    val l = 100
    val r = new Random()

    val arr = Array.fill(l)(Random.nextInt(l))

    QuickSortWorker.insertionSort(arr)
    for (i <- 0 until (l - 1)) {
      assert(arr(i) <= arr(i + 1), s"Failed at index $i: ${arr(i)}, ${arr(i+1)}")
    }
  }
}