package dk.itu.wsq.test

import dk.itu.wsq._
import dk.itu.wsq.cases.quicksort._
import dk.itu.wsq.queue._
import scala.util.Random
import org.scalatest._

class QuickSortWorkerTests extends FlatSpec with Matchers {
  "Sorting using QuickSort" should "sort" in {
    val l = 1000

    val arr = Array.fill(l)(Random.nextInt(l))

    val wp = new QuickSortWorkerPool(2, ABPQueue)
    
    val result = wp.run(QuickSortNode(arr, Root()))

    result match {
      case Some(r) => {
        for (i <- 0 until (l - 1)) {
          assert(r(i) <= r(i + 1), s"Failed at index $i: ${r(i)}, ${r(i+1)}\n {$r}")
        }
        assert(r.length == l)
      }
      case None => assert(false)
    }
  }
}
