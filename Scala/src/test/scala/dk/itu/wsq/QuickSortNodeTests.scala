package dk.itu.wsq.test

import dk.itu.wsq._
import dk.itu.wsq.cases.quicksort._
import scala.util.{ Random, Sorting }
import org.scalatest._

class QuickSortNodeTests extends FlatSpec with Matchers {
  "The two halves from a divide plus the pivot " should "equal the length of the original array" in {
    val l = 10000
    val originalArr = Array.fill(l)(Random.nextInt(l))

    val originalNode = new QuickSortNode(originalArr, Root)

    val (left, right) = originalNode.divide
    originalNode.left  = Some(left.arr)
    originalNode.right = Some(right.arr)

    assert(left.arr.length + right.arr.length + 1 === l)
  }

  it should "be bag-equal to the original array" in {
    val l = 10000
    val originalArr = Array.fill(l)(Random.nextInt(l))

    val originalNode = new QuickSortNode(originalArr, Root)

    val (left, right) = originalNode.divide

    val newArr = (left.arr :+ originalNode.pivot) ++ right.arr

    assert(originalArr.length === newArr.length, "Not equal length")

    assert((originalArr diff newArr).length === 0, "Not equal")
  }

  "Dividing and combining a QueueNode" should "return the original array" in {
    val l = 10000
    val originalArr = Array.fill(l)(Random.nextInt(l))

    val originalNode = new QuickSortNode(originalArr, Root)

    val (left, right) = originalNode.divide
    originalNode.left  = Some(left.arr)
    originalNode.right = Some(right.arr)

    val newArray = originalNode.combine

    newArray match {
      case Some(arr) => {
        assert(arr.length === l, "The new array is the wrong length")
        assert((originalArr diff arr).length === 0, "Not equal")
      }
      case None => assert(false)
    }
  }

  "InsertionSort" should "sort" in {
    val l = 100
    val r = new Random()

    val arr = Array.fill(l)(Random.nextInt(l))

    QuickSortNode.insertionSort(arr)
    for (i <- 0 until (l - 1)) {
      assert(arr(i) <= arr(i + 1), s"Failed at index $i: ${arr(i)}, ${arr(i+1)}")
    }
  }
}
