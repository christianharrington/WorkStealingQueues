package dk.itu.wsq.cases

import dk.itu.wsq.{Task, WorkUnit}
import scala.collection.mutable.ArrayBuffer

object QuickSort {
  def insertionSort(arr: ArrayBuffer[Int]): ArrayBuffer[Int] = {
    var x = 0
    var j = 0

    for (i <- 0 until arr.length) {
      x = arr(i)
      j = i
      while (j > 0 && arr(j - 1) > x) {
        arr(j) = arr(j -1)
        j = j - 1
      }
      arr(j) = x
    }

    arr
  }
}

class QuickSort(toSort: ArrayBuffer[Int]) extends Task[ArrayBuffer[Int], ArrayBuffer[Int]] {
  type WU = WorkUnit[ArrayBuffer[Int], ArrayBuffer[Int]]
  
  val random = new java.util.Random()

  private val threshold = 100

  def run(in: WU): Either[Seq[WU], ArrayBuffer[Int]] = {
    val arr = in.input.head
    
    if (arr.length <= threshold) {
      Right(QuickSort.insertionSort(arr))
    } else {
      val pivotPoint = random.nextInt(arr.length)
      val pivot = arr(pivotPoint)
      
      val leftSide  = new ArrayBuffer[Int]()
      val rightSide = new ArrayBuffer[Int]()
      
      for (i <- 0 until pivotPoint) {
        (if (arr(i) < pivot) leftSide else rightSide) += arr(i)
      }
      
      for (i <- pivotPoint + 1 until arr.length) {
        (if (arr(i) < pivot) leftSide else rightSide) += arr(i)
      }

      in.neededResults = 3
      in.hasRun()
      
      val pivotBuffer = new ArrayBuffer[Int](1)
      pivotBuffer += pivot
      in.addResult(0, pivotBuffer)
      
      val left  = new WorkUnit[ArrayBuffer[Int], ArrayBuffer[Int]](1, Some(in), Seq(leftSide))
      val right = new WorkUnit[ArrayBuffer[Int], ArrayBuffer[Int]](2, Some(in), Seq(rightSide))

      Left(Seq(left, right))
    }
  }

  def complete(in: WU): ArrayBuffer[Int] = {
    val pivot = in.results.head
    val arrs = in.results.tail
    arrs.head ++= pivot ++= arrs.tail.flatten
  }
}
