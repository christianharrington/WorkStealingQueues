package dk.itu.wsq.queue.stm

import dk.itu.wsq.queue._

import scala.concurrent.stm._

class ChaseLevSTMQueue[E: Manifest] extends WorkStealingQueue[E] {
  import java.util.concurrent.atomic._
  
  // Never decremented. Assumed to never overflow.
  protected var top: Ref[Long] = Ref(0)
  // Indicates where next element is pushed
  protected var bottom: Long = 0
  
  // Initial size is 2^logInitialSize
  protected val logInitialSize = 2
  protected var activeArray = new CircularArray[E](logInitialSize)

  final def push(element: E): Unit = {
    val b = bottom
    val t = top.single()
    var arr = activeArray
    val size = b - t
    // If the array is too small, grow the array
    if (size >= arr.size - 1) {
      arr = arr.grow(b, t)
      activeArray = arr
    }

    arr(b) = element
    bottom = b + 1
  }

  final def steal(): Option[E] = {
    val t = top.single()
    val b = bottom
    val arr = activeArray
    val size = b - t

    if (size <= 0) {
      None
    } else {
      val elem = arr(t) // Get top element

      atomic { implicit txn =>
        if (top() == t) {
          top() = t + 1
          Some(elem)
        } else {
          None
        }
      }
    }
  }

  def take(): Option[E] = {
    var b = bottom
    val arr = activeArray
    b = b - 1
    bottom = b
    val t = top.single()
    val size = b - t

    if (size < 0) {
      bottom = t
      None
    } else {
      val elem = arr(b) // Get bottom element

      if (size > 0) {
        Some(elem)
      } else {
        bottom = t + 1

        atomic { implicit txn =>
          if (top() == t) {
            top() = t + 1
            Some(elem)
          } else {
            None
          }
        }
      }
    }
  }

  final def length = (atomic { implicit txn => bottom - top() }).toInt
}

trait ChaseLevShrinkingSTMQueue[E] extends ChaseLevSTMQueue[E] {
  protected def perhapsShrink(bottom: Long, top: Long): Unit

  override final def take(): Option[E] = {
    var b = bottom
    val arr = activeArray
    b = b - 1
    bottom = b
    val t = top.single()
    val size = b - t

    if (size < 0) {
      bottom = t
      None
    } else {
      val elem = arr(b)

      if (size > 0) {
        perhapsShrink(b, t)
        Some(elem)
      } else {
        bottom = t + 1

        atomic { implicit txn =>
          if (top() == t) {
            top() = t + 1
            Some(elem)
          } else {
            None
          }
        }
      }
    }    
  }
}

class ChaseLevNaiveShrinkingSTMQueue[E: Manifest] extends ChaseLevShrinkingQueue[E] {
  private val shrinkingConstant = 4 // Should be ≥ 3

  protected final def perhapsShrink(bottom: Long, top: Long): Unit = {
    val arr = activeArray
    if ((bottom - top) < (arr.size / shrinkingConstant)) {
      val newArray = arr.shrinkNaive(bottom, top)
      activeArray = newArray
    }
  }
}
