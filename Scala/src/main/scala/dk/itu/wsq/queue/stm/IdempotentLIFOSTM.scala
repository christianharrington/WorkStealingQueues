package dk.itu.wsq.queue

import dk.itu.wsq.queue._

class IdempotentLIFOSTM[E: Manifest] extends WorkStealingQueue[E] {
  import scala.concurrent.stm._
  import scala.annotation.tailrec

  case class Anchor(tail: Int, tag: Int)

  private val anchor: Ref[Anchor] = Ref(Anchor(0, 0)) // (tail, tag)
  private var capacity: Int = 1
  private var tasks: Array[E] = new Array[E](capacity)

  @tailrec
	final def push(e: E): Unit = {    
    val localAnchor = anchor.single()

    if(localAnchor.tail == capacity) {
      // Capacity limit reached, expand and try again
      expand() 
      push(e)
    } else {
      // Else put the element and write to anchor with read values plus one
      tasks(localAnchor.tail) = e
      anchor.single() = Anchor(localAnchor.tail + 1, localAnchor.tag + 1)
    }
  }

	final def take(): Option[E] = {    
    val localAnchor = anchor.single()

    if(localAnchor.tail == 0) {
      None
    } else {
      val task = tasks(localAnchor.tail - 1)
      anchor.single() = Anchor(localAnchor.tail - 1, localAnchor.tag)
      Some(task)
    }
  }

  final def steal(): Option[E] = {
    val localAnchor = anchor.single()
    if(localAnchor.tail == 0) {
      None
    } else {
      val arr = tasks
      val task = arr(localAnchor.tail - 1)
      atomic { implicit txn => 
        if(anchor.single() == localAnchor) {
          anchor.single() = Anchor(localAnchor.tail - 1, localAnchor.tag)
          Some(task)
        }
        else {
          steal()
        }
      }
    }
  }

  final private def expand(): Unit = {
    val newCapacity = capacity * 2
    val arr = new Array[E](newCapacity)

    for(i <- 0 until capacity) { arr(i) = tasks(i) }

    tasks = arr
    capacity = newCapacity
  }

  final def length: Int = anchor.single().tail
}
