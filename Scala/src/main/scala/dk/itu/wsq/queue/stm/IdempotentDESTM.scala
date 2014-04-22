package dk.itu.wsq.queue.stm

import dk.itu.wsq.queue._

class IdempotentDESTM[E: Manifest] extends WorkStealingQueue[E] {
  import scala.annotation.tailrec
  import scala.concurrent.stm._

  case class Anchor(head: Int, size: Int, tag: Int)

  private var tasks: Array[E] = new Array[E](1)

  private var anchor: Ref[Anchor] = Ref(Anchor(0, 0, 0)) // (head, size, tag)

  @tailrec
  final def push(e: E): Unit = {
    val localAnchor = anchor.single()
    if(localAnchor.size == tasks.length) {
      expand()
      push(e)
    } else {
      tasks((localAnchor.head + localAnchor.size) % tasks.length) = e
      anchor.single() = Anchor(localAnchor.head, localAnchor.size + 1, localAnchor.tag + 1)
    }
  }

  final def take(): Option[E] = {
    val localAnchor = anchor.single()
    if(localAnchor.size == 0) {
      None
    } else {
      val task = tasks((localAnchor.head + localAnchor.size - 1) % tasks.length)
      anchor.single() = Anchor(localAnchor.head, localAnchor.size - 1, localAnchor.tag)
      Some(task)
    }
  }

  final def steal(): Option[E] = {
    val localAnchor = anchor.single()
    if(localAnchor.size == 0) {
      None
    } else {
      val arr = tasks
      val task = arr(localAnchor.head % arr.length)
      val h2 = localAnchor.head + 1 
      atomic {implicit txn => 
        if(anchor() == localAnchor) {
          anchor() = Anchor(h2, localAnchor.size - 1, localAnchor.tag)
          Some(task)
        }
        else {
          steal()
        }
      }
    }
  }

  final def expand(): Unit = {
    val localAnchor = anchor.single()
    val arr = new Array[E](localAnchor.size * 2)
    for (i <- 0 until localAnchor.size) {
      arr((localAnchor.head + i) % arr.length) = tasks((localAnchor.head + i) % tasks.length)
    }
    tasks = arr
  }

  final def length: Int = anchor.single().size
}
