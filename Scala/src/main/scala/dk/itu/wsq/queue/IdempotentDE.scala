package dk.itu.wsq.queue

class IdempotentDE[E: Manifest] extends WorkStealingQueue[E] {
  import java.util.concurrent.atomic._
  import scala.annotation.tailrec

  case class Anchor(head: Int, size: Int, tag: Int)

  private var tasks: Array[E] = new Array[E](1)

  private var anchor: AtomicReference[Anchor] = 
    new AtomicReference[Anchor](Anchor(0, 0, 0)) // (head, size, tag)

  @tailrec
  final def push(e: E): Unit = {
    val localAnchor = anchor.get()
    if(localAnchor.size == tasks.length) {
      expand()
      push(e)
    } else {
      tasks((localAnchor.head + localAnchor.size) % tasks.length) = e
      anchor.set(Anchor(localAnchor.head, localAnchor.size + 1, localAnchor.tag + 1))
    }
  }

  final def take(): Option[E] = {
    val localAnchor = anchor.get()
    if(localAnchor.size == 0) {
      None
    } else {
      val task = tasks((localAnchor.head + localAnchor.size - 1) % tasks.length)
      anchor.set(Anchor(localAnchor.head, localAnchor.size - 1, localAnchor.tag))
      Some(task)
    }
  }

  @tailrec
  final def steal(): Option[E] = {
    val localAnchor = anchor.get()
    if(localAnchor.size == 0) {
      None
    } else {
      val arr = tasks
      val task = arr(localAnchor.head % arr.length)
      val h2 = localAnchor.head + 1 
      if(anchor.compareAndSet(localAnchor, Anchor(h2, localAnchor.size - 1, localAnchor.tag))) {
        Some(task)
      }
      else {
        steal()
      }
    }
  }

  final def expand(): Unit = {
    val localAnchor = anchor.get()
    val arr = new Array[E](localAnchor.size * 2)
    for (i <- 0 until localAnchor.size) {
      arr((localAnchor.head + i) % arr.length) = tasks((localAnchor.head + i) % tasks.length)
    }
    tasks = arr
  }

  final def length: Int = anchor.get().size
}
