package dk.itu.wsq.queue

class IdempotentLIFO[E: Manifest] extends WorkStealingQueue[E] {
  import java.util.concurrent.atomic._
  import scala.annotation.tailrec

  case class Anchor(tail: Int, tag: Int)

  private val anchor: AtomicReference[Anchor] = new AtomicReference[Anchor](Anchor(0, 0)) // (tail, tag)
  private var capacity: Int = 1
  private var tasks: Array[E] = new Array[E](capacity)

  @tailrec
	final def push(e: E): Unit = {    
    val localAnchor = anchor.get()

    if(localAnchor.tail == capacity) {
      // Capacity limit reached, expand and try again
      expand() 
      push(e)
    } else {
      // Else put the element and write to anchor with read values plus one
      tasks(localAnchor.tail) = e
      anchor.set(Anchor(localAnchor.tail + 1, localAnchor.tag + 1))
    }
  }

	final def take(): Option[E] = {    
    val localAnchor = anchor.get()

    if(localAnchor.tail == 0) {
      None
    } else {
      val task = tasks(localAnchor.tail - 1)
      anchor.set(Anchor(localAnchor.tail - 1, localAnchor.tag))
      Some(task)
    }
  }

  @tailrec
  final def steal(): Option[E] = {
    val localAnchor = anchor.get()
    if(localAnchor.tail == 0) {
      None
    } else {
      val arr = tasks
      val task = arr(localAnchor.tail - 1)
      if(anchor.compareAndSet(localAnchor, Anchor(localAnchor.tail - 1, localAnchor.tag))) {
        Some(task)
      }
      else {
        steal()
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

  final def length: Int = anchor.get().tail
}
