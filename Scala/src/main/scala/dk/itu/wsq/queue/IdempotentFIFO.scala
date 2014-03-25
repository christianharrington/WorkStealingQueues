package dk.itu.wsq.queue

class IdempotentFIFO[E: Manifest] extends WorkStealingQueue[E] {
  import java.util.concurrent.atomic._
  import scala.annotation.tailrec

  private val head: AtomicInteger = new AtomicInteger(0)
  private var tail: Int = 0
  private var tasks: Array[E] = new Array[E](1)

  @tailrec
	final def push(e: E): Unit = {
    val h = head.get()
    val t = tail
    if(t >= h + tasks.length) {
      expand()
      push(e)
    } else {
      tasks(t % tasks.length) = e
      tail = t + 1 
    }
  }

	final def take(): Option[E] = {
    val h = head.get()
    val t = tail
    if(h == t) {
      None
    } else {
      val task = tasks(h % tasks.length)
      head.set(h + 1)
      Some(task)
    }
  }

  @tailrec
  final def steal(): Option[E] = {
    //println("Stealing...")
    val h = head.get()
    val t = tail
    if(h == t) {
      None
    } else {
      val arr = tasks
      val task = arr(h % arr.length)
      if(head.compareAndSet(h, h + 1)) {
        Some(task)
      }
      else {
        steal()
      }
    }
  }

  private final def expand(): Unit = {
    val size = tasks.length
    val arr = new Array[E](size * 2)
    for (i <- head.get() until tail) {
      arr(i % arr.length) = tasks(i % tasks.length)
    }
    tasks = arr
  }

  final def length: Int = tail - head.get()
}
