package dk.itu.wsq.queue

class IdempotentDE[E: Manifest] extends WorkStealingQueue[E] {

  import java.util.concurrent.atomic._

  private var tasks: Array[E] = new Array[E](1)

  private var anchor: AtomicReference[(Int, Int, Int)] = new AtomicReference[(Int, Int, Int)]((0, 0, 0)) // (head, size, tag)

  def push(e: E): Unit = {
    val (h, s, g) = anchor.get()
    if(s == tasks.length) {
      expand()
      push(e)
    } else {
      tasks((h+s) % tasks.length) = e
      anchor.set((h, s + 1, g + 1))
    }
  }

  def take(): Option[E] = {
    val (h, s, g) = anchor.get()
    if(s == 0) {
      None
    } else {
      val task = tasks((h+s-1) % tasks.length)
      anchor.set((h, s - 1, g))
      Some(task)
    }
  }

  def steal(): Option[E] = {
    while(true) {
      val (h, s, g) = anchor.get()
      if(s == 0) {
        return None
      } else {
        val arr = tasks
        val task = arr(h % arr.length)
        val h2 = h + 1 
        if(anchor.compareAndSet((h, s, g), (h2, s - 1, g))) {
          return Some(task)
        }
      }
    }
    None
  }

    def expand(): Unit = {
    val (h, s, g) = anchor.get()
    val arr = new Array[E](s * 2)
    for (i <- 0 until s) {
      arr((h + i) % arr.length) = tasks((h + i) % tasks.length)
    }
    tasks = arr
  }

  def length: Int = anchor.get()._2
}