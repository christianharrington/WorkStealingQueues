package dk.itu.wsq.queue

class LifoIWSQueue[E: Manifest] extends WorkStealingQueue[E] {

  import java.util.concurrent.atomic._

  private var anchor: AtomicReference[(Int, Int)] = new AtomicReference[(Int, Int)]((0, 0)) // (tail, tag)
  private var capacity: Int = 1
  private var tasks: Array[E] = new Array[E](capacity)

	def push(e: E): Unit = {    
    val (tail, tag) = anchor.get()

    if(tail == capacity) {
      // Capacity limit reached, expand and try again
      expand() 
      push(e)
    } else {
      // Else put the element and write to anchor with read values plus one
      tasks(tail) = e
      anchor.set((tail + 1, tag + 1))
    }
  }

	def take(): Option[E] = {    
    val (tail, tag) = anchor.get()

    if(tail == 0) {
      None
    } else {
      val task = tasks(tail - 1)
      anchor.set((tail - 1, tag))
      Some(task)
    }
  }

  def steal(): Option[E] = {
    while(true) {
      var (tail, tag) = anchor.get()
      if(tail == 0) {
        return None
      } else {
        val arr = tasks
        var task = arr(tail - 1)
        if(anchor.compareAndSet((tail, tag), (tail - 1, tag))) {
          return Some(task)
        }
      }
    } 
    None
  }

  def expand(): Unit = {
    val newCapacity = capacity * 2
    val arr = new Array[E](newCapacity)

    for(i <- 0 until capacity) { arr(i) = tasks(i) }

    tasks = arr
    capacity = newCapacity
  }

  def length: Int = anchor.get()._1
}
