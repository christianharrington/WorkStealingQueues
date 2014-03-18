package dk.itu.wsq.queue

class DuplicatingQueue[E: Manifest](val size: Int) extends WorkStealingQueue[E] {

  private var tasks: Array[Option[E]] = new Array[Option[E]](size)
  @volatile private var head: Int = 0
  @volatile private var tail: Int = 0
  private var tailMin = Integer.MAX_VALUE

  def push(e: E): Unit = {
    if(tail < (Math.min(tailMin, head) + size) && tail < Integer.MAX_VALUE/2) {
      tasks(tail % size) = Some(e)
      tail += 1
    } else {
      this.synchronized {
        if(head > tailMin) {
          head = tailMin
        }
        tailMin = Integer.MAX_VALUE

        val count = Math.max(0, tail - head)

        head = head % size
        tail = tail + count
      }
      push(e) // In the paper they run the task here.
    }
  }

  def take(): Option[E] = {
    tail -= 1
    if(head <= Math.min(tailMin, tail)) {
      if(tailMin > tail) {
        tailMin = tail
      }      
      val task = tasks(tail % size)
      tasks(tail % size) = None
      task
    } else {
      this.synchronized {
        if(head > tailMin) {
          head = tailMin
        }
        tailMin = Integer.MAX_VALUE
        if(head <= tail) {
          val task = tasks(tail % size)
          tasks(tail % size) = None
          task
        } else {
          tail += 1
          None
        }
      } 
    }
  }

  def steal(): Option[E] = {
    this.synchronized {
      if(head < tail) {
        val task = tasks(head % size)
        head += 1
        task
      } else {
        None
      }
    }
  }

  def length: Int = tail - head
}
