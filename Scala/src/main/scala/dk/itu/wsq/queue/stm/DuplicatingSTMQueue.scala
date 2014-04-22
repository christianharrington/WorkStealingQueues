package dk.itu.wsq.queue.stm

import dk.itu.wsq.queue._

class DuplicatingSTMQueue[E: Manifest](val size: Int) extends WorkStealingQueue[E] {
  import scala.annotation.tailrec
  import scala.concurrent.stm._

  private val tasks: Array[Option[E]] = new Array[Option[E]](size)
  private var head: Ref[Int] = Ref(0)
  private var tail: Ref[Int] = Ref(0)
  private var tailMin = Integer.MAX_VALUE

  final def push(e: E): Unit = {
    atomic { implicit txn =>
      if(tail() < (Math.min(tailMin, head()) + size) && tail() < Integer.MAX_VALUE/2) {
        tasks(tail() % size) = Some(e)
        tail() = tail() + 1
      } else {
        if(head() > tailMin) {
          head() = tailMin
        }
        tailMin = Integer.MAX_VALUE

        val count = Math.max(0, tail() - head())

        head() = head() % size
        tail() = tail() + count
        push(e) // In the paper they run the task here.
      }
    }
  }

  final def take(): Option[E] = {
    atomic { implicit txn => 
      tail -= 1
      if(head() <= Math.min(tailMin, tail())) {
        if(tailMin > tail()) {
          tailMin = tail()
        }      
        val task = tasks(tail() % size)
        tasks(tail() % size) = None
        task
      } else {
        if(head() > tailMin) {
          head() = tailMin
        }
        tailMin = Integer.MAX_VALUE
        if(head() <= tail()) {
          val task = tasks(tail() % size)
          tasks(tail() % size) = None
          task
        } else {
          tail() = tail() + 1
          None
        } 
      }
    }
  }

  final def steal(): Option[E] = {
    atomic { implicit txn =>
      if(head() < tail()) {
        val task = tasks(head() % size)
        head() = head() + 1
        task
      } else {
        None
      }
    }
  }

  final def length: Int = tail.single() - head.single()
}
