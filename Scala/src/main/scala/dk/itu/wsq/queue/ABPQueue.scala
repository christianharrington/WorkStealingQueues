package dk.itu.wsq.queue

object ABPQueue extends QueueImplementation {
  override def toString(): String = "ABP Queue"
}

import java.util.concurrent.atomic._

case class Tag(val value: Int) extends AnyVal

case class Age(val tag: Tag, val top: Int) {
  override def equals(that: Any) = that match {
    case t: Age => tag == t.tag && top == t.top
    case _      => false
  }
}

class ABPQueue[E] extends WorkStealingQueue[E] {
  private val age = new AtomicReference[Age](Age(Tag(0), 0))
  private var bottom: Int = 0
  private val queue = new AtomicReferenceArray[E](512)

  def push(v: E): Unit = {
    val localBot = bottom
    queue.set(localBot, v)
    bottom = localBot + 1
  }

  def take(): Option[E] = {
    val oldBottom = bottom

    if (oldBottom == 0) {
      None
    }
    else {
      val localBot = oldBottom - 1
      bottom = localBot

      val v = queue.get(localBot)
      val oldAge = age.get
      
      if (localBot > oldAge.top) {
        Some(v)
      }
      else {
        bottom = 0
        val newAge = new Age(Tag(oldAge.tag.value + 1), 0)
        if (localBot == oldAge.top) {
          if (age.compareAndSet(oldAge, newAge)) {
            Some(v)
          }
          else {
            age.set(newAge)
            None
          }
        }
        else {
          age.set(newAge)
          None
        }
      }
    }
  }

  def steal(): Option[E] = {
    val oldAge = age.get
    val localBot = bottom

    if (localBot <= oldAge.top) {
      None
    }
    else {
      val v = queue.get(oldAge.top)
      val newAge = Age(oldAge.tag, oldAge.top + 1)

      if (age.compareAndSet(oldAge, newAge)) {
        Some(v)
      }
      else {
        None
      }
    }
  }

  def length = bottom - age.get.top
}
