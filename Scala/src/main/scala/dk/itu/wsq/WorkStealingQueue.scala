package dk.itu.wsq

import java.util.concurrent.atomic._

class Age(@volatile var tag: Int, @volatile var top: Int) {
  override def equals(that: Any) = that match {
    case t: Age => tag == t.tag && top == t.top
    case _      => false
  }
}

class WorkStealingQueue[T] {
  var age = new AtomicReference[Age](new Age(0, 0))
  @volatile var bottom: Int = 0
  val queue = new AtomicReferenceArray[T](512)

  def push(v: T): Unit = {
    @volatile var localBot = bottom
    queue.set(localBot, v)
    localBot = localBot + 1
    bottom = localBot
  }

  def take(): Option[T] = {
    var localBot = bottom

    if (localBot == 0) None else {
      localBot = localBot - 1
      bottom = localBot

      val v = queue.get(localBot)
      val oldAge = age.get
      if (localBot > oldAge.top) {
        Some(v)
      }
      else {
        bottom = 0
        val newAge = new Age(oldAge.tag + 1, 0)
        if (localBot == oldAge.top) {
          if (age.compareAndSet(oldAge, newAge)) {
            Some(v)
          }
          else {
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

  def steal(): Option[T] = {
    val oldAge = age.get
    val localBot = bottom

    if (localBot <= oldAge.top) {
      None
    }
    else {
      val v = queue.get(oldAge.top)
      val newAge = oldAge
      newAge.top = newAge.top + 1

      if (age.compareAndSet(oldAge, newAge)) {
        Some(v)
      }
      else {
        None
      }
    }
  }
}
