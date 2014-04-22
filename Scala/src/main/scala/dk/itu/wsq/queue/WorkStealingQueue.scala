package dk.itu.wsq.queue

import dk.itu.wsq.queue.stm._

object `package` {
  val allQueueImpls = Seq(
    ABPQueueImpl,
    ABPSTMQueueImpl,
    ChaseLevQueueImpl,
    ChaseLevNaiveShrinkingQueueImpl,
    ChaseLevSTMQueueImpl,
    ChaseLevNaiveShrinkingSTMQueueImpl,
    IdempotentLIFOImpl,
    IdempotentLIFOSTMImpl,
    IdempotentFIFOImpl,
    IdempotentFIFOSTMImpl,
    IdempotentDEImpl,
    IdempotentDESTMImpl,
    DuplicatingQueueImpl,
    DuplicatingSTMQueueImpl)

  val idempotentQueueImpls = Seq(
    IdempotentLIFOImpl,
    IdempotentLIFOSTMImpl,
    IdempotentFIFOImpl,
    IdempotentFIFOSTMImpl,
    IdempotentDEImpl,
    IdempotentDESTMImpl,
    DuplicatingQueueImpl,
    DuplicatingSTMQueueImpl)
}

sealed abstract class QueueImpl
object ABPQueueImpl extends QueueImpl {
  override def toString(): String = "ABP Queue"
}
object ABPSTMQueueImpl extends QueueImpl {
  override def toString(): String = "ABP STM Queue"
}
object ChaseLevQueueImpl extends QueueImpl {
  override def toString(): String =  "Chase-Lev Queue" 
}
object ChaseLevNaiveShrinkingQueueImpl extends QueueImpl {
  override def toString(): String = "Chase-Lev Naive Shrinking Queue"
}
object ChaseLevSTMQueueImpl extends QueueImpl {
  override def toString(): String = "Chase-Lev STM Queue"
}
object ChaseLevNaiveShrinkingSTMQueueImpl extends QueueImpl {
  override def toString(): String = "Chase-Lev Naive Shrinking STM Queue"
}
object IdempotentLIFOImpl extends QueueImpl {
  override def toString(): String = "Idempotent Work Stealing Queue (LIFO)"
}
object IdempotentLIFOSTMImpl extends QueueImpl {
  override def toString(): String = "Idempotent Work Stealing STM Queue (LIFO)"
}
object IdempotentFIFOImpl extends QueueImpl {
  override def toString(): String = "Idempotent Work Stealing Queue (FIFO)"
}
object IdempotentFIFOSTMImpl extends QueueImpl {
  override def toString(): String = "Idempotent Work Stealing STM Queue (FIFO)"
}
object IdempotentDEImpl extends QueueImpl {
  override def toString(): String = "Idempotent Work Stealing Queue (Double-Ended)"
}
object IdempotentDESTMImpl extends QueueImpl {
  override def toString(): String = "Idempotent Work Stealing STM Queue (Double-Ended)"
}
object DuplicatingQueueImpl extends QueueImpl {
  override def toString(): String = "Duplicating Queue"
}
object DuplicatingSTMQueueImpl extends QueueImpl {
  override def toString(): String = "Duplicating STM Queue"
}

trait WorkStealingQueue[E] {
	def push(e: E): Unit

	def take(): Option[E]

  def steal(): Option[E]

  def length: Int
}

trait QueueHelper {
  def queueImplToQueue[E: Manifest](q: QueueImpl): WorkStealingQueue[E] = {
    q match {
      case ABPQueueImpl                       => new ABPQueue[E](512)
      case ABPSTMQueueImpl                    => new ABPSTMQueue[E](512)
      case ChaseLevQueueImpl                  => new ChaseLevQueue[E]()
      case ChaseLevNaiveShrinkingQueueImpl    => new ChaseLevNaiveShrinkingQueue[E]()
      case ChaseLevSTMQueueImpl               => new ChaseLevSTMQueue[E]()
      case ChaseLevNaiveShrinkingSTMQueueImpl => new ChaseLevNaiveShrinkingSTMQueue[E]()
      case IdempotentLIFOImpl                 => new IdempotentLIFO[E]()
      case IdempotentLIFOSTMImpl              => new IdempotentLIFOSTM[E]()
      case IdempotentFIFOImpl                 => new IdempotentFIFO[E]()
      case IdempotentFIFOSTMImpl              => new IdempotentFIFOSTM[E]()
      case IdempotentDEImpl                   => new IdempotentDE[E]()
      case IdempotentDESTMImpl                => new IdempotentDESTM[E]()
      case DuplicatingQueueImpl               => new DuplicatingQueue[E](512)
      case DuplicatingSTMQueueImpl            => new DuplicatingSTMQueue[E](512)
    }
  }

  def runWithQueues[E: Manifest]
    (qs: Seq[QueueImpl])
    (f: WorkStealingQueue[E] => Unit): Unit = {
    val queues = for (q <- qs) yield {
      queueImplToQueue(q)
    }

    queues foreach (q => f(q))
  }

  def runWithQueues[E: Manifest]
    (qs: QueueImpl*): WorkStealingQueue[E] => Unit = {
    runWithQueues(qs: _*)
  }

  def runWithEveryQueue[E: Manifest](f: WorkStealingQueue[E] => Unit): Unit = {
    runWithQueues(allQueueImpls)(f)
  }

  def runWithQueueImpls[E: Manifest]
    (qs: Seq[QueueImpl])
    (f: QueueImpl => Unit) : Unit = {
    qs foreach (q => f(q))
  }

  def runWithQueueImpls[E: Manifest]
    (qs: QueueImpl*): QueueImpl => Unit = {
    runWithQueueImpls(qs: _*)
  }

  def runWithEveryQueueImpl(f: QueueImpl => Unit): Unit = {
    runWithQueueImpls(allQueueImpls)(f)
  }

  def everyQueueExcept(qis: QueueImpl*): Seq[QueueImpl] = {
    allQueueImpls.filterNot(q => qis.contains(q))
  }
}
