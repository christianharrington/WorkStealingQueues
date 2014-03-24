package dk.itu.wsq.queue

sealed abstract class QueueImplementation
object ABPQueueImpl extends QueueImplementation {
  override def toString(): String = "ABP Queue"
}
object ChaseLevQueueImpl extends QueueImplementation {
  override def toString(): String =  "Chase-Lev Queue" 
}
object ChaseLevNaiveShrinkingQueueImpl extends QueueImplementation {
  override def toString(): String = "Chase-Lev Naive Shrinking Queue"
}
object IdempotentLIFOImpl extends QueueImplementation {
  override def toString(): String = "Idempotent Work Stealing Queue (LIFO)"
}
object IdempotentFIFOImpl extends QueueImplementation {
  override def toString(): String = "Idempotent Work Stealing Queue (FIFO)"
}
object IdempotentDEImpl extends QueueImplementation {
  override def toString(): String = "Idempotent Work Stealing Queue (Double-Ended)"
}
object DuplicatingQueueImpl extends QueueImplementation {
  override def toString(): String = "Duplicating Queue"
}

object AllQueueImpls {
  def apply(): Seq[QueueImplementation] = Seq(
    ABPQueueImpl,
    ChaseLevQueueImpl,
    ChaseLevNaiveShrinkingQueueImpl,
    IdempotentLIFOImpl,
    IdempotentFIFOImpl,
    IdempotentDEImpl,
    DuplicatingQueueImpl)
}

trait WorkStealingQueue[E] {
	def push(e: E): Unit

	def take(): Option[E]

  def steal(): Option[E]

  def length: Int
}

trait QueueHelper {
  def queueImplToQueue[E: Manifest](q: QueueImplementation): WorkStealingQueue[E] = {
    q match {
      case ABPQueueImpl                    => new ABPQueue[E](512)
      case ChaseLevQueueImpl               => new ChaseLevQueue[E]()
      case ChaseLevNaiveShrinkingQueueImpl => new ChaseLevNaiveShrinkingQueue[E]()
      case IdempotentLIFOImpl              => new IdempotentLIFO[E]()
      case IdempotentFIFOImpl              => new IdempotentFIFO[E]()
      case IdempotentDEImpl                => new IdempotentDE[E]()
      case DuplicatingQueueImpl            => new DuplicatingQueue[E](1000000)
    }
  }

  def runWithQueues[E: Manifest]
    (qs: Seq[QueueImplementation])
    (f: WorkStealingQueue[E] => Unit): Unit = {
    val queues = for (q <- qs) yield {
      queueImplToQueue(q)
    }

    queues foreach (q => f(q))
  }

  def runWithQueues[E: Manifest]
    (qs: QueueImplementation*): WorkStealingQueue[E] => Unit = {
    runWithQueues(qs: _*)
  }

  def runWithEveryQueue[E: Manifest](f: WorkStealingQueue[E] => Unit): Unit = {
    runWithQueues(AllQueueImpls())(f)
  }

    def runWithQueueImpls[E: Manifest]
    (qs: Seq[QueueImplementation])
    (f: QueueImplementation => Unit) : Unit = {
    qs foreach (q => f(q))
  }

  def runWithQueueImpls[E: Manifest]
    (qs: QueueImplementation*): QueueImplementation => Unit = {
    runWithQueueImpls(qs: _*)
  }

  def runWithEveryQueueImpl(f: QueueImplementation => Unit): Unit = {
    runWithQueueImpls(AllQueueImpls())(f)
  }

  def everyQueueExcept(qis: QueueImplementation*): Seq[QueueImplementation] = {
    AllQueueImpls().filterNot(q => qis.contains(q))
  }
}
