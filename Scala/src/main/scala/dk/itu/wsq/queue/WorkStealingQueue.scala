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
object LifoIWSQueueImpl extends QueueImplementation {
  override def toString(): String = "Idempotent Work Stealing Queue (LIFO)"
}

object AllQueueImpls {
  def apply(): Seq[QueueImplementation] = Seq(
    ABPQueueImpl,
    ChaseLevQueueImpl,
    ChaseLevNaiveShrinkingQueueImpl,
    LifoIWSQueueImpl)
}

trait WorkStealingQueue[E] {
	def push(e: E): Unit

	def take(): Option[E]

  def steal(): Option[E]

  def length: Int
}

trait QueueHelper {
  def queueImplToQueue[E: Manifest](qi: QueueImplementation): WorkStealingQueue[E] = qi match {
    case ABPQueueImpl                    => new ABPQueue[E]()
    case ChaseLevQueueImpl               => new ChaseLevQueue[E]()
    case ChaseLevNaiveShrinkingQueueImpl => new ChaseLevNaiveShrinkingQueue[E]()
    case LifoIWSQueueImpl                => new LifoIWSQueue[E]()
  }

  def runWithQueues[E: Manifest](qs: QueueImplementation*)(f: WorkStealingQueue[E] => Unit) : Unit = {
    val queues = for (q <- qs) yield {
      queueImplToQueue(q)
    }

    queues foreach (q => f(q))
  }

  def runWithEveryQueue[E: Manifest](f: WorkStealingQueue[E] => Unit): Unit = {
    runWithQueues(AllQueueImpls(): _*)(f)
  }

  def runWithQueueImpls[E: Manifest](qs: QueueImplementation*)(f: QueueImplementation => Unit) : Unit = {
    qs foreach (q => f(q))
  }

  def runWithEveryQueueImpl(f: QueueImplementation => Unit): Unit = {
    runWithQueueImpls(AllQueueImpls(): _*)(f)
  }

  def everyQueueExcept(qis: QueueImplementation*): Seq[QueueImplementation] = {
    AllQueueImpls().filterNot(q => qis.contains(q))
  }
}
