package dk.itu.wsq.queue

abstract class QueueImplementation

trait WorkStealingQueue[E] {

	def push(e: E): Unit

	def take(): Option[E]

  def steal(): Option[E]

  def length: Int
}
