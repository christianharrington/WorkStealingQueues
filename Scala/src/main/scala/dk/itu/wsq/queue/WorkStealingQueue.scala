package dk.itu.wsq.queue

trait WorkStealingQueue[E] {

	def push(e: E): Unit

	def take(): Option[E]

  def steal(): Option[E]

  def length: Int
}