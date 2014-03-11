package dk.itu.wsq

trait Worker[N <: Node] extends Runnable {
  var currentNode: Option[N]
  def steal(): Option[N]
}
