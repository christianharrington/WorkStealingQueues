package dk.itu.wsq.cases.xmlserialization

import dk.itu.wsq._
import dk.itu.wsq.queue._

class XMLSerializationWorker(val id:Int, val workerPool: XMLSerializationWorkerPool, 
                             private val queue: WorkStealingQueue[XMLNode]) 
  extends Worker[XMLNode] {

  var currentNode: Option[XMLNode] = None

  def run(): Unit = {
    while (workerPool.result.isEmpty) {
      currentNode match {
        case Some(node) => 
          execute(node) match {
            case Some(s) => workerPool.result = Some(s)
            case None    => currentNode = queue.take()
          }
        case None => currentNode = workerPool.steal(id)
      }
    }
  }

  def execute(node: XMLNode): Option[String] = {
    val serialize = node.serialize

    if (serialize.isEmpty) {
      node.divide foreach { child => enqueue(child) }

      None
    } else {
      node.parent match {
        case None => node.stringValue
        case Some(parent) => {
          node.notifyParent()
          enqueue(parent)

          None
        }
      }
    }
  }

  def steal(): Option[XMLNode] = queue.steal()

  def enqueue(node: XMLNode): Unit = queue.push(node)
}