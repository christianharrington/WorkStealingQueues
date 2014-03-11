package dk.itu.wsq

import scala.util.Random

trait WorkerPool[N <: Node, W <: Worker[N], R] {
  import dk.itu.wsq.cases.quicksort._

  def workers: List[W]

  def threads: List[Thread]

  @volatile var result: Option[R] = None 

  def run(initial: N): Option[R] = {
    workers.head.currentNode = Some(initial)
    threads.foreach(t => t.start())
    threads.foreach(t => t.join())

    result
  }

  def steal(id: Int): Option[N] = {
    workers(Random.nextInt(workers.length)).steal()
  }
}
