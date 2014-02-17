package dk.itu.wsq

class Worker[T, R](val workerPool: WorkerPool[T, R], val task: Task[T, R]) extends Runnable {
  val queue = new WorkStealingQueue[T]()
  var work: Option[T] = None

  def run(): Unit = {
    while (workerPool.result.isEmpty) {
      work match {
        case Some(w) => {
          val (more, result) = task.run(w)

          result match {
            case Some(r) => workerPool.result = Some(r)
            case None    => {
              more.length match {
                case 0 => work = queue.take()
                case 1 => work = Some(more.head)
                case _ => {
                  work = Some(more.head)
                  more.tail.foreach(w => queue.push(w))
                } 
              }
            }
          }
        }
        case None => {
          work = workerPool.steal()
        } 
      }
    }
  }

  def steal(): Option[T] = {
    queue.steal()
  }
}
