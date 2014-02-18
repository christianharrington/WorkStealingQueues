package dk.itu.wsq

class Worker[T, R](val workerPool: WorkerPool[T, R], val task: Task[T, R]) extends Runnable {
  private val queue = new WorkStealingQueue[WorkUnit[T, R]]()
  private var currentWorkUnit: Option[WorkUnit[T, R]] = None
  
  def setNextWorkUnit(work: WorkUnit[T, R])         = currentWorkUnit = Some(work)
  def setNextWorkUnit(work: Option[WorkUnit[T, R]]) = currentWorkUnit = work

  def run(): Unit = {
    println("Starting thread...")

    while (workerPool.result.isEmpty) {
      currentWorkUnit match {
        case Some(work) => {
          val workOrResult = 
            if (work.readyToComplete) {
              //println("This work unit is ready to complete")
              Right(task.complete(work))
            } else {
              task.run(work)
            }

          workOrResult match {
            case Right(r) => {
              work.parent match {
                case None => {
                  //println("We have a result, and no parents")
                  workerPool.result = Some(r) // We are done!
                }
                case Some(parent) => {
                  //println(s"Index: ${work.index}")
                  val parentIsReady = parent.addResult(work.index, r)
                  
                  if (parentIsReady) {
                    //println("Working on parent")
                    setNextWorkUnit(parent) // Yes. Lets finish it
                  }
                  else {
                    //println("Got a result, but parent isn't ready")
                    setNextWorkUnit(queue.take()) // Nope. Back to work
                  }
                }
              }
            }
            case Left(w) => {
              //println("No results, more work")
              w.length match {
                case 0 => {
                  //println("No new work, taking from queue")
                  setNextWorkUnit(queue.take())
                }
                case 1 => {
                  //println(s"One new work, working on that. Queue length: ${queue.length}")
                  setNextWorkUnit(w.head)
                }
                case _ => {
                  //println(s"More new work. Queue length: ${queue.length}")
                  setNextWorkUnit(w.head)
                  w.tail.foreach(w => queue.push(w))
                } 
              }
            }
          }
        }
        case None => {
          setNextWorkUnit(workerPool.steal())
        } 
      }
    }
  }

  def steal(): Option[WorkUnit[T, R]] = {
    queue.steal()
  }
}
