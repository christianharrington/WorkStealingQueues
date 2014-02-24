package dk.itu.wsq

class WorkUnit[T, R: Manifest](val index: Int, val parent: Option[WorkUnit[T, R]], val input: Seq[T]) {
  import java.util.concurrent.atomic._
  import scala.collection.mutable

  private var hasBeenRun = false
  
  var neededResults = 0
  private var completedResults = new AtomicInteger(0)
  var results = new Array[R](3)

  def addResult(i: Int, r: R): Boolean = {
  	//println("size: " + results.size)
    results(i) = r

    val completed = completedResults.incrementAndGet()
    completed == neededResults
  }
  
  def hasRun(): Unit = hasBeenRun = true

  def readyToComplete: Boolean = {
    (completedResults.get == neededResults) && hasBeenRun
  }
}
