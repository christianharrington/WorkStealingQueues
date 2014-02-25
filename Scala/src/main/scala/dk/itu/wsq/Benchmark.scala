/*package dk.itu.wsq

object Benchmark extends App {
  import dk.itu.wsq._
  import dk.itu.wsq.cases.QuickSort 
  import java.util.Random
  import scala.collection.mutable.ArrayBuffer

  System.in.read()

  def time[A](f: => A) = {
    val s = System.nanoTime
    var ret = f
    val t = (System.nanoTime - s)/1e6

    println(s"Time: $t ms")
    ret
  }

  val l = 100000

  println(s"Generating ArrayBuffer with $l number from 0 to $l ...")
  val random    = new Random()
  val testArray = ArrayBuffer.fill(l)(random.nextInt(l))
  println("Done")

  val wp   = new WorkerPool(QuickSort, 4)
  val initial = new WorkUnit[ArrayBuffer[Int], ArrayBuffer[Int]](0, None, Seq(testArray))
  
  println("Starting...")
  time(wp.run(Some(initial)))
  println("Done")

  /*val testArrayBuiltIn = ArrayBuffer.fill(l)(random.nextInt(l))
  println("Starting...")
  time(testArrayBuiltIn.sorted)
  println("Done")*/
}
*/