package dk.itu.wsq

object Benchmark extends App {
  import dk.itu.wsq._
  import dk.itu.wsq.cases._
  import java.util.Random
  import scala.collection.mutable.ArrayBuffer

  def time[A](f: => A) = {
    val s = System.nanoTime
    val ret = f
    val t = (System.nanoTime - s)/1e6

    println(s"Time: $t ms")
    ret
  }

  val l = 100000

  println(s"Generating ArrayBuffer with $l number from 0 to $l ...")
  val random    = new Random()
  val testArray = ArrayBuffer.fill(l)(random.nextInt(l))
  val testArrayBuiltIn = new ArrayBuffer[Int](l)
  testArray.copyToBuffer(testArrayBuiltIn)
  println("Done")

  val wp = new WorkerPool(4)
  
  println("Starting...")
  time(wp.run(testArray))
  println("Done")

  println("Starting...")
  time(testArrayBuiltIn.sorted)
  println("Done")
}
