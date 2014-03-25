package dk.itu.wsq.cases.xmlserialization

import dk.itu.wsq._
import dk.itu.wsq.queue._

object XMLSerializationBenchmark {
  import com.typesafe.config._

  def apply(workers: Int, seed: Long, conf: Config): Benchmark = {
    val depth = conf.getInt("benchmarks.xml.depth")
    val children = conf.getInt("benchmarks.xml.children")
    val attributes = conf.getInt("benchmarks.xml.attributes")

    XMLSerializationBenchmark(workers, depth, children, attributes, seed)
  }
}

case class XMLSerializationBenchmark(workers: Int, depth: Int, children: Int, attributes: Int, seed: Long)
  extends Benchmark
  with QueueHelper {

  private val in = XMLGenerator(seed, depth, children, attributes)

  def name = s"XML with $workers workers and depth $depth, children $children, and attributes $attributes"

  def run(queueImpl: QueueImpl): Double = {

    val wp = new XMLSerializationWorkerPool(workers, queueImpl)

    val (t, _) = time(wp.run(new XMLNode(in, None)))
    
    t
  }

  def worksWith: Seq[QueueImpl] = everyQueueExcept(idempotentQueueImpls: _*)
}
