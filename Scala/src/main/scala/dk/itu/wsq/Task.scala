package dk.itu.wsq

trait Task[T, R] {
  def run(in: T): (Seq[T], Option[R])
}
