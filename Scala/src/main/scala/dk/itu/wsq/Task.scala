package dk.itu.wsq

trait Task[T, R] {
  def run(in: WorkUnit[T, R]): (Either[Seq[WorkUnit[T, R]], R])

  def complete(in: WorkUnit[T, R]): R
}
