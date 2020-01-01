package monocle.poly

trait Getter[-A, +B] extends Fold[A, B] {
  def get(from: A): B

  def iterator(from: A): Iterator[B] = Iterator.single(get(from))
}

object Getter {
  def apply[A, B](_get: A => B): Getter[A, B] = new Getter[A, B] {
    def get(from: A): B = _get(from)
  }
}
