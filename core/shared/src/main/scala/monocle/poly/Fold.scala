package monocle.poly

trait Fold[-A, +B] { self =>
  def iterator(from: A): Iterator[B]

  def andThen[C](other: Fold[B, C]): Fold[A, C] =
    new Fold[A, C] {
      def iterator(from: A): Iterator[C] = self.iterator(from).flatMap(other.iterator)
    }
}

object Fold {
  def apply[A, B](_toIterator: A => Iterator[B]): Fold[A, B] =
    new Fold[A, B] {
      def iterator(from: A): Iterator[B] =
        _toIterator(from)
    }
}
