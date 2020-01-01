package monocle.poly

trait PLens[-A1, +A2, +B1, -B2] extends Getter[A1, B1] with POptional[A1, A2, B1, B2] { self =>
  def getOrModify(from: A1): Either[A2, B1] = Right(get(from))

  override def iterator(from: A1): Iterator[B1] = Iterator.single(get(from))
  override def getOption(from: A1): Option[B1]  = Some(get(from))

  def andThen[C1, C2](other: PLens[B1, B2, C1, C2]): PLens[A1, A2, C1, C2] =
    new PLens[A1, A2, C1, C2] {
      def get(from: A1): C1             = other.get(self.get(from))
      def modify(f: C1 => C2): A1 => A2 = self.modify(other.modify(f))
    }
}

object PLens {
  def apply[A1, A2, B1, B2](_get: A1 => B1)(_set: B2 => A1 => A2): PLens[A1, A2, B1, B2] =
    new PLens[A1, A2, B1, B2] {
      def get(from: A1): B1              = _get(from)
      def modify(f: B1 => B2): A1 => A2  = from => set(f(get(from)))(from)
      override def set(to: B2): A1 => A2 = _set(to)
    }
}
