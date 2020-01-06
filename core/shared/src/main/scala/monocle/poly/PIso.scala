package monocle.poly

trait PIso[-A1, +A2, +B1, -B2] extends PLens[A1, A2, B1, B2] with PPrism[A1, A2, B1, B2] { self =>
  override def iterator(from: A1): Iterator[B1]      = Iterator.single(get(from))
  override def getOrModify(from: A1): Either[A2, B1] = Right(get(from))
  override def getOption(from: A1): Option[B1]       = Some(get(from))
  override def set(to: B2): A1 => A2                 = _ => reverseGet(to)
  override def modify(f: B1 => B2): A1 => A2         = getOrModify(_).fold(identity, f andThen reverseGet)

  def andThen[C1, C2](other: PIso[B1, B2, C1, C2]): PIso[A1, A2, C1, C2] =
    new PIso[A1, A2, C1, C2] {
      def get(from: A1): C1      = other.get(self.get(from))
      def reverseGet(to: C2): A2 = self.reverseGet(other.reverseGet(to))
    }
}

object PIso {
  def apply[A1, A2, B1, B2](_get: A1 => B1)(_reverseGet: B2 => A2): PIso[A1, A2, B1, B2] =
    new PIso[A1, A2, B1, B2] {
      def get(from: A1): B1      = _get(from)
      def reverseGet(to: B2): A2 = _reverseGet(to)
    }

  def id[A, B]: PIso[A, B, A, B] = apply[A, B, A, B](identity)(identity)
}
