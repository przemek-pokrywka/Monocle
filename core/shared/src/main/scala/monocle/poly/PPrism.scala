package monocle.poly

trait PPrism[-A1, +A2, +B1, -B2] extends POptional[A1, A2, B1, B2] { self =>
  def reverseGet(to: B2): A2

  def modify(f: B1 => B2): A1 => A2 = getOrModify(_).fold(identity, f andThen reverseGet)

  override def set(to: B2): A1 => A2 = _ => reverseGet(to)

  def andThen[C1, C2](other: PPrism[B1, B2, C1, C2]): PPrism[A1, A2, C1, C2] =
    new PPrism[A1, A2, C1, C2] {
      def reverseGet(to: C2): A2 = self.reverseGet(other.reverseGet(to))
      def getOrModify(from: A1): Either[A2, C1] =
        self.getOrModify(from).flatMap(other.getOrModify(_).left.map(self.set(_)(from)))
    }
}

object PPrism {
  def apply[A1, A2, B1, B2](_getOrModify: A1 => Either[A2, B1])(_reverseGet: B2 => A2): PPrism[A1, A2, B1, B2] =
    new PPrism[A1, A2, B1, B2] {
      def getOrModify(from: A1): Either[A2, B1] = _getOrModify(from)
      def reverseGet(to: B2): A2                = _reverseGet(to)
    }

  def some[A, B]: PPrism[Option[A], Option[B], A, B] =
    PPrism[Option[A], Option[B], A, B](_.toRight(None))(Some(_))
}
