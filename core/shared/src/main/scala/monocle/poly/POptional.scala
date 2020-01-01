package monocle.poly

import monocle.function.Field1

trait POptional[-A1, +A2, +B1, -B2] extends Fold[A1, B1] with PSetter[A1, A2, B1, B2] { self =>
  def getOrModify(from: A1): Either[A2, B1]
  def getOption(from: A1): Option[B1]  = getOrModify(from).toOption
  def iterator(from: A1): Iterator[B1] = getOrModify(from).fold(_ => Iterator.empty, Iterator.single)

  def andThen[C1, C2](other: POptional[B1, B2, C1, C2]): POptional[A1, A2, C1, C2] =
    new POptional[A1, A2, C1, C2] {
      def getOrModify(from: A1): Either[A2, C1] =
        self.getOrModify(from).flatMap(other.getOrModify(_).left.map(self.set(_)(from)))
      def modify(f: C1 => C2): A1 => A2 = self.modify(other.modify(f))
    }

  def andThenPrism[C1, C2](other: PPrism[B1, B2, C1, C2]): POptional[A1, A2, C1, C2] =
    andThen(other)

  ///////////////////////////////////
  // dot syntax for standard types
  ///////////////////////////////////

  def some[C1, C2](implicit ev1: B1 <:< Option[C1], ev2: Option[C2] <:< B2): POptional[A1, A2, C1, C2] =
    this.asInstanceOf[POptional[A1, A2, Option[C1], Option[C2]]].andThenPrism(PPrism.some)
}

object POptional {
  def apply[A1, A2, B1, B2](_getOrModify: A1 => Either[A2, B1])(_set: B2 => A1 => A2): POptional[A1, A2, B1, B2] =
    new POptional[A1, A2, B1, B2] {
      def getOrModify(from: A1): Either[A2, B1] = _getOrModify(from)
      def modify(f: B1 => B2): A1 => A2         = from => getOrModify(from).fold(identity, b1 => set(f(b1))(from))
      override def set(to: B2): A1 => A2        = _set(to)
    }

  implicit class POptionalOptionOps[A1, A2, B1, B2](optic: POptional[A1, A2, Option[B1], Option[B2]]) {
    def some2: POptional[A1, A2, B1, B2] =
      optic.andThenPrism(PPrism.some)
  }

  implicit class MonoOptionalOps[A, B](optic: Optional[A, B]) {
    def _1(implicit ev: Field1[B]): Optional[A, ev.B]    = first(ev)
    def first(implicit ev: Field1[B]): Optional[A, ev.B] = optic.andThen(ev.first.toPoly)
  }
}
