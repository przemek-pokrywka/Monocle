package monocle.poly

object Prism {
  def apply[A, B](_getOption: A => Option[B])(_reverseGet: B => A): Prism[A, B] =
    new Prism[A, B] {
      def getOrModify(from: A): Either[A, B] = _getOption(from).toRight(from)
      def reverseGet(to: B): A               = _reverseGet(to)
    }

  def some[A]: Prism[Option[A], A] = PPrism.some
}
