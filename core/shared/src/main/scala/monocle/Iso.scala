package monocle

import monocle.function._

abstract class Iso[A, B] extends Lens[A, B] with Prism[Nothing, A, B] { self =>
  override def modify(f: B => B): A => A =
    from => reverseGet(get(from))

  def compose[C](other: Iso[B, C]): Iso[A, C] = new Iso[A, C] {
    def get(from: A): C      = other.get(self.get(from))
    def reverseGet(to: C): A = self.reverseGet(other.reverseGet(to))
  }
}

object Iso {
  def apply[A, B](_get: A => B)(_reverseGet: B => A): Iso[A, B] =
    new Iso[A, B] {
      def get(from: A): B      = _get(from)
      def reverseGet(to: B): A = _reverseGet(to)
    }

  def reverse[A, B](implicit ev: Reverse.Aux[A, B]): Iso[A, B] =
    ev.reverse

  def id[A]: Iso[A, A] =
    Iso[A, A](identity)(identity)
}
