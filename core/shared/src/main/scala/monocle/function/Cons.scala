package monocle.function

import monocle.{Lens, UOptional, UPrism}

trait Cons[A] {
  type B

  def cons: UPrism[A, (B, A)]

  def headOption: UOptional[A, B] = cons composeLens Lens.first
  def tailOption: UOptional[A, A] = cons composeLens Lens.second
}

object Cons {
  type Aux[A, B0] = Cons[A] { type B = B0 }

  def apply[A, B0](prism: UPrism[A, (B0, A)]): Aux[A, B0] =
    new Cons[A] {
      type B = B0
      def cons: UPrism[A, (B0, A)] = prism
    }

  implicit def list[A]: Cons.Aux[List[A], A] =
    apply(UPrism[List[A], (A, List[A])] {
      case Nil     => None
      case x :: xs => Some((x, xs))
    } { case (x, xs) => x :: xs })

  implicit def vector[A]: Cons.Aux[Vector[A], A] =
    apply(UPrism[Vector[A], (A, Vector[A])](xs => xs.headOption.map(_ -> xs.tail)) {
      case (x, xs) => x +: xs
    })
}
