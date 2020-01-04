package monocle.function

import monocle.{UOptional, UPrism}

trait Possible[A] {
  type B

  def possible: UOptional[A, B]
}

object Possible {
  type Aux[A, B0] = Possible[A] { type B = B0 }

  def apply[A, B0](optional: UOptional[A, B0]): Aux[A, B0] = new Possible[A] {
    type B = B0
    override val possible: UOptional[A, B0] = optional
  }

  implicit def optionPossible[A]: Aux[Option[A], A] =
    apply(UPrism.some)
}
