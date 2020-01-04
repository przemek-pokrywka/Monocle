package monocle.applied

import monocle.Prism

trait AppliedPrism[+E, A, B] extends AppliedOptional[E, A, B] {
  def value: A
  def optic: Prism[E, A, B]

  def compose[E1 >: E, C](other: Prism[E1, B, C]): AppliedPrism[E1, A, C] =
    AppliedPrism(value, optic.compose(other))
}

object AppliedPrism {
  def apply[E, A, B](_value: A, _optic: Prism[E, A, B]): AppliedPrism[E, A, B] =
    new AppliedPrism[E, A, B] {
      def value: A              = _value
      def optic: Prism[E, A, B] = _optic
    }
}
