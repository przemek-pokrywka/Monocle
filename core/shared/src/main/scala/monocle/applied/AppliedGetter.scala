package monocle.applied

import monocle.Getter

trait AppliedGetter[A, B] extends AppliedFold[A, B] {
  def value: A
  def optic: Getter[A, B]

  def get: B =
    optic.get(value)

  def compose[C](other: Getter[B, C]): AppliedGetter[A, C] =
    AppliedGetter(value, optic.compose(other))
}

object AppliedGetter {
  def apply[A, B](_value: A, _optic: Getter[A, B]): AppliedGetter[A, B] =
    new AppliedGetter[A, B] {
      def value: A            = _value
      def optic: Getter[A, B] = _optic
    }
}
