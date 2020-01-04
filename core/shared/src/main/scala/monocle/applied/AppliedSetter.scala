package monocle.applied

import monocle._

trait AppliedSetter[A, B] {
  def value: A
  def optic: Setter[A, B]

  def set: B => A =
    optic.set(_)(value)

  def compose[C](other: Setter[B, C]): AppliedSetter[A, C] =
    AppliedSetter(value, optic.compose(other))
}

object AppliedSetter {
  def apply[A, B](_value: A, _optic: Setter[A, B]): AppliedSetter[A, B] =
    new AppliedSetter[A, B] {
      def value: A            = _value
      def optic: Setter[A, B] = _optic
    }
}
