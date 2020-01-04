package monocle.applied

import monocle.Lens

trait AppliedLens[A, B] extends AppliedOptional[Nothing, A, B] with AppliedGetter[A, B] {
  def value: A
  def optic: Lens[A, B]

  def compose[C](other: Lens[B, C]): AppliedLens[A, C] =
    AppliedLens(value, optic.compose(other))
}

object AppliedLens {
  def apply[A, B](_value: A, _optic: Lens[A, B]): AppliedLens[A, B] =
    new AppliedLens[A, B] {
      def value: A          = _value
      def optic: Lens[A, B] = _optic
    }
}
