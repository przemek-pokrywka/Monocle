package monocle.applied

import monocle.Iso

trait AppliedIso[A, B] extends AppliedLens[A, B] with AppliedPrism[Nothing, A, B] {
  def value: A
  def optic: Iso[A, B]

  def compose[C](other: Iso[B, C]): AppliedIso[A, C] =
    AppliedIso(value, optic.compose(other))
}

object AppliedIso {
  def apply[A, B](_value: A, _optic: Iso[A, B]): AppliedIso[A, B] =
    new AppliedIso[A, B] {
      def value: A         = _value
      def optic: Iso[A, B] = _optic
    }

  def id[A](value: A): AppliedIso[A, A] =
    apply(value, Iso.id)
}
