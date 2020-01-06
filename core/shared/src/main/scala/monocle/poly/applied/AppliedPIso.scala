package monocle.poly.applied

import monocle.poly.{PIso, PPrism}

trait AppliedPIso[A1, A2, B1, B2] extends AppliedPPrism[A1, A2, B1, B2] {
  override def optic: PIso[A1, A2, B1, B2]

  def get: B1 = optic.get(value)

  def andThen[C1, C2](other: PIso[B1, B2, C1, C2]): AppliedPIso[A1, A2, C1, C2] =
    AppliedPIso(value, optic.andThen(other))

  def andThenPrism[C1, C2](other: PPrism[B1, B2, C1, C2]): AppliedPPrism[A1, A2, C1, C2] =
    AppliedPPrism(value, optic.andThen(other))
}

object AppliedPIso {
  def apply[A1, A2, B1, B2](_value: A1, _optic: PIso[A1, A2, B1, B2]): AppliedPIso[A1, A2, B1, B2] =
    new AppliedPIso[A1, A2, B1, B2] {
      def value: A1                   = _value
      def optic: PIso[A1, A2, B1, B2] = _optic
    }

  def id[A, B](value: A): AppliedPIso[A, B, A, B] =
    apply(value, PIso.id[A, B])
}
