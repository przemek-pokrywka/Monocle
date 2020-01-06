package monocle.poly.applied

import monocle.poly.{PIso, PPrism}

trait AppliedPPrism[A1, A2, B1, B2] {
  def value: A1

  def optic: PPrism[A1, A2, B1, B2]

  def getOption: Option[B1] = optic.getOption(value)
  def set(to: B2): A2       = optic.set(to)(value)

  def andThen[C1, C2](other: PPrism[B1, B2, C1, C2]): AppliedPPrism[A1, A2, C1, C2] =
    AppliedPPrism(value, optic.andThen(other))

  def andThenIso[C1, C2](other: PIso[B1, B2, C1, C2]): AppliedPPrism[A1, A2, C1, C2] =
    AppliedPPrism(value, optic.andThen(other))
}

object AppliedPPrism {
  def apply[A1, A2, B1, B2](_value: A1, _optic: PPrism[A1, A2, B1, B2]): AppliedPPrism[A1, A2, B1, B2] =
    new AppliedPPrism[A1, A2, B1, B2] {
      def value: A1                     = _value
      def optic: PPrism[A1, A2, B1, B2] = _optic
    }
}
