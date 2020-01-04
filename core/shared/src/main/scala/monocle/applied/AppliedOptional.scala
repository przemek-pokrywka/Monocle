package monocle.applied

import monocle.Optional

trait AppliedOptional[+E, A, B] extends AppliedFold[A, B] with AppliedSetter[A, B] {
  def value: A
  def optic: Optional[E, A, B]

  def getOption: Option[B] =
    optic.getOption(value)

  def set(to: B): A =
    optic.set(to)(value)

  def modify(f: B => B): A =
    optic.modify(f)(value)

  def compose[E1 >: E, C](other: Optional[E1, B, C]): AppliedOptional[E1, A, C] =
    AppliedOptional(value, optic.compose(other))
}

object AppliedOptional {
  def apply[E, A, B](_value: A, _optic: Optional[E, A, B]): AppliedOptional[E, A, B] =
    new AppliedOptional[E, A, B] {
      def value: A                 = _value
      def optic: Optional[E, A, B] = _optic
    }
}
