package monocle.syntax

import monocle.applied._
import monocle._

object applied extends AppliedSyntax

trait AppliedSyntax {
  implicit class AppliedOps[A](value: A) {
    def optic: AppliedIso[A, A] =
      AppliedIso.id(value)

    def optic[B](lens: Lens[A, B]): AppliedLens[A, B] =
      AppliedLens(value, lens)

    def optic[E, B](prism: Prism[E, A, B]): AppliedPrism[E, A, B] =
      AppliedPrism(value, prism)

    def optic[E, B](optional: Optional[E, A, B]): AppliedOptional[E, A, B] =
      AppliedOptional(value, optional)

    def optic[B](getter: Getter[A, B]): AppliedGetter[A, B] =
      AppliedGetter(value, getter)

    def optic[B](fold: Fold[A, B]): AppliedFold[A, B] =
      AppliedFold(value, fold)

    def optic[B](setter: Setter[A, B]): AppliedSetter[A, B] =
      AppliedSetter(value, setter)
  }
}
