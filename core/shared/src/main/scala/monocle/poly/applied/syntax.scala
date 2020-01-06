package monocle.poly.applied

object syntax extends AppliedSyntax

trait AppliedSyntax {
  implicit class AppliedOps[A](value: A) {
    def optic[B]: AppliedPIso[A, B, A, B] =
      AppliedPIso.id[A, B](value)
  }
}
