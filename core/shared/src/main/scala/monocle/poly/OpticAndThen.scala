package monocle.poly

trait OpticAndThen[Optic[A, B, C, D]] {
  def andThen[A, B, C, D, E, F](left: Optic[A, B, C, D], right: Optic[C, D, E, F]): Optic[A, B, E, F]
}

object OpticAndThen {

  implicit val setter: OpticAndThen[PSetter] = new OpticAndThen[PSetter] {
    def andThen[A, B, C, D, E, F](left: PSetter[A, B, C, D], right: PSetter[C, D, E, F]): PSetter[A, B, E, F] =
      left.andThen(right)
  }

  implicit val optional: OpticAndThen[POptional] = new OpticAndThen[POptional] {
    def andThen[A, B, C, D, E, F](left: POptional[A, B, C, D], right: POptional[C, D, E, F]): POptional[A, B, E, F] =
      left.andThen(right)
  }

  implicit val lens: OpticAndThen[PLens] = new OpticAndThen[PLens] {
    def andThen[A, B, C, D, E, F](left: PLens[A, B, C, D], right: PLens[C, D, E, F]): PLens[A, B, E, F] =
      left.andThen(right)
  }

  implicit val prism: OpticAndThen[PPrism] = new OpticAndThen[PPrism] {
    def andThen[A, B, C, D, E, F](left: PPrism[A, B, C, D], right: PPrism[C, D, E, F]): PPrism[A, B, E, F] =
      left.andThen(right)
  }

  implicit val iso: OpticAndThen[PIso] = new OpticAndThen[PIso] {
    def andThen[A, B, C, D, E, F](left: PIso[A, B, C, D], right: PIso[C, D, E, F]): PIso[A, B, E, F] =
      left.andThen(right)
  }

  implicit final class AndThenSyntax[Optic[A, B, C, D], A, B, C, D](private val self: Optic[A, B, C, D]) {
    def >>>[Optic1[A, B, C, D] >: Optic[A, B, C, D], E, F](
      that: Optic1[C, D, E, F]
    )(implicit ev: OpticAndThen[Optic1]): Optic1[A, B, E, F] =
      ev.andThen(self, that)
  }
}
