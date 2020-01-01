package monocle.poly

object Lens {
  def apply[A, B](_get: A => B)(_set: B => A => A): Lens[A, B] =
    PLens(_get)(_set)
}
