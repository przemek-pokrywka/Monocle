package monocle.poly

object Iso {
  def apply[A, B](get: A => B)(reverseGet: B => A): Iso[A, B] =
    PIso(get)(reverseGet)

  def id[A]: Iso[A, A] = apply[A, A](identity)(identity)
}
