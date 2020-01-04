package monocle

trait Setter[A, B] { self =>
  def modify(f: B => B): A => A

  def set(to: B): A => A = modify(_ => to)

  def compose[C](other: Setter[B, C]): Setter[A, C] =
    new Setter[A, C] {
      def modify(f: C => C): A => A = self.modify(other.modify(f))
    }

  def composePrism[E, C](other: Prism[E, B, C]): Setter[A, C] = compose(other)
}

object Setter {
  def apply[A, B](_modify: (B => B) => (A => A)): Setter[A, B] = new Setter[A, B] {
    def modify(f: B => B): A => A = _modify(f)
  }
}
