package monocle.poly

trait PSetter[-A1, +A2, +B1, -B2] { self =>
  def modify(f: B1 => B2): A1 => A2
  def set(to: B2): A1 => A2 = modify(_ => to)

  def andThen[C1, C2](other: PSetter[B1, B2, C1, C2]): PSetter[A1, A2, C1, C2] =
    new PSetter[A1, A2, C1, C2] {
      def modify(f: C1 => C2): A1 => A2 = self.modify(other.modify(f))
    }
}

object PSetter {
  def apply[A1, A2, B1, B2](_modify: (B1 => B2) => (A1 => A2)): PSetter[A1, A2, B1, B2] =
    new PSetter[A1, A2, B1, B2] {
      def modify(f: B1 => B2): A1 => A2 = _modify(f)
    }
}
