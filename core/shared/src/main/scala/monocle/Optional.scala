package monocle

import monocle.function._

trait Optional[+E, A, B] extends Fold[A, B] with Setter[A, B] { self =>
  def getEither(from: A): Either[E, B]

  def getOption(from: A): Option[B] = getEither(from).toOption

  def modify(f: B => B): A => A =
    from => modifyE(f)(from).getOrElse(from)

  def modifyE(f: B => B): A => Either[E, A] =
    from => getEither(from).map(to => set(f(to))(from))

  def setE(to: B): A => Either[E, A] = modifyE(_ => to)

  def mapError[E1](f: E => E1): Optional[E1, A, B] =
    new Optional[E1, A, B] {
      def getEither(from: A): Either[E1, B] = self.getEither(from).left.map(f)
      override def set(to: B): A => A       = self.set(to)
    }

  def withError[E1](error: E1): Optional[E1, A, B] =
    mapError(_ => error)

  override def toIterator(from: A): Iterator[B] =
    getOption(from).iterator

  final def compose[E1 >: E, C](other: Optional[E1, B, C]): Optional[E1, A, C] = new Optional[E1, A, C] {
    def getEither(from: A): Either[E1, C]  = self.getEither(from).flatMap(other.getEither)
    override def set(to: C): A => A        = self.modify(other.set(to))
    override def modify(f: C => C): A => A = self.modify(other.modify(f))
  }
}

object Optional {
  def apply[E, A, B](_getEither: A => Either[E, B])(_set: (A, B) => A): Optional[E, A, B] = new Optional[E, A, B] {
    def getEither(from: A): Either[E, B] = _getEither(from)
    override def set(to: B): A => A      = _set(_, to)
  }
}

object UOptional {
  def apply[A, B](_getEither: A => Option[B])(_set: (A, B) => A): UOptional[A, B] = new UOptional[A, B] {
    def getEither(from: A): Either[Unit, B]    = _getEither(from).toRight(())
    override def getOption(from: A): Option[B] = _getEither(from)
    override def set(to: B): A => A            = _set(_, to)
  }

  def void[S, A]: UOptional[S, A]                                                = apply[S, A](_ => None)((a, _) => a)
  def headOption[S, A](implicit ev: Cons.Aux[S, A]): UOptional[S, A]             = ev.headOption
  def index[S, I, A](index: I)(implicit ev: Index.Aux[S, I, A]): UOptional[S, A] = ev.index(index)
  def tailOption[S](implicit ev: Cons[S]): UOptional[S, S]                       = ev.tailOption
  def possible[A, B](implicit ev: Possible.Aux[A, B]): UOptional[A, B]           = ev.possible
}
