package monocle

import monocle.function._

trait Prism[+E, A, B] extends Optional[E, A, B] { self =>
  def reverseGet(to: B): A

  override def modifyE(f: B => B): A => Either[E, A] =
    from => getEither(from).map(f).map(reverseGet)

  override def modify(f: B => B): A => A = a => getOption(a).fold(a)(reverseGet)

  override def set(to: B): A => A = _ => reverseGet(to)

  override def asTarget[C](implicit ev: B =:= C): Prism[E, A, C] =
    asInstanceOf[Prism[E, A, C]]

  def compose[E1 >: E, C](other: Prism[E1, B, C]): Prism[E1, A, C] = new Prism[E1, A, C] {
    def getEither(from: A): Either[E1, C]      = self.getEither(from).flatMap(other.getEither)
    def reverseGet(to: C): A                   = self.reverseGet(other.reverseGet(to))
    override def getOption(from: A): Option[C] = self.getOption(from).flatMap(other.getOption)
  }

  def mapError[E1](f: E => E1): Prism[E1, A, B] =
    new Prism[E1, A, B] {
      def getEither(from: A): Either[E1, B] = self.getEither(from).left.map(f)
      def reverseGet(to: B): A              = self.reverseGet(to)
    }

  def withError[E1](error: E1): Prism[E1, A, B] =
    mapError(_ => error)

  ///////////////////////////////////
  // dot syntax for optics typeclass
  ///////////////////////////////////

  override def cons(implicit ev: Cons[B]): Prism[E, A, (ev.B, B)]  = compose(ev.cons)
  override def reverse(implicit ev: Reverse[B]): Prism[E, A, ev.B] = compose(ev.reverse)

  ///////////////////////////////////
  // dot syntax for standard types
  ///////////////////////////////////

  override def left[X, C](implicit ev: B =:= Either[E, C]): Prism[E, A, E] =
    asTarget[Either[X, C]].composePrism(Prism.left)
  override def right[X, C](implicit ev: B =:= Either[E, C]): Prism[E, A, C] =
    asTarget[Either[X, C]].composePrism(Prism.right)
  override def some[C](implicit ev: B =:= Option[C]): Prism[E, A, C] =
    asTarget[Option[C]].composePrism(Prism.some)

}

object Prism {
  def apply[E, A, B](_getEither: A => Either[E, B])(_reverseGet: B => A): Prism[E, A, B] = new Prism[E, A, B] {
    def getEither(from: A): Either[E, B] = _getEither(from)
    def reverseGet(to: B): A             = _reverseGet(to)
  }

  def some[E, A](error: E): Prism[E, Option[A], A] =
    UPrism.partial[Option[A], A] { case Some(a) => a }(Some(_)).withError(error)
}

object UPrism {
  def apply[A, B](_getOption: A => Option[B])(_reverseGet: B => A): UPrism[A, B] = new UPrism[A, B] {
    def getEither(from: A): Either[Unit, B]    = _getOption(from).toRight(())
    def reverseGet(to: B): A                   = _reverseGet(to)
    override def getOption(from: A): Option[B] = _getOption(from)
  }

  def partial[A, B](get: PartialFunction[A, B])(reverseGet: B => A): UPrism[A, B] =
    apply(get.lift)(reverseGet)

  def cons[S, A](implicit ev: Cons.Aux[S, A]): UPrism[S, (A, S)] =
    ev.cons

  def some[A]: UPrism[Option[A], A] =
    UPrism.partial[Option[A], A] { case Some(a) => a }(Some(_))

  def none[A]: UPrism[Option[A], Unit] =
    partial[Option[A], Unit] { case None => () }(_ => None)

  def left[E, A]: UPrism[Either[E, A], E] =
    partial[Either[E, A], E] { case Left(e) => e }(Left(_))

  def right[E, A]: UPrism[Either[E, A], A] =
    partial[Either[E, A], A] { case Right(e) => e }(Right(_))
}
