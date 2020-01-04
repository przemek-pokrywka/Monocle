package monocle

import monocle.function._

trait Optional[+E, A, B] extends Fold[A, B] with Setter[A, B] { self =>
  def getEither(from: A): Either[E, B]

  def getOption(from: A): Option[B] = getEither(from).toOption

  def modifyE(f: B => B): A => Either[E, A] =
    from => getEither(from).map(to => set(f(to))(from))

  override def toIterator(from: A): Iterator[B] =
    getOption(from).iterator

  override def asTarget[C](implicit ev: B =:= C): Optional[E, A, C] =
    asInstanceOf[Optional[E, A, C]]

  final def compose[E1 >: E, C](other: Optional[E1, B, C]): Optional[E1, A, C] = new Optional[E1, A, C] {
    def getEither(from: A): Either[E1, C] =
      self.getEither(from).flatMap(other.getEither)
    override def set(to: C): A => A        = self.modify(other.set(to))
    override def modify(f: C => C): A => A = self.modify(other.modify(f))
  }

  def composeLens[C](other: Lens[B, C]): Optional[E, A, C]                          = compose(other)
  override def composePrism[E1 >: E, C](other: Prism[E1, B, C]): Optional[E1, A, C] = compose(other)

  ///////////////////////////////////
  // dot syntax for optics typeclass
  ///////////////////////////////////

  override def _1(implicit ev: Field1[B]): Optional[E, A, ev.B] = first(ev)
  override def _2(implicit ev: Field2[B]): Optional[E, A, ev.B] = second(ev)
  override def _3(implicit ev: Field3[B]): Optional[E, A, ev.B] = third(ev)
  override def _4(implicit ev: Field4[B]): Optional[E, A, ev.B] = fourth(ev)
  override def _5(implicit ev: Field5[B]): Optional[E, A, ev.B] = fifth(ev)
  override def _6(implicit ev: Field6[B]): Optional[E, A, ev.B] = sixth(ev)

  override def first(implicit ev: Field1[B]): Optional[E, A, ev.B]  = compose(ev.first)
  override def second(implicit ev: Field2[B]): Optional[E, A, ev.B] = compose(ev.second)
  override def third(implicit ev: Field3[B]): Optional[E, A, ev.B]  = compose(ev.third)
  override def fourth(implicit ev: Field4[B]): Optional[E, A, ev.B] = compose(ev.fourth)
  override def fifth(implicit ev: Field5[B]): Optional[E, A, ev.B]  = compose(ev.fifth)
  override def sixth(implicit ev: Field6[B]): Optional[E, A, ev.B]  = compose(ev.sixth)

  override def at[I, C](i: I)(implicit ev: At.Aux[B, I, C]): Optional[E, A, Option[C]] = compose(ev.at(i))
  override def cons(implicit ev: Cons[B]): Optional[E, A, (ev.B, B)]                   = compose(ev.cons)
  override def headOption(implicit ev: Cons[B]): Optional[E, A, ev.B]                  = compose(ev.headOption)
  override def tailOption(implicit ev: Cons[B]): Optional[E, A, B]                     = compose(ev.tailOption)
  override def index[I, C](i: I)(implicit ev: Index.Aux[B, I, C]): Optional[E, A, C]   = compose(ev.index(i))
  override def possible(implicit ev: Possible[B]): Optional[E, A, ev.B]                = compose(ev.possible)
  override def reverse(implicit ev: Reverse[B]): Optional[E, A, ev.B]                  = compose(ev.reverse)

  ///////////////////////////////////
  // dot syntax for standard types
  ///////////////////////////////////

  override def left[X, C](implicit ev: B =:= Either[E, C]): Optional[E, A, E] =
    asTarget[Either[X, C]].composePrism(Prism.left)
  override def right[X, C](implicit ev: B =:= Either[E, C]): Optional[E, A, C] =
    asTarget[Either[X, C]].composePrism(Prism.right)
  override def some[C](implicit ev: B =:= Option[C]): Optional[E, A, C] =
    asTarget[Option[C]].composePrism(Prism.some)
}

object Optional {
  def apply[A, B](_getOption: A => Option[B])(_set: (A, B) => A): Optional[Unit, A, B] = new Optional[Unit, A, B] {
    def getEither(from: A): Either[Nothing, B] = _getOption(from).toRight(())
    override def getOption(from: A): Option[B] = _getOption(from)
    override def set(to: B): A => A            = _set(_, to)
  }

  def void[S, A]: Optional[Unit, S, A] =
    Optional[S, A](_ => None)((a, _) => a)

  def headOption[S, A](implicit ev: Cons.Aux[S, A]): Optional[Unit, S, A] =
    ev.headOption

  def index[S, I, A](index: I)(implicit ev: Index.Aux[S, I, A]): Optional[Unit, S, A] =
    ev.index(index)

  def tailOption[S](implicit ev: Cons[S]): Optional[Unit, S, S] =
    ev.tailOption

  def possible[A, B](implicit ev: Possible.Aux[A, B]): Optional[Unit, A, B] =
    ev.possible
}
