package monocle

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class OpticErrorTest extends AnyFunSuite with Matchers {

  case class Foo(map: Map[Int, Option[Boolean]])

  val map: Lens[Foo, Map[Int, Option[Boolean]]] =
    Lens[Foo, Map[Int, Option[Boolean]]](_.map)((foo, newVal) => foo.copy(map = newVal))

  def at(i: Int): Optional[String, Map[Int, Option[Boolean]], Option[Boolean]] =
    UOptional.index[Map[Int, Option[Boolean]], Int, Option[Boolean]](i).withError(s"Missing value at index $i")

  def some[A]: Prism[String, Option[A], A] =
    UPrism.some[A].withError("Expected Some but got None")

  val foo = Foo(Map(1 -> Some(true), 2 -> None))

  def optic(i: Int): Optional[String, Foo, Boolean] =
    map.compose(at(i)).compose(some[Boolean])

  test("get error reporting") {
    optic(1).getEither(foo) shouldEqual Right(true)
    optic(2).getEither(foo) shouldEqual Left("Expected Some but got None")
    optic(3).getEither(foo) shouldEqual Left(s"Missing value at index 3")
  }

  test("set error reporting") {
    optic(1).setE(false)(foo) shouldEqual Right(Foo(Map(1 -> Some(false), 2 -> None)))
    optic(2).setE(false)(foo) shouldEqual Left("Expected Some but got None")
    optic(3).setE(false)(foo) shouldEqual Left(s"Missing value at index 3")
  }

}
