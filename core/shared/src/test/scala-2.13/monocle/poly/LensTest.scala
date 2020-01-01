package monocle.poly

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class LensTest extends AnyFunSuite with Matchers {
  case class Foo(map: Map[Int, String], list: List[Int], tuple: (Boolean, String, Int, Long, Double, (Int, String)))

  val foo = Foo(
    map = Map(1 -> "One", 2 -> "Two"),
    list = List(1, 2, 3),
    tuple = (false, "hello", -1, 4L, 0.5, (1, "world"))
  )

  val tuple =
    Lens[Foo, (Boolean, String, Int, Long, Double, (Int, String))](_.tuple)(newValue => _.copy(tuple = newValue))

  test("some poly") {
    case class User[A](name: String, email: Option[String], other: Option[A])

    def other[A, B]: PLens[User[A], User[B], Option[A], Option[B]] =
      PLens[User[A], User[B], Option[A], Option[B]](_.other)(newVal => _.copy(other = newVal))

    val intUser = User("John", Some("john@foo.com"), Some(28))

    other[Int, String].some[Int, String].set("bar")(intUser) shouldEqual User("John", Some("john@foo.com"), Some("bar"))
    other[Int, String].some2.set("bar")(intUser) shouldEqual User("John", Some("john@foo.com"), Some("bar"))
  }

  test("some mono") {
    case class User(name: String, email: Option[String])

    val other: Lens[User, Option[String]] =
      Lens[User, Option[String]](_.email)(newVal => _.copy(email = newVal))

    val intUser = User("John", Some("john@foo.com"))

    other.some[String, String].set("bar")(intUser) shouldEqual User("John", Some("bar"))
    other.some2.set("bar")(intUser) shouldEqual User("John", Some("bar"))
  }

  test("first") {
    tuple.first.get(foo) shouldEqual foo.tuple._1
  }
}
