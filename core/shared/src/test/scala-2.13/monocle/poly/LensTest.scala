package monocle.poly

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class LensTest extends AnyFunSuite with Matchers {
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
}
