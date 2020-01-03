package monocle.poly

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class OpticAndThenTest extends AnyFunSuite with Matchers {
  case class User(email: Option[String], address: Address)
  case class Address(streetNumber: Int, postcode: String)

  import OpticAndThen._

  val user = User(email = Some("foo@bar.com"), address = Address(12, "EC1"))

  val email: Lens[User, Option[String]] =
    Lens[User, Option[String]](_.email)(newVal => _.copy(email = newVal))

  val address: Lens[User, Address] =
    Lens[User, Address](_.address)(newVal => _.copy(address = newVal))

  val streetNumber: Lens[Address, Int] =
    Lens[Address, Int](_.streetNumber)(newVal => _.copy(streetNumber = newVal))

  test("Lens andThen Lens") {
    (address >>> streetNumber).get(user) shouldEqual user.address.streetNumber
  }

  test("Lens andThen Prism") {
//  does not compile
//    (email >>> Prism.some).getOption(user) shouldEqual user.email
    (email >>> Prism.some[String]).getOption(user) shouldEqual user.email
  }

}
