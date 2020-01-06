package monocle

import monocle.poly.{PIso, PPrism}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class TypeInferenceTest extends AnyFunSuite with Matchers {
  case class Id[A](value: A)

  test("monomorphic") {
    def id[A]: Iso[Id[A], A] = Iso[Id[A], A](_.value)(Id(_))

    // works
    Prism.some.getOption(Some("foo")) shouldEqual Some("foo")
    Prism.some.set("bar")(Some("foo")) shouldEqual Some("bar")

    // type mismatch;
    //[error]  found   : monocle.Iso[Id[Nothing],Nothing]
    //[error]  required: monocle.Iso[Id[Nothing],C]
//    Prism.some.composeIso(id).getOption(Some(Id(5))) shouldEqual Some(5)

    import monocle.syntax.applied._

    // polymorphic expression cannot be instantiated to expected type;
    // [error]  found   : [A]monocle.Prism[Option[A],A]
    // [error]  required: monocle.Prism[Some[Id[Int]],?]
//    Some(Id(5)).optic.composePrism(Prism.some).composeIso(id).getOption shouldEqual Some(5)

    Option(Id(5)).optic.composePrism(Prism.some).composeIso(id).getOption shouldEqual Some(5)
    Option(Id(5)).optic.composePrism(Prism.some).composeIso(id).set(3) shouldEqual Option(Id(3))

  }

  test("polymorphic") {
    def id[A, B]: PIso[Id[A], Id[B], A, B] = PIso[Id[A], Id[B], A, B](_.value)(Id(_))

    // works
    PPrism.some.getOption(Some("foo")) shouldEqual Some("foo")
    PPrism.some.set(5)(Some("foo")) shouldEqual Some(5)

    // type mismatch;
    //[error]  found   : monocle.poly.PIso[Id[Nothing],Id[Nothing],Nothing,Nothing]
    //[error]  required: monocle.poly.PIso[Id[Nothing],Id[Nothing],Nothing,C2]
//    PPrism.some.andThenIso(id).getOption(Some(Id(5))) shouldEqual Some(5)

    import monocle.poly.applied.syntax._

    // type mismatch;
    // [error]  found   : monocle.poly.PPrism[Option[Id[Int]],Option[Nothing],Id[Int],Nothing]
    // [error]  required: monocle.poly.PPrism[Some[Id[Int]],Option[Nothing],Id[Int],C2]
//    Some(Id(5)).optic.andThenPrism(poly.PPrism.some).andThenIso(id).getOption shouldEqual Some(5)

    Some(Id(5)).optic.andThenPrism(poly.Prism.some).andThenIso(id).getOption shouldEqual Some(5)

    // polymorphic expression cannot be instantiated to expected type;
    //[error]  found   : [A, B]monocle.poly.PPrism[Option[A],Option[B],A,B]
    //[error]  required: monocle.poly.PPrism[Some[Id[Int]],String,?,?]
//    Some(Id(5)).optic[String].andThenPrism(poly.PPrism.some).andThenIso(id).set("foo") shouldEqual Some("foo")

  }

}
