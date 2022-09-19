package wolfendale.scalacheck.regexp

import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class RegexpGenSpec extends AnyWordSpec with Matchers with ScalaCheckPropertyChecks {

  implicit val arbChar: Arbitrary[Char] = Arbitrary(Gen.alphaNumChar)
  implicit val noShrinkString: Shrink[String] = Shrink.shrinkAny

  ".from" must {

    "create a valid generator from `gbp [+-]?[0-9]{5,9}\\.[0-9]{2}`" in {

      val r = "gbp [+-]?[0-9]{5,9}\\.[0-9]{2}"
      val gen = RegexpGen.from(r)

      forAll(gen) {
        str =>
          println(str)
          str must fullyMatch regex r
      }
    }

    "create a valid generator from `[1-9]\\d?(,\\d{3}){0,3}`" in {

      val r = "[1-9]\\d?(,\\d{3}){0,3}"
      val gen = RegexpGen.from(r)

      forAll(gen) {
        str =>
          println(str)
          str must fullyMatch regex r
      }
    }

    "create a valid generator from `^[a-zA-Z &`\\-\\'\\.]{1,35}$`" in {

      val r = "^[a-zA-Z &`\\-\\'\\.]{1,35}$"
      val gen = RegexpGen.from(r)

      forAll(gen) {
        str =>
          println(str)
          str must fullyMatch regex r
      }
    }
  }
}
