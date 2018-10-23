package wolfendale.scalacheck.regexp.data

import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{MustMatchers, WordSpec}
import wolfendale.scalacheck.regexp.data.Group._

object Iso extends MustMatchers {
  implicit class IsoOps[A](val left: Group[A]) extends AnyVal {
    def mustBeIsoTo(right: Group[A]): Unit =
      values(left) mustBe values(right)

    private def values(set: Group[A]): Set[A] =
      set match {
        case Inclusion(values) => values
        case Exclusion(values) => values
      }
  }
}

class GroupSpec extends WordSpec with MustMatchers with PropertyChecks {

  import Iso._

  val emptySet: Inclusion[Int] = Inclusion(Set())
  val universalSet: Set[Int] = (1 to 50).toSet
  val universalSet0: Exclusion[Int] = Exclusion(Set())

  def setGen: Gen[Inclusion[Int]] =
    Gen.listOf(Gen.oneOf(universalSet.toList)).map(x => Inclusion(x.toSet))

  "commutative laws" should {
    "hold for A ∪ B = B ∪ A" in {
      forAll(setGen, setGen) {
        (a, b) =>
          a ++ b mustBe b ++ a
      }
    }

    "hold for A ∩ B = B ∩ A" in {
      forAll(setGen, setGen) {
        (a, b) =>
          a.intersect(b) mustBe b.intersect(a)
      }
    }
  }

  "associative laws" should {
    "hold for A ∪ (B ∪ C) = (A ∪ B) ∪ C" in {
      forAll(setGen, setGen, setGen) {
        (a, b, c) =>
          a ++ (b ++ c) mustBe (a ++ b) ++ c
      }
    }

    "hold for A ∩ (B ∩ C) = (A ∩ B) ∩ C" in {
      forAll(setGen, setGen, setGen) {
        (a, b, c) =>
          a.intersect(b.intersect(c)) mustBe a.intersect(b).intersect(c)
      }
    }
  }

  "distributive laws" should {
    "hold for A ∩ (B ∪ C) = (A ∩ B) ∪ (A ∩ C)" in {
      forAll(setGen, setGen, setGen) {
        (a, b, c) =>
          a.intersect(b ++ c) mustBe a.intersect(b) ++ a.intersect(c)
      }
    }

    "hold for A ∪ (B ∩ C) = (A ∪ B) ∩ (A ∪ C)" in {
      forAll(setGen, setGen, setGen) {
        (a, b, c) =>
          a ++ b.intersect(c) mustBe (a ++ b).intersect(a ++ c)
      }
    }
  }

  "identity laws" should {
    "hold for A ∪ ∅ = ∅ ∪ A = A" in {
      forAll(setGen) {
        a =>
          a ++ emptySet mustBe a
          emptySet ++ a mustBe a
      }
    }

    "hold for A ∩ U = U ∩ A = A" in {
      forAll(setGen) {
        a =>
          val u = Inclusion(universalSet)

          a.intersect(u) mustBe a
          u.intersect(a) mustBe a
      }
    }
  }

  "complement laws" should {
    "hold for A ∪ Ac = U" in {
      forAll(setGen) {
        a =>
          val u = universalSet0
          val ac = a.compliment

          a ++ ac mustBe u
      }
    }

    "hold for Ac ∪ A = U" in {
      forAll(setGen) {
        a =>
          val u = universalSet0
          val ac = a.compliment

          ac ++ a mustBe u
      }
    }

    "hold for A ∩ Ac = ∅" in {
      forAll(setGen) {
        a =>
          val ac = a.compliment
          val o = emptySet

          a.intersect(ac) mustBe o
      }
    }

    "hold for Ac ∩ A = ∅" in {
      forAll(setGen) {
        a =>
          val ac = a.compliment
          val o = emptySet

          ac.intersect(a) mustBe o
      }
    }
  }

  "idempotent laws" should {
    "hold for A ∪ A = A" in {
      forAll(setGen) {
        a =>
          a ++ a mustBe a
      }
    }

    "hold for A ∩ A = A" in {
      forAll(setGen) {
        a =>
          a.intersect(a) mustBe a
      }
    }

    "hold for A - Ac = A" in {
      forAll(setGen) {
        a =>
          val ac = a.compliment

          a -- ac mustBe a
      }
    }

    "hold for Ac - A = Ac" in {
      forAll(setGen) {
        a =>
          val ac = a.compliment

          ac -- a mustBe ac
      }
    }
  }

  "null laws" should {
    "hold for A ∪ U = U" in {
      forAll(setGen) {
        a =>
          val u = Inclusion(universalSet)

          a ++ u mustBe u
      }
    }

    "hold for A ∩ ∅ = ∅" in {
      forAll(setGen) {
        a =>
          val o = emptySet

          a.intersect(o) mustBe o
      }
    }

    "hold for ∅ ∩ A = ∅" in {
      forAll(setGen) {
        a =>
          val o = emptySet

          o.intersect(a) mustBe o
      }
    }
  }

  "absorption laws" should {
    "hold for A ∪ (A ∩ B) = A" in {
      forAll(setGen, setGen) {
        (a, b) =>
          a ++ a.intersect(b) mustBe a
      }
    }

    "hold for A ∩ (A ∪ B) = A" in {
      forAll(setGen, setGen) {
        (a, b) =>
          a.intersect(a ++ b) mustBe a
      }
    }
  }

  "demorgans laws" should {
    "hold for (A ∪ B)c = Ac ∩ Bc" in {
      forAll(setGen, setGen) {
        (a, b) =>
          val ac = a.compliment
          val bc = b.compliment

          (a ++ b).compliment mustBe ac.intersect(bc)
      }
    }

    "hold for (A ∩ B)c = Ac ∪ Bc" in {
      forAll(setGen, setGen) {
        (a, b) =>
          val ac = a.compliment
          val bc = b.compliment

          a.intersect(b).compliment mustBe ac ++ bc
      }
    }
  }

  "involution law" should {
    "hold for (Ac)c = A" in {
      forAll(setGen) {
        a =>
          a.compliment.compliment mustBe a
      }
    }
  }


  "kurts laws" should {
    "hold for Ac - B = (A ∪ B)c" in {
      forAll(setGen, setGen) {
        (a, b) =>
          val ac = a.compliment

          ac -- b mustBe (a ++ b).compliment
      }
    }

    "hold for Bc - A = (A ∪ B)c" in {
      forAll(setGen, setGen) {
        (a, b) =>
          val bc = b.compliment

          bc -- a mustBe (a ++ b).compliment
      }
    }

    "hold for A - Bc = A ∩ B" in {
      forAll(setGen, setGen) {
        (a, b) =>
          val bc = b.compliment

          a -- bc mustBe a.intersect(b)
      }
    }

    "hold for B - Ac = A ∩ B" in {
      forAll(setGen, setGen) {
        (a, b) =>
          val ac = a.compliment

          b -- ac mustBe a.intersect(b)
      }
    }

    "hold for A ∩ Bc = A - B" in {
      forAll(setGen, setGen) {
        (a, b) =>
          val bc = b.compliment

          a.intersect(bc) mustBe a -- b
      }
    }

    "hold for A ∪ Bc = B - A" in {
      forAll(setGen, setGen) {
        (a, b) =>
          val bc = b.compliment

          a ++ bc mustBeIsoTo b -- a
      }
    }

    "hold for Bc ∩ A = A - B" in {
      forAll(setGen, setGen) {
        (a, b) =>
          val bc = b.compliment

          bc.intersect(a) mustBe b -- a
      }
    }

    "hold for Bc ∪ A = B - A" in {
      forAll(setGen, setGen) {
        (a, b) =>
          val bc = b.compliment

          bc ++ a mustBeIsoTo b -- a
      }
    }
  }

  "set theorems" should {
    "hold for A ∪ (B − A) = A ∪ B" in {
      forAll(setGen, setGen) {
        (a, b) =>
          a ++ (b -- a) mustBe a ++ b
      }
    }

    "hold for A − B = Bc − Ac" in {
      forAll(setGen, setGen) {
        (a, b) =>
          val ac = a.compliment
          val bc = b.compliment

          a -- b mustBeIsoTo bc -- ac
      }
    }
  }
}