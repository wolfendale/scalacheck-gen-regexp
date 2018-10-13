package wolfendale.scalacheck.regexp

import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{MustMatchers, WordSpec}
import ast._

class ASTProcessorSpec extends WordSpec with MustMatchers with PropertyChecks {

  ".apply" must {

    "generate a literal char" in {

      val strGen: Gen[String] = Gen.alphaNumChar.map(_.toString)

      forAll(strGen) {
        literal =>

          val gen: Gen[String] =
            ASTProcessor.apply(Literal(literal))

          forAll(gen) {
            str =>
              str must fullyMatch regex literal
          }
      }
    }

    "generate a word char" in {

      val gen: Gen[String] =
        ASTProcessor.apply(WordChar)

      forAll(gen) {
        str =>
          str must fullyMatch regex "\\w"
      }
    }

    "generate a digit char" in {

      val gen: Gen[String] =
        ASTProcessor.apply(DigitChar)

      forAll(gen) {
        str =>
          str must fullyMatch regex "\\d"
      }
    }

    "generate a space char" in {

      val gen: Gen[String] =
        ASTProcessor.apply(SpaceChar)

      forAll(gen) {
        str =>
          str must fullyMatch regex "\\s"
      }
    }

    // not a great test :/
    "generate any char" in {

      val gen: Gen[String] =
        ASTProcessor.apply(AnyChar)

      forAll(gen) {
        str =>
          str must fullyMatch regex "."
      }
    }

    "generate something within a group" in {

      val strGen: Gen[String] = Gen.alphaNumChar.map(_.toString)

      forAll(strGen) {
        literal =>

          val gen: Gen[String] =
            ASTProcessor.apply(Group(Literal(literal)))

          forAll(gen) {
            str =>
              str must fullyMatch regex literal
          }
      }
    }

    "generate something within a non capturing group" in {

      val strGen: Gen[String] = Gen.alphaNumChar.map(_.toString)

      forAll(strGen) {
        literal =>

          val gen: Gen[String] =
            ASTProcessor.apply(NonCapturingGroup(Literal(literal)))

          forAll(gen) {
            str =>
              str must fullyMatch regex literal
          }
      }
    }

    "generate something from an alternation" in {

      val gen: Gen[String] =
        ASTProcessor.apply(Or(Literal("a"), Literal("b")))

      forAll(gen) {
        str =>
          str must fullyMatch regex "a|b"
      }
    }

    "generate multiple terms" in {

      val gen: Gen[String] =
        ASTProcessor.apply(And(WordChar, DigitChar))

      forAll(gen) {
        str =>
          str must fullyMatch regex "\\w\\d"
      }
    }

    // bad test
    "generate an optional term" in {

      val gen: Gen[String] =
        ASTProcessor.apply(Optional(WordChar))

      forAll(gen) {
        str =>
          str must fullyMatch regex "\\w?"
      }
    }

    // bad test
    "generate one or more of a term" in {

      val gen: Gen[String] =
        ASTProcessor.apply(OneOrMore(WordChar))

      forAll(gen) {
        str =>
          str must fullyMatch regex "\\w+"
      }
    }

    // bad test
    "generate zero or more of a term" in {

      val gen: Gen[String] =
        ASTProcessor.apply(ZeroOrMore(WordChar))

      forAll(gen) {
        str =>
          str must fullyMatch regex "\\w*"
      }
    }

    "generate a range of terms" in {

      val gen: Gen[String] =
        ASTProcessor.apply(Range(WordChar, 3, 33))

      forAll(gen) {
        str =>
          str must fullyMatch regex "\\w{3,33}"
      }
    }

    "generate a range of terms with no max" in {

      val gen: Gen[String] =
        ASTProcessor.apply(RangeFrom(WordChar, 3))

      forAll(gen) {
        str =>
          str must fullyMatch regex "\\w{3,}"
      }
    }

    "generate a specific number of terms" in {

      val gen: Gen[String] =
        ASTProcessor.apply(Length(WordChar, 33))

      forAll(gen) {
        str =>
          str must fullyMatch regex "\\w{33}"
      }
    }

    "generate a negated word char" in {

      val gen: Gen[String] =
        ASTProcessor.apply(Negated(WordChar))

      forAll(gen) {
        str =>
          str must fullyMatch regex "\\W"
      }
    }

    "generate a negated space char" in {

      val gen: Gen[String] =
        ASTProcessor.apply(Negated(SpaceChar))

      forAll(gen) {
        str =>
          str must fullyMatch regex "\\S"
      }
    }

    "generate a negated digit char" in {

      val gen: Gen[String] =
        ASTProcessor.apply(Negated(DigitChar))

      forAll(gen) {
        str =>
          str must fullyMatch regex "\\D"
      }
    }

    "generate an empty string for BOS anchor" in {

      val gen: Gen[String] =
        ASTProcessor.apply(BOS)

      forAll(gen) {
        str =>
          str mustBe empty
      }
    }

    "generate an empty string for EOS anchor" in {

      val gen: Gen[String] =
        ASTProcessor.apply(EOS)

      forAll(gen) {
        str =>
          str mustBe empty
      }
    }

    "generate an empty string for a word boundary" in {

      val gen: Gen[String] =
        ASTProcessor.apply(WordBoundary)

      forAll(gen) {
        str =>
          str mustBe empty
      }
    }

    "generate a character class with a literal" in {

      val strGen: Gen[String] = Gen.alphaNumChar.map(_.toString)

      forAll(strGen) {
        literal =>

          val gen: Gen[String] =
            ASTProcessor.apply(CharacterClass(CharacterClass.Literal(literal)))

          forAll(gen) {
            str =>
              str must fullyMatch regex literal
          }
      }
    }

    "generate a character class which behaves like an alternation" in {

      val gen: Gen[String] =
        ASTProcessor.apply(CharacterClass(CharacterClass.Literal("a"), CharacterClass.Literal("b")))

      forAll(gen) {
        str =>
          str must fullyMatch regex "a|b"
          str must fullyMatch regex "[ab]"
      }
    }

    "generate a character class with a word char" in {

      val gen: Gen[String] =
        ASTProcessor.apply(CharacterClass(CharacterClass.WordChar))

      forAll(gen) {
        str =>
          str must fullyMatch regex "\\w"
          str must fullyMatch regex "[\\w]"
      }
    }

    "generate a character class with a digit char" in {

      val gen: Gen[String] =
        ASTProcessor.apply(CharacterClass(CharacterClass.DigitChar))

      forAll(gen) {
        str =>
          str must fullyMatch regex "\\d"
          str must fullyMatch regex "[\\d]"
      }
    }

    "generate a character class with a space char" in {

      val gen: Gen[String] =
        ASTProcessor.apply(CharacterClass(CharacterClass.SpaceChar))

      forAll(gen) {
        str =>
          str must fullyMatch regex "\\s"
          str must fullyMatch regex "[\\s]"
      }
    }

    "generate a character class with a lowercase char range" in {

      val genGen: Gen[(Char, Char, Gen[String])] =
        for {
          min <- Gen.choose('a', 'z')
          max <- Gen.choose(min, 'z')
        } yield (min, max, ASTProcessor.apply(CharacterClass(CharacterClass.CharRange(min, max))))

      forAll(genGen) {
        case (min, max, gen) =>
          forAll(gen) {
            str =>
              str must fullyMatch regex s"[$min-$max]"
          }
      }
    }

    "generate a character class with an uppercase char range" in {

      val genGen: Gen[(Char, Char, Gen[String])] =
        for {
          min <- Gen.choose('A', 'Z')
          max <- Gen.choose(min, 'Z')
        } yield (min, max, ASTProcessor.apply(CharacterClass(CharacterClass.CharRange(min, max))))

      forAll(genGen) {
        case (min, max, gen) =>
          forAll(gen) {
            str =>
              str must fullyMatch regex s"[$min-$max]"
          }
      }
    }

    "generate a character class with a digit range" in {

      val genGen: Gen[(Int, Int, Gen[String])] =
        for {
          min <- Gen.choose(0, 9)
          max <- Gen.choose(min, 9)
        } yield (min, max, ASTProcessor.apply(CharacterClass(CharacterClass.DigitRange(min, max))))

      forAll(genGen) {
        case (min, max, gen) =>
          forAll(gen) {
            str =>
              str must fullyMatch regex s"[$min-$max]"
          }
      }
    }

    "generate a character class with an intersection" in {

      val genGen: Gen[String] = ASTProcessor.apply(CharacterClass(
        CharacterClass.Intersection(
          CharacterClass.CharRange('a', 'z'),
          CharacterClass(CharacterClass.CharRange('b', 'y')),
          List(CharacterClass(CharacterClass.WordChar))))
      )

      forAll(genGen) { str =>
        str must fullyMatch regex "[a-z&&[b-y]&&[\\w]]"
      }
    }
  }
}
