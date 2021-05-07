package wolfendale.scalacheck.regexp

import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{MustMatchers, WordSpec}
import ast._

class ASTProcessorSpec extends WordSpec with MustMatchers with PropertyChecks {

  implicit def noShrink[A]: Shrink[A] = Shrink.shrinkAny

  ".apply" must {

    "generate a literal char" in {

      forAll(Gen.alphaNumChar) {
        literal =>

          val gen: Gen[String] =
            ASTProcessor.apply(Literal(literal))

          forAll(gen) {
            str =>
              str must fullyMatch regex literal.toString
          }
      }
    }

    "generate a word char" in {

      val gen: Gen[String] =
        ASTProcessor.apply(CharacterClass.Word)

      forAll(gen) {
        str =>
          str must fullyMatch regex "\\w"
      }
    }

    "generate a digit char" in {

      val gen: Gen[String] =
        ASTProcessor.apply(CharacterClass.Digit)

      forAll(gen) {
        str =>
          str must fullyMatch regex "\\d"
      }
    }

    "generate a space char" in {

      val gen: Gen[String] =
        ASTProcessor.apply(CharacterClass.Space)

      forAll(gen) {
        str =>
          str must fullyMatch regex "\\s"
      }
    }

    // not a great test :/
    "generate any char" in {

      val gen: Gen[String] =
        ASTProcessor.apply(CharacterClass.Any)

      forAll(gen) {
        str =>
          str must fullyMatch regex "."
      }
    }

    "generate something within a group" in {

      forAll(Gen.alphaNumChar) {
        literal =>

          val gen: Gen[String] =
            ASTProcessor.apply(Group(Literal(literal)))

          forAll(gen) {
            str =>
              str must fullyMatch regex s"($literal)"
          }
      }
    }

    "generate something within a non capturing group" in {

      forAll(Gen.alphaNumChar) {
        literal =>

          val gen: Gen[String] =
            ASTProcessor.apply(Group(Literal(literal), capturing = false))

          forAll(gen) {
            str =>
              str must fullyMatch regex s"(?:$literal)"
          }
      }
    }

    "generate something from an alternation" in {

      val gen: Gen[String] =
        ASTProcessor.apply(Or(Literal('a'), Literal('b')))

      forAll(gen) {
        str =>
          str must fullyMatch regex "a|b"
      }
    }

    "generate multiple terms" in {

      val gen: Gen[String] =
        ASTProcessor.apply(And(CharacterClass.Word, CharacterClass.Digit))

      forAll(gen) {
        str =>
          str must fullyMatch regex "\\w\\d"
      }
    }

    // bad test
    "generate an optional term" in {

      val gen: Gen[String] =
        ASTProcessor.apply(Optional(CharacterClass.Word))

      forAll(gen) {
        str =>
          str must fullyMatch regex "\\w?"
      }
    }

    // bad test
    "generate one or more of a term" in {

      val gen: Gen[String] =
        ASTProcessor.apply(OneOrMore(CharacterClass.Word))

      forAll(gen) {
        str =>
          str must fullyMatch regex "\\w+"
      }
    }

    // bad test
    "generate zero or more of a term" in {

      val gen: Gen[String] =
        ASTProcessor.apply(ZeroOrMore(CharacterClass.Word))

      forAll(gen) {
        str =>
          str must fullyMatch regex "\\w*"
      }
    }

    "generate a range of terms" in {

      val gen: Gen[String] =
        ASTProcessor.apply(Range(CharacterClass.Word, 3, 33))

      forAll(gen) {
        str =>
          str must fullyMatch regex "\\w{3,33}"
      }
    }

    "generate a range of terms with no max" in {

      val gen: Gen[String] =
        ASTProcessor.apply(RangeFrom(CharacterClass.Word, 3))

      forAll(gen) {
        str =>
          str must fullyMatch regex "\\w{3,}"
      }
    }

    "generate a specific number of terms" in {

      val gen: Gen[String] =
        ASTProcessor.apply(Length(CharacterClass.Word, 33))

      forAll(gen) {
        str =>
          str must fullyMatch regex "\\w{33}"
      }
    }

    "generate a negated word char" in {

      val gen: Gen[String] =
        ASTProcessor.apply(CharacterClass.Negated(CharacterClass.Word))

      forAll(gen) {
        str =>
          str must fullyMatch regex "\\W"
      }
    }

    "generate a negated space char" in {

      val gen: Gen[String] =
        ASTProcessor.apply(CharacterClass.Negated(CharacterClass.Space))

      forAll(gen) {
        str =>
          str must fullyMatch regex "\\S"
      }
    }

    "generate a negated digit char" in {

      val gen: Gen[String] =
        ASTProcessor.apply(CharacterClass.Negated(CharacterClass.Digit))

      forAll(gen) {
        str =>
          str must fullyMatch regex "\\D"
      }
    }

    "generate an empty string for beginning of string anchor" in {

      val gen: Gen[String] =
        ASTProcessor.apply(Start)

      forAll(gen) {
        str =>
          str mustBe empty
      }
    }

    "generate an empty string for end of string anchor" in {

      val gen: Gen[String] =
        ASTProcessor.apply(End)

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

      forAll(Gen.alphaNumChar) {
        literal =>

          val gen: Gen[String] =
            ASTProcessor.apply(CharacterClass.Group(CharacterClass.Literal(literal)))

          forAll(gen) {
            str =>
              str must fullyMatch regex s"[$literal]"
          }
      }
    }

    "generate a character class which behaves like an alternation" in {

      val gen: Gen[String] =
        ASTProcessor.apply(CharacterClass.Group(CharacterClass.Literal('a'), CharacterClass.Literal('b')))

      forAll(gen) {
        str =>
          str must fullyMatch regex "a|b"
          str must fullyMatch regex "[ab]"
      }
    }

    "generate a character class with a word char" in {

      val gen: Gen[String] =
        ASTProcessor.apply(CharacterClass.Group(CharacterClass.Word))

      forAll(gen) {
        str =>
          str must fullyMatch regex "\\w"
          str must fullyMatch regex "[\\w]"
      }
    }

    "generate a character class with a digit char" in {

      val gen: Gen[String] =
        ASTProcessor.apply(CharacterClass.Group(CharacterClass.Digit))

      forAll(gen) {
        str =>
          str must fullyMatch regex "\\d"
          str must fullyMatch regex "[\\d]"
      }
    }

    "generate a character class with a space char" in {

      val gen: Gen[String] =
        ASTProcessor.apply(CharacterClass.Group(CharacterClass.Space))

      forAll(gen) {
        str =>
          str must fullyMatch regex "\\s"
          str must fullyMatch regex "[\\s]"
      }
    }

    "generate a character class with a range" in {

      val genGen: Gen[(Char, Char, Gen[String])] =
        for {
          min <- Gen.choose('0', '9')
          max <- Gen.choose(min, '9')
        } yield (min, max, ASTProcessor.apply(CharacterClass.Group(CharacterClass.Range(min, max))))

      forAll(genGen) {
        case (min, max, gen) =>
          forAll(gen) {
            str =>
              str must fullyMatch regex s"[$min-$max]"
          }
      }
    }
  }
}
