package wolfendale.scalacheck.regexp

import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{MustMatchers, WordSpec}
import ast._

class GenParserSpec extends WordSpec with MustMatchers with PropertyChecks {

  implicit val noShrinkString: Shrink[String] = Shrink.shrinkAny
  implicit def noShrinkSeq[A]: Shrink[Seq[A]] = Shrink.shrinkAny

  ".parse" must {

    val neAlphaNum: Gen[String] = {
      Gen
        .nonEmptyListOf(Gen.alphaNumChar)
        .map(_.mkString(""))
    }

    val alphaNumSpaces: Gen[String] = {
      Gen
        .nonEmptyListOf(Gen.oneOf(Gen.alphaNumChar, Gen.const(" ")))
        .map(_.mkString(""))
    }

    "parse a literal" in {
      forAll(alphaNumSpaces) {
        str =>
          GenParser.parse(str) mustEqual Literal(str)
      }
    }

    Seq("\\", ")", "(", "$", "[", ".", "+", "*", "|", "?", "\\w", "\\d", "\\s").foreach {
      meta =>
        s"parse an escaped `$meta`" in {
          GenParser.parse(s"\\$meta") mustEqual Literal(meta)
        }
    }

    "parse a word char" in {
      GenParser.parse("\\w") mustEqual WordChar
    }

    "parse a digit char" in {
      GenParser.parse("\\d") mustEqual DigitChar
    }

    "parse a space char" in {
      GenParser.parse("\\s") mustEqual SpaceChar
    }

    "parse a wildcard" in {
      GenParser.parse(".") mustEqual AnyChar
    }

    "parse a literal character from a character class" in {

      val gen: Gen[Seq[Char]] =
        Gen.nonEmptyListOf(Gen.alphaNumChar)

      forAll(gen) {
        a =>
          GenParser.parse(s"[${a.mkString("")}]") mustEqual CharacterClass(
            a.map(c => CharacterClass.Literal(c.toString)): _*
          )
      }
    }

    "parse a digit range" in {

      val gen: Gen[(Int, Int)] =
        for {
          min <- Gen.chooseNum(0, 9)
          max <- Gen.chooseNum(min, 9)
        } yield (min, max)

      forAll(gen) {
        case (min, max) =>
          GenParser.parse(s"[$min-$max]") mustEqual CharacterClass(CharacterClass.DigitRange(min, max))
      }
    }

    "fail to parse an out of order digit range" ignore {

      val gen: Gen[(Int, Int)] =
        for {
          min <- Gen.chooseNum(1, 9)
          max <- Gen.chooseNum(0, min)
        } yield (min, max)

      forAll(gen) {
        case (min, max) =>
          an[Exception] mustBe thrownBy(GenParser.parse(s"[$min-$max]"))
      }
    }

    "parse a lowercase char range" in {

      val gen: Gen[(Char, Char)] =
        for {
          min <- Gen.alphaLowerChar
          max <- Gen.choose(min, 'z')
        } yield (min, max)

      forAll(gen) {
        case (min, max) =>
          GenParser.parse(s"[$min-$max]") mustEqual CharacterClass(CharacterClass.CharRange(min, max))
      }
    }

    "fail to parse an out of order lowercase char range" ignore {

      val gen: Gen[(Char, Char)] =
        for {
          min <- Gen.choose('b', 'z')
          max <- Gen.choose('a', min)
        } yield (min, max)

      forAll(gen) {
        case (min, max) =>
          an[Exception] mustBe thrownBy(GenParser.parse(s"[$min-$max"))
      }
    }

    "parse an uppercase char range" in {

      val gen: Gen[(Char, Char)] =
        for {
          min <- Gen.alphaUpperChar
          max <- Gen.choose(min, 'Z')
        } yield (min, max)

      forAll(gen) {
        case (min, max) =>
          GenParser.parse(s"[$min-$max]") mustEqual CharacterClass(CharacterClass.CharRange(min, max))
      }
    }

    "parse a word char in a character class" in {
      GenParser.parse("[\\w]") mustEqual CharacterClass(CharacterClass.WordChar)
    }

    "parse a digit char in a character class" in {
      GenParser.parse("[\\d]") mustEqual CharacterClass(CharacterClass.DigitChar)
    }

    "parse a space char in a character class" in {
      GenParser.parse("[\\s]") mustEqual CharacterClass(CharacterClass.SpaceChar)
    }

    Seq("\\", "]").foreach {
      meta =>
        s"parse an escaped `$meta` in a character class" in {
          GenParser.parse(s"[\\$meta]") mustEqual CharacterClass(CharacterClass.Literal(meta))
        }
    }

    Seq("w", "d", "s").foreach {
      meta =>
        s"parse an escaped `\\$meta` in a character class" in {
          GenParser.parse(s"[\\\\$meta]") mustEqual CharacterClass(CharacterClass.Literal("\\"), CharacterClass.Literal(meta))
        }
    }

    "parse alternates of multiple terms" in {
      forAll(neAlphaNum, neAlphaNum, neAlphaNum) {
        case (a, b, c) =>
          GenParser.parse(s"$a|$b|$c") mustEqual Literal(a) | Literal(b) | Literal(c)
      }
    }

    "parse a group" in {
      forAll(neAlphaNum) {
        a =>
          GenParser.parse(s"(($a))") mustEqual Group(Group(Literal(a)))
      }
    }

    "parse a non-capturing group" in {
      forAll(neAlphaNum) {
        a =>
          GenParser.parse(s"(?:(?:$a))") mustEqual NonCapturingGroup(NonCapturingGroup(Literal(a)))
      }
    }

    "parse a group with an alternate in it" in {
      forAll(neAlphaNum, neAlphaNum, neAlphaNum, neAlphaNum) {
        case (a, b, c, d) =>
          GenParser.parse(s"$a($b|$c)$d") mustEqual And(And(Literal(a), Group(Literal(b) | Literal(c))), Literal(d))
      }
    }

    "parse a nested group with an expression to the right" in {
      forAll(neAlphaNum, neAlphaNum) {
        case (a, b) =>
          GenParser.parse(s"(($a)$b)") mustEqual Group(And(Group(Literal(a)), Literal(b)))
      }
    }

    "parse a nested group with an expression to the left" in {
      forAll(neAlphaNum, neAlphaNum) {
        case (a, b) =>
          GenParser.parse(s"($a($b))") mustEqual Group(And(Literal(a), Group(Literal(b))))
      }
    }

    "parse an optional literal" in {
      GenParser.parse("a?") mustEqual Optional(Literal("a"))
    }

    "parse an optional literal (bind to quantifier rather than surrounding literals)" in {
      GenParser.parse("abc?") mustEqual And(Literal("ab"), Optional(Literal("c")))
    }

    "parse an optional group" in {
      GenParser.parse("(abc)?") mustEqual Optional(Group(Literal("abc")))
    }

    "parse an optional with an alternate" in {
      GenParser.parse("ab|c?") mustEqual Or(Literal("ab"), Optional(Literal("c")))
    }

    "parse an optional with an alternate (left)" in {
      GenParser.parse("ab?|c") mustEqual Or(And(Literal("a"), Optional(Literal("b"))), Literal("c"))
    }

    "parse a 'zeroOrMore' literal" in {
      GenParser.parse("a*") mustEqual ZeroOrMore(Literal("a"))
    }

    "parse a 'zeroOrMore' literal (bind to quantifier rather than surrounding literals)" in {
      GenParser.parse("abc*") mustEqual And(Literal("ab"), ZeroOrMore(Literal("c")))
    }

    "parse a 'zeroOrMore' group" in {
      GenParser.parse("(abc)*") mustEqual ZeroOrMore(Group(Literal("abc")))
    }

    "parse a 'zeroOrMore' with an alternate" in {
      GenParser.parse("ab|c*") mustEqual Or(Literal("ab"), ZeroOrMore(Literal("c")))
    }

    "parse a 'zeroOrMore' with an alternate (left)" in {
      GenParser.parse("ab*|c") mustEqual Or(And(Literal("a"), ZeroOrMore(Literal("b"))), Literal("c"))
    }

    "parse a 'oneOrMore' literal" in {
      GenParser.parse("a+") mustEqual OneOrMore(Literal("a"))
    }

    "parse a 'oneOrMore' literal (bind to quantifier rather than surrounding literals)" in {
      GenParser.parse("abc+") mustEqual And(Literal("ab"), OneOrMore(Literal("c")))
    }

    "parse a 'oneOrMore' group" in {
      GenParser.parse("(abc)+") mustEqual OneOrMore(Group(Literal("abc")))
    }

    "parse a 'oneOrMore' with an alternate" in {
      GenParser.parse("ab|c+") mustEqual Or(Literal("ab"), OneOrMore(Literal("c")))
    }

    "parse a 'oneOrMore' with alternate (left)" in {
      GenParser.parse("ab+|c") mustEqual Or(And(Literal("a"), OneOrMore(Literal("b"))), Literal("c"))
    }

    "parse a 'rangeFrom'" in {
      forAll(Gen.chooseNum(0, 100)) {
        min =>
          GenParser.parse(s"a{$min,}") mustEqual RangeFrom(Literal("a"), min)
      }
    }

    "parse a range" in {

      val gen: Gen[(Int, Int)] =
        for {
          min <- Gen.chooseNum(0, 100)
          max <- Gen.chooseNum(min, 100)
        } yield (min, max)

      forAll(gen) {
        case (min, max) =>
          GenParser.parse(s"a{$min,$max}") mustEqual Range(Literal("a"), min, max)
      }
    }

    "fail to parse a range" ignore {

      val gen: Gen[(Int, Int)] =
        for {
          min <- Gen.chooseNum(1, 100)
          max <- Gen.chooseNum(0, min)
        } yield (min, max)

      forAll(gen) {
        case (min, max) =>
          an[Exception] mustBe thrownBy(GenParser.parse(s"a{$min,$max}"))
      }
    }

    "parse a specific length repeated pattern" in {
      forAll(Gen.choose(1, 100)) {
        length =>
          GenParser.parse(s"ab{$length}") mustEqual And(Literal("a"), Length(Literal("b"), length))
      }
    }

    "parse word boundaries" in {
      GenParser.parse("\\bcat\\b") mustEqual And(And(WordBoundary, Literal("cat")), WordBoundary)
    }

    "parse string boundaries" in {
      GenParser.parse("^cat$") mustEqual And(And(BOS, Literal("cat")), EOS)
    }

    "parse substitutions" in {
      GenParser.parse("(a)\\1") mustEqual And(Group(Literal("a")), Substitution(1))
    }

    "parse a negated word char" in {
      GenParser.parse("\\W") mustEqual Negated(WordChar)
    }

    "parse a negated space char" in {
      GenParser.parse("\\S") mustEqual Negated(SpaceChar)
    }

    "parse a negated digit char" in {
      GenParser.parse("\\D") mustEqual Negated(DigitChar)
    }

    "parse a negated word boundary" in {
      GenParser.parse("\\B") mustEqual Negated(WordBoundary)
    }

    "parse a negated character class" in {
      GenParser.parse("[^abc]") mustEqual Negated(CharacterClass(CharacterClass.Literal("a"), CharacterClass.Literal("b"), CharacterClass.Literal("c")))
    }

    "parse input containing EOS character" in {
      forAll(neAlphaNum) { a =>
        GenParser.parse(a + "$") mustEqual And(Literal(a), EOS)
      }
    }
  }
}
