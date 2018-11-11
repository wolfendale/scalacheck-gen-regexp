package wolfendale.scalacheck.regexp

import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{MustMatchers, WordSpec}
import wolfendale.scalacheck.regexp.ast._

class GenParserSpec extends WordSpec with MustMatchers with PropertyChecks {

  implicit def noShrink[A]: Shrink[A] = Shrink.shrinkAny

  ".parse" must {

    val alphaNumAndSpace: Gen[Char] =
      Gen.alphaChar

    "parse a literal" in {
      forAll(alphaNumAndSpace) {
        char =>
          GenParser.parse(s"$char") mustEqual Literal(char)
      }
    }

    Seq('\\', ')', '(', '$', '[', '.', '+', '*', '|', '?').foreach { //, "\\w", "\\d", "\\s").foreach {
      meta =>
        s"parse an escaped `$meta`" in {
          GenParser.parse(s"\\$meta") mustEqual Literal(meta)
        }
    }

    Seq('w', 'd', 's', 'W', 'D', 'S').foreach {
      meta =>
        s"parse an escaped `\\$meta`" in {
          GenParser.parse(s"\\\\$meta") mustEqual Literal(meta)
        }
    }

    "parse a word char" in {
      GenParser.parse("\\w") mustEqual CharacterClass.Word
    }

    "parse a digit char" in {
      GenParser.parse("\\d") mustEqual CharacterClass.Digit
    }

    "parse a space char" in {
      GenParser.parse("\\s") mustEqual CharacterClass.Space
    }

    "parse a wildcard" in {
      GenParser.parse(".") mustEqual CharacterClass.Any
    }

    "parse a literal character from a character class" in {

      val gen: Gen[Seq[Char]] =
        Gen.nonEmptyListOf(Gen.alphaNumChar)

      forAll(gen) {
        a =>
          GenParser.parse(s"[${a.mkString("")}]") mustEqual CharacterClass.Group(
            a.map(c => CharacterClass.Literal(c)): _*
          )
      }
    }

    "parse a char range" in {

      val gen: Gen[(Char, Char)] =
        for {
          min <- Gen.choose(Char.MinValue, Char.MaxValue)
          max <- Gen.choose(min, Char.MaxValue)
        } yield (min, max)

      forAll(gen) {
        case (min, max) =>
          GenParser.parse(s"[$min-$max]") mustEqual CharacterClass.Group(CharacterClass.Range(min, max))
      }
    }

    "fail to parse an out of order lowercase char range" ignore {

      val gen: Gen[(Char, Char)] =
        for {
          min <- Gen.choose((Char.MinValue + 1).toChar, Char.MaxValue)
          max <- Gen.choose(Char.MinValue, min)
        } yield (min, max)

      forAll(gen) {
        case (min, max) =>
          an[Exception] mustBe thrownBy(GenParser.parse(s"[$min-$max"))
      }
    }

    "parse a word char in a character class" in {
      GenParser.parse("[\\w]") mustEqual CharacterClass.Group(CharacterClass.Word)
    }

    "parse a digit char in a character class" in {
      GenParser.parse("[\\d]") mustEqual CharacterClass.Group(CharacterClass.Digit)
    }

    "parse a space char in a character class" in {
      GenParser.parse("[\\s]") mustEqual CharacterClass.Group(CharacterClass.Space)
    }

    Seq('\\', ']').foreach {
      meta =>
        s"parse an escaped `$meta` in a character class" in {
          GenParser.parse(s"[\\$meta]") mustEqual CharacterClass.Group(CharacterClass.Literal(meta))
        }
    }

    Seq('w', 'd', 's').foreach {
      meta =>
        s"parse an escaped `\\$meta` in a character class" in {
          GenParser.parse(s"[\\\\$meta]") mustEqual CharacterClass.Group(CharacterClass.Literal(meta))
        }
    }

    "parse alternates of multiple terms" in {
      forAll(alphaNumAndSpace, alphaNumAndSpace, alphaNumAndSpace) {
        case (a, b, c) =>
          GenParser.parse(s"$a|$b|$c") mustEqual Or(Or(Literal(a), Literal(b)), Literal(c))
      }
    }

    "parse a group" in {
      forAll(alphaNumAndSpace) {
        a =>
          GenParser.parse(s"(($a))") mustEqual Group(Group(Literal(a)))
      }
    }

    "parse a non-capturing group" in {
      forAll(alphaNumAndSpace) {
        a =>
          GenParser.parse(s"(?:(?:$a))") mustEqual Group(Group(Literal(a), capturing = false), capturing = false)
      }
    }

    "parse a group with an alternate in it" in {
      forAll(alphaNumAndSpace, alphaNumAndSpace, alphaNumAndSpace, alphaNumAndSpace) {
        case (a, b, c, d) =>
          GenParser.parse(s"$a($b|$c)$d") mustEqual And(Literal(a), Group(Or(Literal(b), Literal(c)), Some(Literal(d))))
      }
    }

    "parse a nested group with an expression to the right" in {
      forAll(alphaNumAndSpace, alphaNumAndSpace) {
        case (a, b) =>
          GenParser.parse(s"(($a)$b)") mustEqual Group(Group(Literal(a), Some(Literal(b))))
      }
    }

    "parse a nested group with an expression to the left" in {
      forAll(alphaNumAndSpace, alphaNumAndSpace) {
        case (a, b) =>
          GenParser.parse(s"($a($b))") mustEqual Group(And(Literal(a), Group(Literal(b))))
      }
    }

    "parse an optional literal" in {
      GenParser.parse("a?") mustEqual Optional(Literal('a'))
    }

    "parse an optional literal (bind to quantifier rather than surrounding literals)" in {
      GenParser.parse("abc?") mustEqual And(And(Literal('a'), Literal('b')), Optional(Literal('c')))
    }

    "parse an optional group" in {
      GenParser.parse("(abc)?") mustEqual Optional(Group(And(And(Literal('a'), Literal('b')), Literal('c'))))
    }

    "parse an optional with an alternate" in {
      GenParser.parse("ab|c?") mustEqual Or(And(Literal('a'), Literal('b')), Optional(Literal('c')))
    }

    "parse an optional with an alternate (left)" in {
      GenParser.parse("ab?|c") mustEqual Or(And(Literal('a'), Optional(Literal('b'))), Literal('c'))
    }

    "parse a 'zeroOrMore' literal" in {
      GenParser.parse("a*") mustEqual ZeroOrMore(Literal('a'))
    }

    "parse a 'zeroOrMore' literal (bind to quantifier rather than surrounding literals)" in {
      GenParser.parse("abc*") mustEqual And(And(Literal('a'), Literal('b')), ZeroOrMore(Literal('c')))
    }

    "parse a 'zeroOrMore' group" in {
      GenParser.parse("(abc)*") mustEqual ZeroOrMore(Group(And(And(Literal('a'), Literal('b')), Literal('c'))))
    }

    "parse a 'zeroOrMore' with an alternate" in {
      GenParser.parse("ab|c*") mustEqual Or(And(Literal('a'), Literal('b')), ZeroOrMore(Literal('c')))
    }

    "parse a 'zeroOrMore' with an alternate (left)" in {
      GenParser.parse("ab*|c") mustEqual Or(And(Literal('a'), ZeroOrMore(Literal('b'))), Literal('c'))
    }

    "parse a 'oneOrMore' literal" in {
      GenParser.parse("a+") mustEqual OneOrMore(Literal('a'))
    }

    "parse a 'oneOrMore' literal (bind to quantifier rather than surrounding literals)" in {
      GenParser.parse("abc+") mustEqual And(And(Literal('a'), Literal('b')), OneOrMore(Literal('c')))
    }

    "parse a 'oneOrMore' group" in {
      GenParser.parse("(abc)+") mustEqual OneOrMore(Group(And(And(Literal('a'), Literal('b')), Literal('c'))))
    }

    "parse a 'oneOrMore' with an alternate" in {
      GenParser.parse("ab|c+") mustEqual Or(And(Literal('a'), Literal('b')), OneOrMore(Literal('c')))
    }

    "parse a 'oneOrMore' with alternate (left)" in {
      GenParser.parse("ab+|c") mustEqual Or(And(Literal('a'), OneOrMore(Literal('b'))), Literal('c'))
    }

    "parse a 'rangeFrom'" in {
      forAll(Gen.chooseNum(0, 100)) {
        min =>
          GenParser.parse(s"a{$min,}") mustEqual RangeFrom(Literal('a'), min)
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
          GenParser.parse(s"a{$min,$max}") mustEqual Range(Literal('a'), min, max)
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
          GenParser.parse(s"ab{$length}") mustEqual And(Literal('a'), Length(Literal('b'), length))
      }
    }

    "parse word boundaries" in {
      GenParser.parse("\\bcat\\b") mustEqual And(And(And(And(WordBoundary, Literal('c')), Literal('a')), Literal('t')), WordBoundary)
    }

    "parse string boundaries" in {
      GenParser.parse("^cat$") mustEqual And(And(And(And(Start, Literal('c')), Literal('a')), Literal('t')), End)
    }

    "parse substitutions" in {
      GenParser.parse("(a)\\1") mustEqual Group(Literal('a'), Some(Substitution(1)))
    }

    "parse a negated word char" in {
      GenParser.parse("\\W") mustEqual CharacterClass.Negated(CharacterClass.Word)
    }

    "parse a negated space char" in {
      GenParser.parse("\\S") mustEqual CharacterClass.Negated(CharacterClass.Space)
    }

    "parse a negated digit char" in {
      GenParser.parse("\\D") mustEqual CharacterClass.Negated(CharacterClass.Digit)
    }

    "parse a negated word boundary" in {
      GenParser.parse("\\B") mustEqual NegatedWordBoundary
    }

    "parse a negated character class" in {
      GenParser.parse("[^abc]") mustEqual CharacterClass.Negated(CharacterClass.Group(CharacterClass.Literal('a'), CharacterClass.Literal('b'), CharacterClass.Literal('c')))
    }

    "parse input containing EOS character" in {
      forAll(alphaNumAndSpace) { a =>
        GenParser.parse(a + "$") mustEqual And(Literal(a), End)
      }
    }

    "parse a character class with intersections" in {
      GenParser.parse("[a&&a]") mustEqual CharacterClass.Group(CharacterClass.Intersection(CharacterClass.Literal('a'), CharacterClass.Literal('a')))
    }

    "parse a character class with multiple intersections" in {
      GenParser.parse("[a&&a&&a]") mustEqual CharacterClass.Group(CharacterClass.Intersection(CharacterClass.Literal('a'), CharacterClass.Intersection(CharacterClass.Literal('a'), CharacterClass.Literal('a'))))
    }

    "parse a more complicated character class with intersections" in {
      GenParser.parse("[\\w&&[abc]&&a]") mustEqual CharacterClass.Group(
        CharacterClass.Intersection(
          CharacterClass.Word,
          CharacterClass.Intersection(
            CharacterClass.Group(
              CharacterClass.Literal('a'),
              CharacterClass.Literal('b'),
              CharacterClass.Literal('c')
            ),
            CharacterClass.Literal('a')
          )
        )
      )
    }
  }
}
