package wolfendale.scalacheck.regexp

import scala.util.parsing.combinator.{PackratParsers, RegexParsers}
import scala.util.parsing.input.CharSequenceReader
import ast._

object GenParser extends RegexParsers with PackratParsers {

  override def skipWhitespace: Boolean = false

  private lazy val int: Parser[Int] = {
    "\\d+".r ^^ { _.toInt }
  }

  private lazy val any: Parser[String] = ".".r

  private lazy val literalTerm: Parser[Literal] = {

    val meta: Parser[String] =
      "\\w" | "\\d" | "\\s" | "\\W" | "\\D" | "\\S" |
      ")" | "(" | "$" | "[" | "." | "+" | "*" | "?" | "|" | "\\" | "{"

    val escaped: Parser[String] =
      "\\" ~> meta

    val allowed: Parser[String] =
      not(meta) ~> any

    (escaped | allowed) ^^ {
      str =>
        Literal(str.last)
    }
  }

  private lazy val characterClass: Parser[CharacterClass with Term] = {

    lazy val insideCharacterClass: Parser[CharacterClass.Group.Term] = {

      val allowed: Parser[String] =
        not("]" | "\\") ~> any

      val literalTerm: Parser[CharacterClass.Literal] = {

        val meta: Parser[String] =
          "\\w" | "\\d" | "\\s" | "\\W" | "\\D" | "\\S"

        val escaped: Parser[String] =
          "\\" ~> (meta | any)

        (escaped | allowed) ^^ {
          str =>
            CharacterClass.Literal(str.last)
        }
      }

      val range: Parser[CharacterClass.Range] =  {

        any ~ ("-" ~> allowed) ^^ {
          case min ~ max =>
            CharacterClass.Range(min.last, max.last)
        }
      }

      lazy val intersection: PackratParser[CharacterClass.Intersection] = {

        val separator: Parser[String] = "&&"

        insideCharacterClass ~ (separator ~> insideCharacterClass) ^^ {
          case one ~ two =>
            CharacterClass.Intersection(one, two)
        }
      }

      intersection | characterClass | range | literalTerm
    }

    lazy val inclusive: Parser[CharacterClass with Term] = {

      lazy val group: Parser[CharacterClass.Group] = {

        '[' ~> insideCharacterClass.* <~ ']' ^^ {
          classes =>
            CharacterClass.Group(classes: _*)
        }
      }

      val word: Parser[CharacterClass.Word.type] =
        "\\w" ^^^ { CharacterClass.Word }
      val digit: Parser[CharacterClass.Digit.type] =
        "\\d" ^^^ { CharacterClass.Digit }
      val space: Parser[CharacterClass.Space.type] =
        "\\s" ^^^ { CharacterClass.Space }
      val any: Parser[CharacterClass.Any.type] =
        '.'   ^^^ { CharacterClass.Any }

      group | word | digit | space | any
    }

    lazy val exclusive: Parser[CharacterClass.Negated] = {

      lazy val negatedGroup =
        "[^" ~> insideCharacterClass.* <~ ']' ^^ {
          classes =>
            CharacterClass.Negated(CharacterClass.Group(classes: _*))
        }

      val negatedWord =
        "\\W" ^^^ { CharacterClass.Negated(CharacterClass.Word) }
      val negatedDigit =
        "\\D" ^^^ { CharacterClass.Negated(CharacterClass.Digit) }
      val negatedSpace =
        "\\S" ^^^ { CharacterClass.Negated(CharacterClass.Space) }

      negatedGroup | negatedWord | negatedSpace | negatedDigit
    }

    exclusive | inclusive
  }

  private lazy val meta: Parser[Meta] = {

    val start: Parser[Start.type] =
      '^' ^^^ { Start }

    val end: Parser[End.type] =
      '$' ^^^ { End }

    val wordBoundary: Parser[WordBoundary.type] =
      "\\b" ^^^ { WordBoundary }

    val negatedWordBoundary: Parser[NegatedWordBoundary.type] =
      "\\B" ^^^ { NegatedWordBoundary }

    start | end | wordBoundary | negatedWordBoundary
  }

  private lazy val substitution: Parser[Substitution] =
    '\\' ~> int ^^ { Substitution.apply }

  private lazy val quantified: PackratParser[Quantified] = {

    val optional: Parser[Optional] =
      (term <~ '?') ^^ { Optional.apply }

    val zeroOrMore: Parser[ZeroOrMore] =
      (term <~ '*') ^^ { ZeroOrMore.apply }

    val oneOrMore: Parser[OneOrMore] =
      (term <~ '+') ^^ { OneOrMore.apply }

    val length: Parser[Length] =
      (term ~ ('{' ~> int <~ '}')) ^^ {
        case t ~ l =>
          Length(t, l)
      }

    val rangeFrom: Parser[RangeFrom] =
      (term ~ ('{' ~> int <~ ",}")) ^^ {
        case t ~ min =>
          RangeFrom(t, min)
      }

    val range: Parser[Range] =
      (term ~ ('{' ~> int ~ (',' ~> int <~ '}'))) ^^ {
        case t ~ (min ~ max) =>
          Range(t, min, max)
      }

    optional | zeroOrMore | oneOrMore | length | rangeFrom | range
  }

  private lazy val group: Parser[Group] = {

    lazy val nonCapturingGroup: Parser[Group] =
      ("(?:" ~> regularExpression <~ ')') ~ regularExpression.?  ^^ {
        case inner ~ rest =>
          Group(inner, rest, capturing = false)
      }

    lazy val capturingGroup: Parser[Group] =
      ('(' ~> regularExpression <~ ')') ~ regularExpression.? ^^ {
        case inner ~ rest =>
          Group(inner, rest)
      }

    nonCapturingGroup | capturingGroup
  }

  private lazy val term: PackratParser[Term] = {
    quantified | group | characterClass | meta | substitution | literalTerm
  }

  private lazy val regularExpression: PackratParser[RegularExpression] = {

    lazy val or: Parser[Or] =
      (regularExpression ~ ('|' ~> term)) ^^ {
        case left ~ right => Or(left, right)
      }

    lazy val and: Parser[And] =
      (regularExpression ~ term) ^^ {
        case left ~ right => And(left, right)
      }

    and | or | term
  }

  def parse(string: String): RegularExpression =
    regularExpression(new PackratReader(new CharSequenceReader(string))).get
}
