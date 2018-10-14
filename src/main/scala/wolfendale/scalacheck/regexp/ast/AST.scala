package wolfendale.scalacheck.regexp.ast

sealed trait RegularExpression {

  def |(that: RegularExpression): RegularExpression =
    Or(this, that)

  def &(that: RegularExpression): RegularExpression =
    And(this, that)
}

case class Literal(value: String) extends RegularExpression

case object WordChar extends RegularExpression
case object DigitChar extends RegularExpression
case object SpaceChar extends RegularExpression
case object AnyChar extends RegularExpression

case object BOS extends RegularExpression
case object EOS extends RegularExpression
case object WordBoundary extends RegularExpression

case class Group(term: RegularExpression) extends RegularExpression
case class Substitution(index: Int) extends RegularExpression
case class NonCapturingGroup(term: RegularExpression) extends RegularExpression

case class Or(t1: RegularExpression, t2: RegularExpression) extends RegularExpression
case class And(t1: RegularExpression, t2: RegularExpression) extends RegularExpression

case class Negated(term: RegularExpression) extends RegularExpression

sealed trait Quantified extends RegularExpression
case class Optional(term: RegularExpression) extends Quantified
case class ZeroOrMore(term: RegularExpression) extends Quantified
case class OneOrMore(term: RegularExpression) extends Quantified
case class Length(term: RegularExpression, min: Int) extends Quantified
case class RangeFrom(term: RegularExpression, min: Int) extends Quantified
case class Range(term: RegularExpression, min: Int, max: Int) extends Quantified

object CharacterClass {

  sealed trait Term extends RegularExpression

  case class Complete(terms: CharacterClass.Term*) extends CharacterClass
  case class Negated(terms: CharacterClass.Term*) extends CharacterClass

  case class Literal(value: String) extends Term

  case class DigitRange(min: Int, max: Int) extends Term
  case class CharRange(min: Char, max: Char) extends Term

  case object WordChar extends Term
  case object DigitChar extends Term
  case object SpaceChar extends Term
  case object WordBoundary extends Term

  case class Intersection(classes: CharacterClass*) extends Term

  private lazy val all: Set[Char] = (for { c <- Char.MinValue to Char.MaxValue } yield c).toSet

  def toSet(term: Term): Set[Char] =
    term match {
      case Literal(value) => value.headOption.map(Set(_)).getOrElse(Set())
      case DigitRange(min, max) => (for { i <- min to max } yield i.toChar).toSet
      case CharRange(min, max) => (for { c <- min to max } yield c.toChar).toSet
      case WordChar =>
        toSet(Literal("_")) union
          toSet(CharRange('a', 'z')) union
          toSet(CharRange('A', 'Z')) union
          toSet(DigitRange(0, 9))
      case DigitChar => toSet(DigitRange(0, 9))
      case SpaceChar => Set(' ', '\t')
      case WordBoundary => all diff toSet(WordChar)
      case Intersection(terms @ _*) =>
        terms.map(toSet).reduce(_ intersect _)
//        toSet(left) intersect toSet(right)
    }

  def toSet(charClass: CharacterClass): Set[Char] =
    charClass.terms.map(toSet).foldLeft(Set[Char]())(_ ++ _)

  def apply(terms: CharacterClass.Term*): CharacterClass =
    CharacterClass.Complete(terms: _*)
}

sealed trait CharacterClass extends CharacterClass.Term {
  def terms: Seq[CharacterClass.Term]
}

