package wolfendale.scalacheck.regexp.ast

sealed trait RegularExpression

sealed trait Term extends RegularExpression

final case class Literal(value: Char) extends Term

sealed trait Meta extends Term
case object Start extends Meta
case object End extends Meta
case object WordBoundary extends Meta
case object NegatedWordBoundary extends Meta

final case class Group(term: RegularExpression, rest: Option[RegularExpression] = None, capturing: Boolean = true) extends Term
final case class Substitution(index: Int) extends Term
final case class Or(left: RegularExpression, right: Term) extends Term
final case class And(left: RegularExpression, right: Term) extends Term

sealed trait Quantified extends Term
case class Optional(term: Term) extends Quantified
case class ZeroOrMore(term: Term) extends Quantified
case class OneOrMore(term: Term) extends Quantified
case class Length(term: Term, length: Int) extends Quantified
case class RangeFrom(term: Term, min: Int) extends Quantified
case class Range(term: Term, min: Int, max: Int) extends Quantified

object CharacterClass {

  final case class Literal(value: Char) extends Group.Term
  case object Word extends CharacterClass with Term
  case object Digit extends CharacterClass with Term
  case object Space extends CharacterClass with Term
  case object Any extends CharacterClass with Term
  final case class Negated(characterClass: CharacterClass) extends CharacterClass with Term
  final case class Intersection(left: Group.Term, right: Group.Term) extends CharacterClass

  final case class Group(terms: Group.Term*) extends CharacterClass with Term

  object Group {
    sealed trait Term extends RegularExpression
  }

  final case class Range(min: Char, max: Char) extends CharacterClass {
    require(min <= max)
  }
}

sealed trait CharacterClass extends CharacterClass.Group.Term
