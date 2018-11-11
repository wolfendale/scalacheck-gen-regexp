package wolfendale.scalacheck.regexp

import org.scalacheck.{Arbitrary, Gen}
import ast._
import wolfendale.scalacheck.regexp.data.Group.{Exclusion, Inclusion}

object ASTProcessor {

  def apply(expression: RegularExpression)(implicit ev: Arbitrary[Char]): Gen[String] = {
    expression match {
      case Literal(value) =>
        Gen.const(value.toString)
      case Group(term, rest, _) =>
        for {
          termGen <- apply(term)
          restGen <- rest.map(apply)
            .getOrElse(Gen.const(""))
        } yield termGen + restGen
      case Or(left, right) =>
        Gen.oneOf(apply(left), apply(right))
      case And(left, right) =>
        for {
          leftGen  <- apply(left)
          rightGen <- apply(right)
        } yield leftGen + rightGen
      case _: Meta =>
        Gen.const("")
      case expression: Quantified =>
        quantified(expression)
      case expression: CharacterClass.Group.Term =>
        characterClass(expression)
          .map(_.toString)
      case expression =>
        sys.error(s"Unsupported syntax! $expression")
    }
  }

  private def quantified(expression: Quantified)(implicit ev: Arbitrary[Char]): Gen[String] = {
    expression match {
      case Optional(term) =>
        Gen.option(apply(term))
          .map(_.getOrElse(""))
      case ZeroOrMore(term) =>
        Gen.listOf(apply(term))
          .map(_.mkString)
      case OneOrMore(term) =>
        for {
          num  <- Gen.chooseNum(1, 100)
          list <- Gen.listOfN(num, apply(term))
        } yield list.mkString
      case Length(term, length) =>
        Gen.listOfN(length, apply(term))
          .map(_.mkString)
      case RangeFrom(term, min) =>
        for {
          num  <- Gen.chooseNum(min, 100)
          list <- Gen.listOfN(num, apply(term))
        } yield list.mkString
      case Range(term, min, max) =>
        for {
          num  <- Gen.chooseNum(min, max)
          list <- Gen.listOfN(num, apply(term))
        } yield list.mkString
    }
  }

  private def characterClass(expression: CharacterClass.Group.Term)(implicit ev: Arbitrary[Char]): Gen[Char] = {

    import CharacterClass._

    val digits: data.Group[Char]     = Inclusion((48.toChar to 57.toChar).toSet)
    val spaces: data.Group[Char]     = Inclusion(Set(' ', '\t', '\r', '\n'))
    val alphaUpper: data.Group[Char] = Inclusion((65.toChar to 90.toChar).toSet)
    val alphaLower: data.Group[Char] = Inclusion((97.toChar to 122.toChar).toSet)
    val alpha: data.Group[Char]      = alphaUpper ++ alphaLower
    val alphaNum: data.Group[Char]   = alpha ++ digits
    val word: data.Group[Char]       = alphaNum ++ Inclusion(Set('_')) //, '-'))
    val any: data.Group[Char]        = Exclusion(Set.empty)

    def toGroup(expression: CharacterClass.Group.Term): data.Group[Char] = {
      expression match {
        case CharacterClass.Literal(value) =>
          Inclusion(Set(value))
        case CharacterClass.Group(values @ _*) =>
          values.map(toGroup).foldLeft[data.Group[Char]](Inclusion(Set.empty)) {
            _ ++ _
          }
        case CharacterClass.Range(min, max) =>
          Inclusion((min to max).toSet)
        case Word =>
          word
        case Digit =>
          digits
        case Space =>
          spaces
        case Any =>
          any
        case Negated(characterClass) =>
          toGroup(characterClass).compliment
        case expression =>
          sys.error(s"Unsupported syntax! $expression")
      }
    }

    def toGen(group: data.Group[Char]): Gen[Char] = {
      group match {
        case Inclusion(values) =>
          Gen.oneOf(values.toSeq)
        case Exclusion(values) =>
          Arbitrary.arbitrary[Char]
            .suchThat(!values.contains(_))
      }
    }

    toGen(toGroup(expression))
  }
}
