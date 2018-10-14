package wolfendale.scalacheck.regexp

import org.scalacheck.{Arbitrary, Gen}
import ast._

object ASTProcessor {

  // TODO negation which doesn't use `suchThat`?
  private def negated(re: Negated)(implicit ev: Arbitrary[Char]): Gen[String] = {

    val arbitraryString: Gen[String] =
      Arbitrary.arbitrary[Char].map(_.toString)

    def termToString(term: CharacterClass.Term): String = {
      term match {
        case CharacterClass.Literal(str) =>
          str
        case CharacterClass.CharRange(min, max) =>
          s"$min-$max"
        case CharacterClass.DigitRange(min, max) =>
          s"$min-$max"
        case CharacterClass.DigitChar =>
          "\\d"
        case CharacterClass.WordChar =>
          "\\w"
        case CharacterClass.SpaceChar =>
          "\\s"
        case _ =>
          ""
      }
    }

    re match {
      case Negated(WordChar) =>
        arbitraryString.suchThat(_.matches("\\W"))
      case Negated(DigitChar) =>
        arbitraryString.suchThat(_.matches("\\D"))
      case Negated(SpaceChar) =>
        arbitraryString.suchThat(_.matches("\\S"))
      case Negated(WordBoundary) =>
        Gen.const("")
      case Negated(CharacterClass(terms @ _*)) =>
        arbitraryString.suchThat(_.matches(s"[^${terms.map(termToString).mkString("")}]"))
        // TODO fix AST so that this isn't a valid construction
      case _ =>
        sys.error("invalid negated term")
    }
  }

  def apply(re: RegularExpression)(implicit ev: Arbitrary[Char]): Gen[String] =
    app(re).apply(Map.empty).map(_._1)

  // TODO tailrec optimisation
  private def app(re: RegularExpression)(implicit ev: Arbitrary[Char]): Map[Int, String] => Gen[(String, Map[Int, String])] = {

    re match {
      case Literal(str) =>
        refs =>
          literal(str).map(_ -> refs)
      case WordChar =>
        refs =>
          wordChar.map(_ -> refs)
      case SpaceChar =>
        refs =>
          spaceChar.map(_ -> refs)
      case DigitChar =>
        refs =>
          digitChar.map(_ -> refs)
      case AnyChar =>
        refs =>
          Arbitrary.arbitrary[Char].map(_.toString -> refs)
      case Group(inner, rest) =>
        refs =>
          for {
            (x , newRefs)    <- app(inner).apply(refs)
            (xs, newNewRefs) <- rest
              .map(z => app(z).apply(newRefs + (newRefs.keys.size -> x)))
              .getOrElse(Gen.const("", newRefs))
          } yield (x + xs, newNewRefs)
      case NonCapturingGroup(inner) =>
        app(inner)
      case Or(left, right) =>
        refs =>
          for {
            l <- app(left).apply(refs)
            r <- app(right).apply(refs)
            x <- Gen.oneOf(l -> refs, r -> refs)
          } yield x._1
      case And(left, right) =>
        refs =>
          for {
            (l, newRefs) <- app(left).apply(refs)
            (r, newRefs2) <- app(right).apply(refs)
          } yield (l + r, newRefs ++ newRefs2)
      case Optional(inner) =>
        refs =>
          app(inner).apply(refs).flatMap {
            case (a, newRefs) =>
              Gen.oneOf(
                a  -> newRefs,
                "" -> newRefs
              )
          }
      case OneOrMore(inner) =>
        refs =>
          app(inner).apply(refs).flatMap {
            case (a, newRefs) =>
              Gen.nonEmptyListOf(a).map(_.mkString("") -> newRefs)
          }
      case ZeroOrMore(inner) =>
        refs =>
          app(inner).apply(refs).flatMap {
            case (a, newRefs) =>
              Gen.listOf(a).map(_.mkString("") -> newRefs)
          }
      case RangeFrom(inner, min) =>
        refs =>
          for {
            length <- Gen.choose(min, 100)
            list <- Gen.listOfN(length, app(inner).apply(refs))
          } yield list.map(_._1).mkString("") -> refs
      case Range(inner, min, max) =>
        refs =>
          for {
            length <- Gen.choose(min, max)
            list <- Gen.listOfN(length, app(inner).apply(refs))
          } yield list.map(_._1).mkString("") -> refs
      case Length(inner, length) =>
        refs =>
          for {
            list <- Gen.listOfN(length, app(inner).apply(refs))
          } yield list.map(_._1).mkString("") -> refs
      case CharacterClass(terms@_*) =>
        refs =>
          processClass(terms).map(_ -> refs)
      case term: Negated =>
        refs =>
          negated(term).map(_ -> refs)
      case Substitution(ref) =>
        refs =>
          Gen.const(refs.apply(ref)).map(_ -> refs)
      case WordBoundary | BOS | EOS =>
        refs =>
          Gen.const("").map(_ -> refs)
    }
  }

  private def processClass(terms: Seq[CharacterClass.Term]): Gen[String] = {

    val gens = terms.toList.map {
      case CharacterClass.Literal(str) =>
        literal(str)
      case CharacterClass.DigitRange(min, max) =>
        Gen.choose(min, max).map(_.toString)
      case CharacterClass.CharRange(min, max) =>
        Gen.choose(min, max).map(_.toString)
      case CharacterClass.WordChar =>
        wordChar
      case CharacterClass.SpaceChar =>
        spaceChar
      case CharacterClass.DigitChar =>
        digitChar
      case _ =>
        Gen.const("")
    }

    gens match {
      case a :: Nil => a
      case a :: b :: xs =>
        Gen.oneOf(a, b, xs: _*)
      case _ =>
        Gen.const("")
    }
  }

  private val wordChar: Gen[String] =
    Gen.oneOf(Gen.alphaNumChar, Gen.const('_')).map(_.toString)

  private val spaceChar: Gen[String] = {
    // should this contain other characters?
    Gen.oneOf(" ", "\t")
  }

  private val digitChar: Gen[String] =
    Gen.numChar.map(_.toString)

  private def literal(str: String): Gen[String] =
    Gen.const(str)
}
