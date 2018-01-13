package wolfendale.scalacheck.regexp

import org.scalacheck.{Arbitrary, Gen}

object RegexGen {

  def from(str: String)(implicit ev: Arbitrary[Char]): Gen[String] = {
    ASTProcessor(GenParser.parse(str))
  }
}
