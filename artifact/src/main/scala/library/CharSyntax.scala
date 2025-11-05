package fcd

trait CharSyntax { self: Parsers & DerivedOps & Syntax =>
  type Elem = Char

  def notChar(c: Char): Parser[Char] = acceptIf(_ != c)

  val char = any
  val letter = acceptIf(_.isLetter)
  val upper = acceptIf(_.isUpper)
  val lower = acceptIf(_.isLower)
  val whitespace = acceptIf(_.isWhitespace)
  val digit = acceptIf(_.isDigit)
  val letterOrDigit = acceptIf(_.isLetterOrDigit)
  val space = acceptIf(_.isSpaceChar)
  val spaces = many(space)
  val newline = acceptIf(_ == '\n')

  def charRange(from: Char, to: Char) = acceptIf { c => c >= from && c <= to }

  val asciiLetter = charRange('a', 'z') | charRange('A', 'Z')

  def string(s: String): Parser[String] = acceptSeq(s) ^^ (_.mkString)

  sealed trait Stringable[T] { def apply: T => String }

  given Stringable[Char] with { def apply = _.toString }
  given Stringable[List[Char]] with { def apply = _.mkString }
  given Stringable[String] with { def apply = identity }
  given stringList: Stringable[List[String]] with { def apply = _.mkString }
  given [T, U](using st: Stringable[T], su: Stringable[U]): Stringable[(T, U)]
  with {
    def apply = { case (l, r) => st.apply(l) ++ su.apply(r) }
  }

  given Conversion[String, Parser[String]] = string
  given Conversion[List[Char], String] = _.mkString

  given [T](using st: Stringable[T]): Conversion[Parser[T], Parser[String]] =
    p => p ^^ st.apply

  given Conversion[Char, Parser[Char]] = accept

  def noneOf(s: String): Parser[Char] = acceptIf(t => !(s contains t))
}
