package fcd
package test

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.BeMatcher

trait CustomMatchers { self: AnyFunSpec & Matchers =>

  // Due to initialization problems we have to use this pattern
  // of def and lazy val.
  //
  // Override _parsers in concrete tests suites with the
  // appropriate parser implementation.
  type Parsers = RichParsers
  def _parsers: RichParsers
  lazy val parsers = _parsers
  import parsers.{Results, isSuccess, Parser, accepts, Elem}

  extension [T](p: => Parser[T]) {
    def shouldParse(s: Iterable[Elem], tags: Tag*) =
      it(s"""should parse "$s" """, tags*) {
        accepts(p, s) `shouldBe` true
      }
    def shouldNotParse(s: Iterable[Elem], tags: Tag*) =
      it(s"""should not parse "$s" """, tags*) {
        accepts(p, s) `shouldBe` false
      }
    // for unambiguous parses
    def shouldParseWith(s: Iterable[Elem], result: T) =
      it(s"""should parse "$s" with correct result""") {
        parse(p, s) `shouldBe` List(result)
      }
  }

  class SuccessMatcher extends BeMatcher[Parser[?]] {
    def apply(left: Parser[?]) =
      MatchResult(
        isSuccess(left),
        left.toString + " was not successful",
        left.toString + " was successful"
      )
  }
  lazy val successful = new SuccessMatcher
  lazy val failure = not(successful)
}
