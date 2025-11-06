package fcd
package test

import scala.language.implicitConversions
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

trait NegationTests extends CustomMatchers {
  self: AnyFunSpec & Matchers & RichParsers =>

  describe("parser \"not(aa)\"") {
    val p = not("aa")
    p `shouldParse` "a"
    p `shouldNotParse` "aa"
    p `shouldParse` "aac"
    p `shouldParse` "abc"
  }

  describe("parser \"not(aa) & lower*\"") {
    val p = not("aa") & many(lower)
    p `shouldParse` "a"
    p `shouldParse` "bc"
    p `shouldParse` "ab"
    p `shouldNotParse` "aa"
    p `shouldParse` "abc"
    p `shouldParse` "aac"
    p `shouldParse` "aacdd"
  }

  describe("parser \"not(aa ~ .*) & lower*\"") {
    val p = not("aa" ~ many(any)) & many(lower)
    p `shouldParse` "a"
    p `shouldParse` "bc"
    p `shouldParse` "ab"
    p `shouldNotParse` "aa"
    p `shouldParse` "abc"
    p `shouldNotParse` "aac"
    p `shouldNotParse` "aacadasdasdasd"
  }

  describe("parser \"not(.* ~ abc ~ .*)\"") {
    val p = not(many(any) ~ "abc" ~ many(any))
    p `shouldParse` ""
    p `shouldParse` "xx"
    p `shouldParse` "xxabxx"
    p `shouldNotParse` "xxabcxxx"
    p `shouldNotParse` "xxabc"
    p `shouldNotParse` "abcxxx"
  }

  describe("parser \"not((baaa | ba) ~ aa ~ .*) & lower*\"") {
    val p = not(("baaa" | "ba") ~ "aa" ~ many(any)) & many(lower)
    p `shouldNotParse` "baaa"
    p `shouldNotParse` "baaaxx"
    p `shouldParse` ""
    p `shouldParse` "baba"
    p `shouldParse` "baacxx"
  }
}
