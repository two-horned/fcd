package fcd

import scala.collection.mutable
import scala.util.DynamicVariable
import might.Attributed

trait DerivativeParsers extends Parsers { self: DerivedOps =>

  type Results[+R] = List[R]

  trait Parser[+R] extends Printable { p =>

    def results: Results[R]
    infix def consume(in: Elem): Parser[R]

    def accepts: Boolean
    def failed: Boolean

    infix def alt[U >: R](q: Parser[U]): Parser[U] = Alt(p, q)
    infix def and[U](q: Parser[U]): Parser[(R, U)] = And(p, q)
    infix def seq[U](q: Parser[U]): Parser[(R, U)] = new Seq(p, q)
    infix def flatMap[U](f: R => Parser[U]): Parser[U] = FlatMap(p, f)
    def done: Parser[R] = if accepts then Succeed(p.results) else fail

    def not: Parser[Unit] = Not(p)

    // the map family
    infix def mapResults[U](f: Results[R] => Results[U]): Parser[U] =
      MapResults(p, f)
    infix def map[U](f: R => U): Parser[U] = p.mapResults(_.map(f))
    infix def withResults[U](res: List[U]): Parser[U] = mapResults(_ => res)

    // for optimization of biased choice
    def prefix: Parser[Unit] = {
      if accepts then always
      else eat(p.consume(_).prefix)
    }
  }

  object Fail extends NullaryPrintable("∅") with Parser[Nothing] {
    override def results = Nil
    override def failed = true
    override def accepts = false
    override def consume(x: Elem) = this

    override def alt[U >: Nothing](q: Parser[U]) = q
    override def seq[U](q: Parser[U]) = this
    override def and[U](q: Parser[U]) = this
    override def map[U](f: Nothing => U) = this
    override def flatMap[U](g: Nothing => Parser[U]) = this
    override def mapResults[U](f: Results[Nothing] => Results[U]) = this
    override def done = this

    override def not = Always
    override def prefix = this
    override def toString: String = "∅"
  }

  object Always extends NullaryPrintable("∞") with Parser[Unit] {
    override def results = List(())
    override def failed = false
    override def accepts = true
    override def consume(x: Elem) = this
    override def not = Fail
    override def and[U](q: Parser[U]) = q map { ((), _) }

    // this is a valid optimization, however it almost never occurs.
    override def alt[U >: Unit](q: Parser[U]) = this
    override def toString = "always"
  }

  case class Succeed[R](ress: Results[R])
      extends NullaryPrintable("ε")
      with Parser[R] {
    override def results = ress
    override def failed = false
    override def accepts = true
    override def consume(x: Elem) = fail
    override def toString = s"ε($ress)"
    override def done = this
    override def mapResults[T](f: Results[R] => Results[T]) =
      Succeed(f(ress))
    override def seq[U](q: Parser[U]) = q mapResults { ress2 =>
      for {
        r <- ress
        r2 <- ress2
      } yield (r, r2)
    }
    override def flatMap[U](f: R => Parser[U]) =
      ress.iterator.map(f).reduce(_ alt _)
  }

  case class Accept(elem: Elem) extends Parser[Elem] {
    def results = Nil
    def failed = false
    def accepts = false
    def consume(x: Elem) = if x == elem then succeed(x) else fail

    lazy val name = "'" + escape(elem) + "'"
    def printNode = s"""$id [label="$name", shape=circle]"""
    private def escape(c: Elem): String =
      c.toString.replace("\\", "\\\\").replace("\"", "\\\"")
  }

  class AcceptIf(f: Elem => Boolean)
      extends NullaryPrintable("acceptIf")
      with Parser[Elem] {
    def results = Nil
    def failed = false
    def accepts = false
    def consume(x: Elem) = if f(x) then succeed(x) else fail
  }

  class Not[R](val p: Parser[R])
      extends UnaryPrintable("not", p)
      with Parser[Unit] {
    def results = if p.results.isEmpty then List(()) else Nil
    def failed = false // we never know, this is a conservative approx.
    def accepts = !p.accepts
    def consume(x: Elem) = p.consume(x).not
    override def not = p withResults List(())
    override def toString = s"not($p)"
  }

  class Alt[R, U >: R](val p: Parser[R], val q: Parser[U])
      extends BinaryPrintable("|", p, q)
      with Parser[U] {
    def results = List.from(p.results.iterator.concat(q.results).distinct)
    def failed = p.failed && q.failed
    def accepts = p.accepts || q.accepts
    def consume(x: Elem) = (p consume x) alt (q consume x)

    // optimization for not(p | map(always))
    override def not = (p.not and q.not) withResults List(())
    override def toString = s"($p | $q)"
  }

  class Seq[R, U](val p: Parser[R], val q: Parser[U])
      extends BinaryPrintable("~", p, q)
      with Parser[R ~ U] {

    def results = for x <- p.results; y <- q.results yield (x, y)
    // q.failed forces q, which might not terminate for grammars with
    // infinite many nonterminals, like:
    //   def foo(p) = 'a' ~ foo(p << 'a')
    // so we approximate similar to flatmap.
    def failed = p.failed // || q.failed
    def accepts = p.accepts && q.accepts
    def consume(x: Elem) = ((p consume x) seq q) alt (p.done seq (q consume x))
    override def toString = s"($p ~ $q)"

    // canonicalization rule (1) from PLDI 2016
    override def seq[T](r: Parser[T]): Parser[(R ~ U) ~ T] =
      (p seq (q seq r)) map { case (rr, (ru, rt)) => ((rr, ru), rt) }
  }

  class Done[R](val p: Parser[R])
      extends UnaryPrintable(s"done", p)
      with Parser[R] {
    def results = p.results
    def failed = p.failed
    def accepts = p.accepts
    def consume(x: Elem) = fail
    override def done = this
    override def toString = s"done($p)"
  }

  class MapResults[R, U](val p: Parser[R], f: Results[R] => Results[U])
      extends UnaryPrintable(s"mapResults", p)
      with Parser[U] {
    // preserve whether p actually has results (f might ignore its argument...)
    def results = if p.results.isEmpty then Nil else f(p.results).distinct
    def failed = p.failed
    def accepts = p.accepts
    def consume(x: Elem) = (p consume x) mapResults f
    override def mapResults[T](g: Results[U] => Results[T]) =
      p mapResults { res => g(f(res)) }
    override def map[T](g: U => T) = p mapResults { f(_) map g }
    override def done = p.done mapResults f

    // we can forget the results here.
    override def not = p.not
    override def toString = s"map($p)"

    // canonicalization rule (2) from PLDI 2016
    // allows for instance rewriting (always.map(f) & p) -> p.map(...f...)
    override def seq[S](q: Parser[S]) =
      (p seq q).mapResults(_.view.unzip match {
        case (us, ss) => f(List.from(us)) zip ss
      })
    override def and[S](q: Parser[S]) =
      (p and q).mapResults(_.view.unzip match {
        case (us, ss) => f(List.from(us)) zip ss
      })
  }

  class And[R, U](val p: Parser[R], val q: Parser[U])
      extends BinaryPrintable("&", p, q)
      with Parser[(R, U)] {
    def results = for x <- p.results; y <- q.results yield (x, y)
    def failed = p.failed || q.failed
    def accepts = p.accepts && q.accepts
    def consume(x: Elem) = (p consume x) and (q consume x)
    override def not = p.not alt q.not
    override def toString = s"($p & $q)"
  }

  class FlatMap[R, U](val p: Parser[R], f: R => Parser[U])
      extends UnaryPrintable("flatMap", p)
      with Parser[U] {
    def results = List.from(p.results.iterator.flatMap(f(_).results).distinct)
    def accepts = !results.isEmpty
    def failed = p.failed // that's the best we know

    def consume(x: Elem) = {
      val next = (p consume x) flatMap f
      val qss = p.results.iterator.map(f(_) consume x)
      qss.foldLeft(next)(_ alt _)
    }
    override def toString = "flatMap"
  }

  class Nonterminal[R](_p: => Parser[R]) extends Parser[R] {
    lazy val p = _p

    def accepts: Boolean = propertiesFix.nullable.value
    def failed: Boolean = propertiesFix.empty.value
    def results: Results[R] = resultsFix.results.value

    // This separation into two fixed points is essential to
    // prevent excessive recomputation.
    private object propertiesFix extends Attributed {
      object nullable extends Attribute[Boolean](false, _ || _, implies)
      object empty extends Attribute[Boolean](true, _ && _, follows)

      empty := p.failed
      nullable := p.accepts

      override protected def updateAttributes() = {
        empty.update()
        nullable.update()
      }
    }

    private object resultsFix extends Attributed {
      object results
          extends Attribute[List[R]](
            Nil,
            (nw, ol) => (nw ++ ol).distinct,
            (nw, ol) => nw.toSet.subsetOf(ol.toSet)
          )

      results := p.results

      override protected def updateAttributes() = results.update()
    }

    private val cache: mutable.HashMap[Elem, Parser[R]] = mutable.HashMap()
    // Wrapping in `nonterminal` is cecessary for left-recursive
    // grammars and for grammars like "DerivativeParsers / preprocessor"
    // that recursively derive. Optimizing the nonterminal node away causes
    // divergence on these grammars. Worse, in the latter case
    // forcing `next` will already cause divergence.
    override def consume(x: Elem) = cache.getOrElseUpdate(
      x,
      if p.failed then fail
      else nonterminal(p consume x)
    )

    def named(str: => String): this.type = {
      name = str
      this
    }
    var name = "nt"
    private val rec = DynamicVariable[Boolean](false)
    override def toString =
      if rec.value then s"nt(${System.identityHashCode(this)})"
      else rec.withValue(true) { s"nt($p)" }

    def printNode =
      if rec.value then ""
      else
        rec.withValue(true) {
          s"""  ${id} [shape=none, fillcolor="#dedede", style=filled, fontsize=8, fontname=mono, label=<$table>];
             |  ${id}:s -> ${p.id}
             |${p.printNode}""".stripMargin('|')
        }
  }

  // combinators without parser arguments
  val fail: Parser[Nothing] = Fail
  val always: Parser[Unit] = Always
  def succeed[R](res: R): Parser[R] = Succeed(List(res))
  def acceptIf(cond: Elem => Boolean): Parser[Elem] = AcceptIf(cond)

  // combinators with parser arguments
  def not[R](p: Parser[R]): Parser[Unit] = p.not
  def map[R, U](p: Parser[R], f: R => U) = p map f
  def flatMap[R, U](p: Parser[R], f: R => Parser[U]) = p flatMap f

  def alt[R, U >: R](p: Parser[R], q: Parser[U]) = p alt q
  def seq[R, U](p: Parser[R], q: Parser[U]) = p seq q
  def and[R, U](p: Parser[R], q: Parser[U]) = p and q

  def feed[R](in: Elem, p: => Parser[R]) = p consume in

  def results[R](p: Parser[R]) = p.results

  def done[T](p: Parser[T]): Parser[T] = p.done

  override def nonterminal[R](_p: => Parser[R]): Nonterminal[R] =
    Nonterminal(_p)
  def nonterminal[R](name: String)(_p: => Parser[R]): Nonterminal[R] =
    Nonterminal(_p).named(name)

  def feed[R](p: Parser[R], in: Elem) = p.consume(in)
  def parse[R](p: Parser[R], in: Iterable[Elem]): Results[R] =
    feedAll(p, in).results

  // for testing
  override def isSuccess[R](p: Parser[R]): Boolean = p.accepts
  override def accept(t: Elem): Parser[Elem] = Accept(t)

  // optimization: Once p accepts, p as a prefix will always accept.
  // often used to implement biased choice: (not(prefix(p)) &> q
  override def prefix: Parser[Any] => Parser[Unit] = _.prefix
}

object DerivativeParsers extends RichParsers with DerivativeParsers
