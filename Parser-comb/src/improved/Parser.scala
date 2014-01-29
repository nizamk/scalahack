package improved

/* 
 * An implementation of a Parser Combinators
 * 
 * SimpleResults represents a result (Success/Failure) as result of parsing.
 * Success would contain consumed input (result: T) and next available input 
 * yet to be consumed (next: Input)
 */
trait SimpleResults {
  type Input

  trait Result[+T] {
    def next: Input
    def map[U](f: T => U): Result[U]
    def flatMapWithNext[U](f: T => Input => Result[U]): Result[U]
    def append[U >: T](alt: => Result[U]): Result[U]
  }

  case class Success[+T](result: T, next: Input) extends Result[T] {
    def map[U](f: T => U) = Success(f(result), next)
    def flatMapWithNext[U](f: T => Input => Result[U]) = f(result)(next)
    def append[U >: T](alt: => Result[U]) = this
  }

  case class Failure(msg: String, next: Input) extends Result[Nothing] {
    def map[U](f: Nothing => U) = this
    def flatMapWithNext[U](f: Nothing => Input => Result[U]) = this
    def append[U](alt: => Result[U]) = alt
  }
}

trait SimpleParsers extends SimpleResults {

  // Instantiate the parser
  def Parser[T](f: Input => Result[T]) =
    new Parser[T] { def apply(in: Input) = f(in) }

  abstract class Parser[+T] extends (Input => Result[T]) {
    /**
     * Implement map allows for for-expression with single generator
     * e.g for( x <- expr1 ) yield expr2 translates into expr1.map(x=>expr2 )
     *
     * Note:
     * The expression "Parser.this" is necessary. If "Parser" would have
     * been missing from "Parser.this", then "this" alone would refer to
     * the parser object being created as a result of this call which would
     * have resulted in recursive calls...
     *
     * The intended result is to use "Parser.this" which would refer to
     * the Parser object the map method being called.
     */
    def map[U](f: T => U): Parser[U] =
      Parser { in => Parser.this(in) map (f) }

    /**
     * Implement flatmap for chaining parsers
     *
     * e.g for (x <-expr1;y <- expr2;) yield expr3 translates into
     * 	   expr1.flatMap(x => for (y <expr2) yield expr3) translates into
     *     expr1.flatMap(x => expr2.map(y => expr3))
     */
    def flatMap[U](f: T => Parser[U]): Parser[U] =
      Parser { in => Parser.this(in) flatMapWithNext (f) }

    def |[U >: T](p: => Parser[U]): Parser[U] =
      Parser { in => Parser.this(in) append (p(in)) }

    def ~[U](p: => Parser[U]): Parser[Pair[T, U]] =
      /*
       * Parser expansion of below For-comprehension:
       * 
       * this.flatMap{a => p.map{b => (a, b)}}
       * 
       * means perform parser "this" and binds its 
       * result "a" in the next computation which perform parser "p"
       * and maps its result to "b" to the pair (a,b)
       * 
       * Note: flatMap == "binds"
       */
      for (a <- this; b <- p) yield (a, b)
  }
}

/*
   * Defining String Parser
   */
trait StringParsers extends SimpleParsers {
  type Input = String
  private val EOI = 0.toChar

  implicit def accept(expected: Char) = new Parser[Char] {
    def apply(in: String) = {
      if (in == "") {
        if (expected == EOI) Success(expected, "")
        else Failure("No more input", in)
      } else if (in.charAt(0) == expected) {
        Success(expected, in.substring(1))
      } else {
        val y = in.charAt(0)
        Failure(s"expected '$expected' but was '$y'", in)
      }
    }
  }

  def log[T](p: => Parser[T])(name: String) = new Parser[T] {
    def apply(in: Input): Result[T] = {
      println("Trying " + name + " at ’" + in + "’")
      val r = p(in)
      println(name + " " + r)
      r
    }
  }

  def eoi = accept(EOI)
}