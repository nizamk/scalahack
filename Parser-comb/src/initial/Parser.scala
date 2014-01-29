package initial

trait SimpleResults {
  type Input

  trait Result[+T] {
    def next: Input
  }

  case class Success[+T](result: T, next: Input) extends Result[T]
  case class Failure(msg: String, next: Input) extends Result[Nothing]
}

trait SimpleParsers extends SimpleResults {
  trait Parser[+T] extends (Input => Result[T]) {

    // Abstract apply function for 
    // consuming an input character
    def apply(in: Input): Result[T]

    // Alternation implementation
    def |[U >: T](p: => Parser[U]): Parser[U] =
      new Parser[U] {
        def apply(in: Input) =
          Parser.this(in) match {
            case Failure(_, _) => p(in)
            case Success(x, n) => Success(x, n)
          }
      }

    // Sequence implementation
    def ~[U](p: => Parser[U]): Parser[Pair[T, U]] =
      new Parser[Pair[T, U]] {
        def apply(in: Input) =

          // evaluate the parser where 
          // the ~ method is called
          Parser.this(in) match {
            case Success(x, next) => p(next) match {
              case Success(x2, next2) => Success((x, x2), next2)
              case Failure(m, n) => Failure(m, n)
            }
            case Failure(m, n) => Failure(m, n)
          }
      }
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
