package improved

object Main extends App {
  println("Demo on Parser Combinators:")
  println("===========================")

  object XParser extends SimpleResults {
    type Input = String
    val acceptX: Input => Result[Char] = { (in: String) =>
      if (in.charAt(0) == 'x')
        // success. so consume 'x' and
        // return the rest not consumed inputs
        Success('x', in.substring(1))
      else
        // Failure. So return the whole
        // not consumed original input 
        Failure("expected 'x'", in)
    }

    /*
     * Sample - parsing and consume only 'x' 
     * and returns the rest of inputs "abc"
     */
    val res = acceptX("xabc")

  }

  object OXParser extends StringParsers {
    /*
     * This fluent expression is equivalent to p.~(q)
     */
    //def xyz = accept('x') ~ accept('y')  ~ accept('z')
    //def xyz = log(accept('x'))("one") ~ log(accept('y'))("two")  ~ log(accept('z'))("three")
    //def xyz = 'x' ~ 'y' ~ 'z'
    //def xyz = log('x')("one") ~ log('y')("two")  ~ log('z')("three")
    def XorY = log('x')("consuming X") ~ log('y')("consuming Y")
  }
  //println("Output OX parsing: " + OXParser.xyz("xy3"))
  println("Output OX parsing: " + OXParser.XorY("xy"))
}