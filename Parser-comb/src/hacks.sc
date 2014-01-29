object hacks {
  val list1 = List(1, 2, 3)                       //> list1  : List[Int] = List(1, 2, 3)
  val list2 = List("a", "b", "c")                 //> list2  : List[String] = List(a, b, c)
  val list3 = List("A", "B", "C")                 //> list3  : List[String] = List(A, B, C)

  list1 map (y => y + "a")                        //> res0: List[String] = List(1a, 2a, 3a)
  list1 map (y => List(y, 3, 4))                  //> res1: List[List[Int]] = List(List(1, 3, 4), List(2, 3, 4), List(3, 3, 4))
  list1 flatMap (y => List(y, 3, 4))              //> res2: List[Int] = List(1, 3, 4, 2, 3, 4, 3, 3, 4)
  for (y <- list1) yield List(y, 3, 4)            //> res3: List[List[Int]] = List(List(1, 3, 4), List(2, 3, 4), List(3, 3, 4))

  for (x <- list1; y <- list2) yield x + y        //> res4: List[String] = List(1a, 1b, 1c, 2a, 2b, 2c, 3a, 3b, 3c)
  list1 flatMap (x => list2 map (y => x + y))     //> res5: List[String] = List(1a, 1b, 1c, 2a, 2b, 2c, 3a, 3b, 3c)

  for (x <- list1; y <- list2; z <- list3) yield x + y + z
                                                  //> res6: List[String] = List(1aA, 1aB, 1aC, 1bA, 1bB, 1bC, 1cA, 1cB, 1cC, 2aA, 
                                                  //| 2aB, 2aC, 2bA, 2bB, 2bC, 2cA, 2cB, 2cC, 3aA, 3aB, 3aC, 3bA, 3bB, 3bC, 3cA, 3
                                                  //| cB, 3cC)

  list1 flatMap (x =>
    list2 flatMap (y =>
      list3 map (z => x + y + z)))                //> res7: List[String] = List(1aA, 1aB, 1aC, 1bA, 1bB, 1bC, 1cA, 1cB, 1cC, 2aA, 
                                                  //| 2aB, 2aC, 2bA, 2bB, 2bC, 2cA, 2cB, 2cC, 3aA, 3aB, 3aC, 3bA, 3bB, 3bC, 3cA, 3
                                                  //| cB, 3cC)

 trait SimpleResults {
    type Input

    trait Result[+T] {
      def next: Input
      def map[U](f: T => U): Result[U]
      def flatMapWithNext[U](f: T => Input => Result[U]): Result[U]
      def append[U >: T](alt: => Result[U]): Result[U]
    }

    case class Success[+T](result: T, next: Input) extends Result[T] {
      // OK
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
  
  object xp extends SimpleResults {
  	 type Input = String
     val x = Success('x', "xy")
     x map(x=>'b')
  }

}