
object projecteuler {

  /*
   If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
   Find the sum of all the multiples of 3 or 5 below 1000.

   Result : 233168
  */
  def problem_1(): Int = {
    val range = 3 until 1000
    range.filter(x => x % 3 == 0 || x % 5 == 0).sum
  }

  /*
    Each new term in the Fibonacci sequence is generated by adding the previous two terms. By starting with 1 and 2, the first 10 terms will be:
    1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
    By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.

    Result:
  */
  def problem_2(): BigInt = {
    //one way using zip.
    lazy val fib: Stream[BigInt] = 0 #:: 1 #:: fib.zip(fib.tail).map(p => p._1 + p._2)
    //the other way using scanLeft.
    lazy val fib2: Stream[BigInt] = 0 #:: fib2.scanLeft(BigInt(1))(_ + _)

    fib.takeWhile(_ < 4000000).filter(_ % 2 == 0).sum
  }

  /*
    The prime factors of 13195 are 5, 7, 13 and 29.
    What is the largest prime factor of the number 600851475143 ?
   */
  def problem_3(): Int = {
    0
  }

  /*
  A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
  Find the largest palindrome made from the product of two 3-digit numbers.
   */
  def problem_4(): Int = {

    def isPalindromic(number: Int): Boolean = {
      val str = number.toString
      str == str.reverse
    }

    (for (i <- 100 to 999; j <- 100 to 999; z = i * j if isPalindromic(z)) yield z).max

  }

  /*
    2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
    What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
   */
  def problem_5(): Int = {

    type MAP = scala.collection.mutable.Map[Int, Int]

    val allPrimeNumber = Array(2, 3, 5, 7, 11, 13, 17, 19)

    def produce(number: Int): MAP = {
      var reminder = number
      val result = collection.mutable.Map[Int, Int]()

      while (reminder != 1) {
        allPrimeNumber.filter(x => (x <= reminder && reminder % x == 0)).foreach((p) => {
          reminder = reminder / p
          val value = result.getOrElse(p, 0) + 1
          result.put(p, value)
        })
      }
      result
    }

    //NOTE the usage of flatMap here.
    val tt: Seq[(Int, Int)] = (2 to 20).flatMap(produce)

    val result = tt.groupBy((x) => x._1).map {
      case (key, seq) => (key, seq.foldLeft(0)((b, pair) => b max pair._2))
    }

    //    val result = collection.mutable.Map[Int, Int]()
    //    tt.foreach( x=> {
    //      val currentV = result.getOrElse(x._1, 0)
    //      result.put(x._1, Math.max( currentV, x._2 ))
    //    })

    result.map((x) => Math.pow(x._1, x._2)).reduce(_ * _).toInt
  }

  /*
  The sum of the squares of the first ten natural numbers is,

  1^2 + 2^2 + ... + 10^2 = 385
  The square of the sum of the first ten natural numbers is,

  (1 + 2 + ... + 10)^2 = 55^2 = 3025
  Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.

  Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
  */
  def problem_6(): Int ={
    val naturalNums = (1 to 100)
    val sumOfSquare = naturalNums.map(Math.pow(_,2).toInt).sum
    val squareOfSum = Math.pow(naturalNums.sum,2).toInt
    squareOfSum - sumOfSquare
  }

  /*
  By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
   What is the 10 001st prime number?
   */
  def problem_7() : Int = {
    //NOTE. how to define a Prime stream quickly.
    lazy val ps: Stream[Int] = 2 #:: Stream.from(3).filter( i => { ps.takeWhile( j=> j*j <=i).forall( i % _ > 0) })
    ps(10000)
  }

  /*
  A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
  a^2 + b^2 = c^2

  There exists exactly one Pythagorean triplet for which a + b + c = 1000.
  Find the product abc.
   */
  def problem_9() :Int ={
    val prod = (for (a <- 1 to 999;
                     b <- a to 999;
                     c  = 1000 - a - b )
      yield (a,b,c) ).filter({ case (a,b,c)=> a*a + b*b == c*c}).map{case (a,b,c)=>a * b* c}
    prod.head
   }

  def problem_10 : Int = {
    lazy val ps: Stream[Int] = 2 #:: Stream.from(3).filter( x=>{
      ps.takeWhile( i => i*i <=x).forall( x % _ > 0)
    } )

    ps.takeWhile( _ < 2000000).sum
  }

  def problem_11() : Int = {
//    val str = "08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08\n49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00\n81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65\n52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91\n22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80\n24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50\n32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70\n67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21\n24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72\n21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95\n78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92\n16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57\n86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58\n19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40\n04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66\n88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69\n04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36\n20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16\n20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54\n01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48\n"
//    val matrixInSeq = str.split("\\s").map( _.toInt )
//    val size = 20
//    assert (matrixInSeq.length == size * size)
//
//    val rows: _root_.scala.collection.Seq[_root_.scala.Array[Int]] = matrixInSeq.sliding(size, size).toSeq
//    val columns: _root_.scala.collection.immutable.IndexedSeq[_root_.scala.collection.Seq[Int]] = 0 until size map { case i=>rows.map(r=> r(i))}
//
//    val range =(0 until size)
//    val allZero = Seq.fill(size)(0)
//
//    val dialogPairs = (range zip allZero) ++ (allZero zip range)
//
//    val allDialogPairs = dialogPairs.map((pair) => {
//      val row = pair._1; val col = pair._2;
//      (row until size) zip (col until size).map {case (r,c)=> rows(r)(c)}
//
//    })
//
//    //TODO
    0

  }

}
 object runner {

  def invokeProblem[T](funcName: String, fu : => T, expected: Option[T]): Unit ={
    val result = fu
    println(s"${funcName}: ${result}" + expected.map(x=> s", expected result ${x}  \t--> " + (if (x==result) "SUCCESS" else "FAIL" )).getOrElse(""))
  }


  def main(args: Array[String]): Unit = {
    Array(
//      ("Problem_1", projecteuler.problem_1, 233168),
//      ("Problem_2", projecteuler.problem_2, 4613732),
//      ("Problem_4", projecteuler.problem_4, 906609),
//      ("Problem_5", projecteuler.problem_5, 232792560),
//      ("Problem_6", projecteuler.problem_6, 25164150),
        ("Problem_7", projecteuler.problem_7, 104743),
      ("Problem_9", projecteuler.problem_9, 31875000),
      ("Problem_10", projecteuler.problem_10, 1179908154)
    ).foreach(
      x=>invokeProblem(x._1, x._2, Option(x._3))
    )
  }
}
