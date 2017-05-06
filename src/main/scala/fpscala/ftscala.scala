package fpscala

/**
  * Created by wudong on 05/05/2017.
  */
object ftscala extends  App{

  def fib(n: Int) : Int = {

    def loop(curIndex: Int, preValue: Int, curValue: Int): Int ={
      if (curIndex==0) curValue
      else loop(curIndex-1, curValue, curValue + preValue)
    }

    loop(n, 0, 1)
  }


  def findFist(ss: Vector[String], key: String) : Int = {
    def loop(curIndex: Int, ss: Vector[String], key: String): Int ={
      if (ss(curIndex) ==key) curIndex
      else{
        loop(curIndex+1, ss, key)
      }
    }
    loop(0, ss, key)
  }

  def isSorted[A](as: Vector[A], ordered: (A,A) => Boolean): Boolean = {

    def loop(curIdx: Int): Boolean = {
      if (curIdx>=as.length-1) true
      else if (!ordered(as(curIdx), as(curIdx+1))) false
      else loop(curIdx+1)
    }

    loop(0)
  }


  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil=> Nil
      case head::tail=> if (f(head)) dropWhile(tail, f) else l
    }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case head :: Nil => Nil
      case head :: tail =>  head :: init(tail)
    }
  }

  print(fib(1), fib(2), fib(3), fib(4), fib(5))


}
