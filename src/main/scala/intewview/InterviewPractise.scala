package intewview

/**
  * Created by wudong on 03/04/2017.
  */
object InterviewPractise {

  //reverse a single-linked list.
  //@tailrec
  def reverse[T](xs: List[T]) : List[T] = xs match {
    case Nil => Nil
    case x :: tail => reverse(tail) ++ List(x);
  }

  def init[T](xs: List[T]) : List[T] = xs match {
    case List() => throw new Error("init of empty list")
    case List(x) => Nil
    case x :: tail => x :: init(tail)
  }

  def concat[T](xs:List[T], ys: List[T]) : List[T] =  xs match {
    case Nil => ys
    case y :: tail => y :: concat(tail, ys)
  }

  def removeAt[T](xs: List[T], n: Int) = {
    def remove(head: List[T], xs: List[T], n: Int, i: Int) : List[T] = {
      if (xs == Nil) head
      else {
        if (n == i) head ++ xs.tail
        else {
          remove(head ++ List(xs.head), xs.tail, n, i + 1)
        }
      }
    }
    remove(List(), xs, n, 0)
  }

  def merge[T](xs: List[T], ys: List[T])(lt: (T, T)=> Boolean) : List[T] =
    (xs, ys) match {
      case (Nil, ys) => ys
      case (xs, Nil) => xs
      case (xh :: xt, yh:: yt) => if ( lt(xh,yh) ) xh :: merge(xt, ys)(lt) else yh :: merge(xs, yt)(lt)
    }


  def main(args: Array[String]): Unit = {
    //assert(reverse(List(0,1,2,3)) == List(3,2,1,0))
  }

}
