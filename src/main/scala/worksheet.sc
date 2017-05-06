import math.Ordering

def merge[T](xs: List[T], ys: List[T])(implicit ord: Ordering[T]) : List[T] =
  (xs, ys) match {
    case (Nil, ys) => ys
    case (xs, Nil) => xs
    case (xh :: xt, yh:: yt) => if ( ord.lt(xh,yh) ) xh :: merge(xt, ys)
    else yh :: merge(xs, yt)
  }

merge(List(1,2,6), List(3,4,5))

val ftime2 = (x: Int)=> 2*x
val fsquare = (x: Int)=> x*x

val composed = ftime2.compose(fsquare)
val andThen = ftime2.andThen(fsquare)

composed(4)
andThen(4)


