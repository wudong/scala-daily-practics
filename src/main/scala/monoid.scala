trait Monoid[A] {
  def id: A
  def op(x: A, y:A):A
}

object Monoid{

  implicit val stringMonoid = new Monoid[String]{
    def id = ""
    def op(x: String , y : String) = x + y
  }

  //Given a Monoid of A, we can have a Monoid for Option[A]
  implicit def optionMonoid[A](implicit am: Monoid[A]): Monoid[Option[A]] =
    new Monoid[Option[A]] {
      def id = None
      def op(x: Option[A], y: Option[A]) : Option[A] = (x, y) match{
        case (x, None) => x
        case (None, y) => y
        case (Some(x), Some(y)) => Some(am.op(x, y))
      }
    }

  //Given a monoid for B, we can have a monoid for functions return B.
  implicit def functionMonoid[A, B](implicit bm: Monoid[B]): Monoid[A => B] =
    new Monoid[(A) => B] {
      override def id = A=>bm.id
      override def op(x: (A) => B, y: (A) => B) = {
        a => bm.op(x(a), y(a))
      }
    }

  //can use a monoid to collapse a bunch of values.
  implicit def fold[A](la: List[A])(implicit am: Monoid[A]) : A =
    la.foldLeft(am.id)(am.op)


  def main(args: Array[String]): Unit = {

    // we'll start with some functions
    val fizz: Int => Option[String] = x => if(x % 3 == 0) Some("fizz") else None
    val buzz: Int => Option[String] = x => if(x % 5 == 0) Some("buzz") else None
    val bazz: Int => Option[String] = x => if(x % 7 == 0) Some("bazz") else None

    val funcs = List(fizz,buzz,bazz)

    // we can combine our functions, this works because we can find an
    // option monoid for strings, since we have a monoid for strings,
    // then we can find a monoid for Int => Option[String] since we now
    // have a monoid for Option[String]
    val fizzbuzzbazz = fold(funcs)

    // handle the Nones
    val fbbOrInt: Int => String = { i =>
      (fizzbuzzbazz(i) getOrElse i.toString) + ","
    }

    // map our function on a list
    val strings: List[String] = (1 until 100).toList map fbbOrInt

    // use fold to collapse our strings using the string monoid
    println(fold((strings)))


  }
}

