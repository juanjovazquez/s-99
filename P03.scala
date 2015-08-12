object P03 {

  def nth[A](i: Int, l: List[A]): A = (i, l) match {
  	case (0, x :: _) => x
  	case (n, _ :: xs) => nth(n - 1, xs)
  	case (_, Nil) => throw new IndexOutOfBoundsException
  }

  def nthBuiltIn[A](i: Int, l: List[A]): A = l(i)

  def nthFunctional[A](i: Int, l: List[A]): A = 
  	l.zip(Stream from 0).filter { _._2 == i }.map { _._1 }.headOption.getOrElse {
  		throw new IndexOutOfBoundsException(s"$i")
  	}

}