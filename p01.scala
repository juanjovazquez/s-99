object P01 {

	def last[A](x: List[A]): A = x match {
		case Nil => throw new NoSuchElementException
    case x :: Nil => x
    case _ :: xs => last(xs)
	} 

}
