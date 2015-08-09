object P02 {

  def penultimate[A](l: List[A]): A = l match {
    case a :: _ :: Nil => a
    case x :: xs => penultimate(xs) 
    case _ => throw new NoSuchElementException("list is not longer enough to have penultimate element")
  }

}
