package juanjovazquez.s99

import java.util.NoSuchElementException

import org.scalatest.FunSuite
import org.scalacheck.Gen
import org.scalacheck.Prop._
import org.scalatest.prop.Checkers

class ListTests extends FunSuite with Checkers {

  import Lists._

  /** P01 */
  test("`last` should find the last element in lists") {
    check { (a: List[Int], e: Int) =>
      checkLast(a, e)(last)
    }
  }

  test("`lastViaReduceRight` should find the last element in lists") {
    check { (a: List[Int], e: Int) =>
      checkLast(a, e)(lastViaReduceRight)
    }
  }

  test("`lastViaReduceLeft` should find the last element in lists") {
    check { (a: List[Int], e: Int) =>
      checkLast(a, e)(lastViaReduceLeft)
    }
  }

  private def checkLast[A](a: List[A], e: A)(f: List[A] => A) = {
    if (a == Nil) {
      throws(classOf[NoSuchElementException]) { f(a) }
    }
    else {
      val target = (e :: a).reverse
      f(target) == e
    }
  }

  /** P02 */
  test("`penultimate` should find the last but one in lists") {
    check { (a: List[Int], e: Int) =>
      if (a == Nil) {
        throws(classOf[NoSuchElementException])(penultimate(a))
      }
      val target = ((e + 1) :: e :: a).reverse
      penultimate(target) == e
    }
  }

  /** P03 */
  test("`nth` should find the kth element in lists") {
    check {
      forAll(listPlusIndexGen) { case (a, k) =>
        lazy val result = nth(k, a)
        if (a == Nil) {
          throws(classOf[IndexOutOfBoundsException])(result)
        } else {
          result == a(k)
        }
      }
    }
  }

  /** P04 */
  test("`length` should return the number of elements in lists") {
    check { (a: List[Int]) =>
      checkLength(a)(length)
    }
  }

  test("`lengthBuiltIn` should return the number of elements in lists") {
    check { (a: List[Int]) =>
      checkLength(a)(lengthBuiltIn)
    }
  }

  test("`lengthViaFoldLeft` should return the number of elements in lists") {
    check { (a: List[Int]) =>
      checkLength(a)(lengthViaFoldLeft)
    }
  }

  test("`lengthViaFoldRight` should return the number of elements in lists") {
    check { (a: List[Int]) =>
      checkLength(a)(lengthViaFoldRight)
    }
  }

  private def checkLength[A](l: List[A])(f: List[A] => Int) = {
    length(l) == l.length
  }

  // Generators

  val listPlusIndexGen = for {
    l <- Gen.listOf[Int](Gen.choose(0, 200))
    k <- Gen.choose(0, l.size - 1)
  } yield (l, k)

}
