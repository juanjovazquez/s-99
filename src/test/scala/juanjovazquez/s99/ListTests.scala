package juanjovazquez.s99

import org.scalatest.FunSuite
import org.scalacheck._
import org.scalacheck.Prop.forAll
import org.scalatest.prop.Checkers

class ListTests extends FunSuite with Checkers {

  import Lists._

  /** P01 */
  test("`last` should find the last element in lists") {
    check { (a: List[Int], e: Int) =>
      val target = (e :: a).reverse
      last(target) == e
    }
  }

  /** P02 */
  test("`penultimate` should find the last but one in lists") {
    check { (a: List[Int], e: Int) =>
      val target = ((e + 1) :: e :: a).reverse
      penultimate(target) == e
    }
  }

  /** P03 */
  test("`nth` should find the kth element in lists") {
    check {
      forAll(listPlusIndexGen) { case (a, k) =>
        nth(k, a) == a(k)
      }
    }
  }

  // Generators

  val listPlusIndexGen = for {
    l <- Gen.nonEmptyListOf[Int](Gen.choose(0, 200))
    k <- Gen.choose(0, l.size - 1)
  } yield (l, k)

}
