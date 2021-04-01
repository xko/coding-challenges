package newstore.fruits

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Spec extends AnyFlatSpec with Matchers {
  private def sum(fruits: Fruit*) = fruits.map(_.priceInCents).sum

  it should "sum many fruits" in {
    Basket(List(Apple, Orange, Orange, Apple)).rawTotal shouldBe
      sum(Apple, Orange, Orange, Apple)
  }

  it should "return 0 for empty" in {
    Basket(Nil).rawTotal shouldBe 0
  }

  "apple promotion" should "discount every 2nd" in {
    Basket(List(Apple, Orange, Orange, Apple), Seq(ApplePromotion) ).total shouldBe
      sum(Apple, Orange, Orange)
  }

  "apple promotion" should "not discount 3rd apple" in {
    Basket(List(Apple, Orange, Orange, Apple, Apple), Seq(ApplePromotion) ).total shouldBe
      sum(Apple, Apple, Orange, Orange)
  }

  "orange promotion" should "discount every 3rd" in {
    Basket(List(Apple, Orange, Orange, Orange, Apple), Seq(OrangePromotion) ).total shouldBe
      sum(Apple, Apple, Orange, Orange)
  }

  "orange promotion" should "not discount 4th and 5th" in {
    Basket(List(Apple, Orange, Orange, Orange, Apple, Orange, Orange), Seq(OrangePromotion) ).total shouldBe
      sum(Apple, Apple, Orange, Orange, Orange, Orange)
  }

  "orange promotion" should "combine with apple promotion" in {
    Basket(List(Apple, Orange, Orange, Orange, Apple), Seq(OrangePromotion, ApplePromotion) ).total shouldBe
      sum(Apple, Orange, Orange)
  }



}
