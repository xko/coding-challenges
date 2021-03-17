package newstore.fruits

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Spec extends AnyFlatSpec with Matchers {
  it should "sum many fruits" in {
    Basket(List(Apple, Orange, Orange, Apple)).rawTotal shouldBe (190)
  }

  it should "return 0 for empty" in {
    Basket(Nil).rawTotal shouldBe(0)
  }

  "apple promotion" should "discount every 2nd" in {
    Basket(List(Apple, Orange, Orange, Apple), Seq(ApplePromotion) ).total shouldBe(165)
  }

  "apple promotion" should "not discount 3rd apple" in {
    Basket(List(Apple, Orange, Orange, Apple, Apple), Seq(ApplePromotion) ).total shouldBe(190)
  }

  "orange promotion" should "discount every 3rd" in {
    Basket(List(Apple, Orange, Orange, Orange, Apple), Seq(OrangePromotion) ).total shouldBe(190)
  }

  "orange promotion" should "combine with apple promotion" in {
    Basket(List(Apple, Orange, Orange, Orange, Apple), Seq(OrangePromotion, ApplePromotion) ).total shouldBe(165)
  }



}
