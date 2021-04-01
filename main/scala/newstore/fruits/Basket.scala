package newstore.fruits

case class Basket(goods: Seq[Fruit], promos: Seq[Promotion] = Nil) {
  def rawTotal = goods.map(_.priceInCents).sum
  def total = rawTotal - promos.map(_.discount(this)).sum
}

trait Promotion {
  def discount(basket: Basket): Int
}


object ApplePromotion extends Promotion {
  override def discount(basket: Basket): Int = basket.goods.collect {
    case a: Apple.type => a
  }.grouped(2).flatMap(_.lift(1)).map(_.priceInCents).sum
}

object OrangePromotion extends Promotion {
  override def discount(basket: Basket): Int = basket.goods.collect {
    case a: Orange.type => a
  }.grouped(3).flatMap(_.lift(2)).map(_.priceInCents).sum
}
