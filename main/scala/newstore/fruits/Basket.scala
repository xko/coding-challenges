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
  }.length / 2 * Apple.priceInCents
}

object OrangePromotion extends Promotion {
  override def discount(basket: Basket): Int = basket.goods.collect {
    case a: Orange.type => a
  }.length / 3 * Orange.priceInCents
}
