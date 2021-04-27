package newstore.fruits

case class Basket(goods: Seq[Fruit], promos: Seq[Promotion] = Nil) {
  def rawTotal = goods.map(_.priceInCents).sum
  def total = rawTotal - promos.map(_.discount(this)).sum
}

trait Promotion {
  def discount(basket: Basket): Int
}


object ApplePromotion extends Promotion {
  override def discount(basket: Basket): Int = basket.goods.count(_ == Apple) / 2 * Apple.priceInCents
}

object OrangePromotion extends Promotion {
  override def discount(basket: Basket): Int = basket.goods.count(_ == Orange) / 3 * Orange.priceInCents
}
