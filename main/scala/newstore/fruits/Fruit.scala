package newstore.fruits

trait Fruit {
  def priceInCents: Int
}


object Orange extends  Fruit {
  override val priceInCents = 70
}

object Apple extends Fruit {
  override val priceInCents = 25
}
