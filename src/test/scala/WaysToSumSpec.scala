import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class WaysToSumSpec extends AnyWordSpec with Matchers {
    "it" should {
        "do simple cases" in {
            WaysToSum.Result.ways(4,2) shouldBe 3
            WaysToSum.Result.ways(5,3) shouldBe 5
            WaysToSum.Result.ways(8,2) shouldBe 5
        }
    }

}
