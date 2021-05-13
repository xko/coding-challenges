import org.scalatest.funspec.AnyFunSpec
import DecibinaryNumbers.Result._
import org.scalatest.matchers.should.Matchers

class DecibinaryNumbersSpec extends AnyFunSpec with Matchers{
  it("counts decibinaries per decimal value"){
    dbinsPerValue(0) shouldBe 1
    dbinsPerValue(1) shouldBe 1
    dbinsPerValue(2) shouldBe 2
    dbinsPerValue(3) shouldBe 2
    dbinsPerValue(4) shouldBe 4
    dbinsPerValue(5) shouldBe 4
    dbinsPerValue(8) shouldBe 10
    dbinsPerValue(12) shouldBe 18
  }

  it("decodes decibinaries to decimal") {
    decode(0::0::0::1::Nil) shouldBe 8
    decode(3::2::Nil) shouldBe 7
  }

}
