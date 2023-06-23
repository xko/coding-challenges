import org.scalatest.funspec.AnyFunSpec
import DecibinaryNumbers.Result._
import DecibinaryNumbers.Solution._
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

  it("increments as expected") {
    increment(fromLong(31)).map(toLong)  shouldBe 32 :: 40 :: Nil
    increment(fromLong(22)).map(toLong)  shouldBe 23 :: Nil
    increment(fromLong(111)).map(toLong) shouldBe 112 :: 120 :: 200 :: 1000 :: Nil
  }

  it("solves examples") {
    decibinaryNumbers(8) shouldBe 12
    decibinaryNumbers(23) shouldBe 23
    decibinaryNumbers(19) shouldBe 102
    decibinaryNumbers(16) shouldBe 14

  }
}
