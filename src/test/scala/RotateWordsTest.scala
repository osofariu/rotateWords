import org.scalatest.{Matchers, path}

class RotateWordsTest extends path.FunSpec with Matchers {

  describe("words containing the same letters") {

    it("group words with same letters") {
      RotateWords.matchWords("Tokyo", "Kyoto", "Paris").length shouldBe(2)
    }

    describe("words that are rotated versions") {

      it("when rotated they match, when ignoring case") {
        RotateWords.matchRotation("tokyo", "kyoto") shouldBe true
      }

      it("when rotated they match, when case is used") {
        RotateWords.matchRotation("Tokyo", "Kyoto") shouldBe true
      }
    }
  }

  describe("rotate") {
    it("rotates tokyo by 1") {
      RotateWords.rotateWord("tokyo", 1) shouldEqual("okyot")
    }
  }
}
