import org.scalatest.{Matchers, path}

class RotateWordsTest extends path.FunSpec with Matchers {

  describe("words containing the same letters") {
    val groupedWords = RotateWords("Tokyo", "Kyoto", "Paris", "Rapis")

    describe("ignoring order of letters") {

      it("group words with same letters finds groups") {
        groupedWords.groupBySameLetters("Tokyo").get shouldEqual List("Tokyo", "Kyoto")
        groupedWords.groupBySameLetters("Paris").get shouldEqual List("Paris", "Rapis")
        groupedWords.groupBySameLetters("Rapis").get shouldEqual List("Paris", "Rapis")

      }

      it("group words with same letters doesnt find words not in original list") {
        groupedWords.groupBySameLetters("tokyor").isEmpty shouldBe true
      }
    }

    describe("taking into account the order of the letters") {

      it("group words with same letters finds groups of two") {
        groupedWords.getGroupByRotation("Tokyo").get.toSet shouldEqual Set("Tokyo", "Kyoto")
      }

      it("group words with same letters finds only words with same rotation") {
        groupedWords.getGroupByRotation("Paris").get.toSet shouldEqual Set("Paris")
      }
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
      RotateWords.rotateWord("tokyo", 1) shouldEqual "okyot"
    }
  }
}
