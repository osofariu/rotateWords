import org.scalatest.{Matchers, path}

class RotateBetterWordsTest extends path.FunSpec with Matchers {

  describe("will group only rotated words") {
    val words =
      it("combines them because they match rotation") {
        RotateBetterWords.rotate(Seq("Tokyo", "Kyoto")) shouldEqual Set(Set("Tokyo", "Kyoto"))
      }
    it("combines those that match rotation, but keeps others separate") {
      RotateBetterWords.rotate(Seq("Tokyo", "Kyoto", "Paris")) shouldEqual Set(Set("Tokyo", "Kyoto"), Set("Paris"))
    }

    it("works with problem example") {
      RotateBetterWords.rotate(Seq("Tokyo", "London", "Rome", "Donlon", "Kyoto", "Paris")) shouldEqual
        Set(Set("Tokyo", "Kyoto"), Set("London", "Donlon"), Set("Rome"), Set("Paris"));
    }
  }

  describe("will not group words that are not a rotation") {
    it("works when no args are provided") {
      RotateBetterWords.rotate(Seq()) shouldEqual Set()

    }
    it("does not combine those that have same letters but don't match rotation") {
      RotateBetterWords.rotate(Seq("Raspi", "Paris")) shouldEqual Set(Set("Raspi"), Set("Paris"))
    }
  }
}
