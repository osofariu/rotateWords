object RotateBetterWords {

  def rotate(words: Seq[String]): Set[Set[String]] = {
    val initialAcc: Seq[Seq[String]] = Seq()
    val rotatedAsSequence = words.foldLeft(initialAcc)((acc, city) => {
      val groupForCity: Int = acc.indexWhere((aGroup: Seq[String]) => {
        wordRotations(city).exists(aCity => {
          aCity.equalsIgnoreCase(aGroup.head)
        })
      })

      if (groupForCity >= 0)
        acc.patch(groupForCity, List(city +: acc(groupForCity)), 1)
      else
        Seq(city) +: acc
    })
    rotatedAsSequence.map(seq => seq.toSet).toSet
  }

  private def wordRotations(word: String): Seq[String] = {
    def rotateWord(str: String, i: Int): String = {
      str.substring(i) + str.substring(0, i)
    }

    0.until(word.length).map(i => {
      rotateWord(word.toLowerCase(), i)
    })
  }
}