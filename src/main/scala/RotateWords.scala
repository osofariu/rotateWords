
class RotateWords(words: List[String]) {

  private val wordsByKey: Map[Set[Char], List[String]] = RotateWords.groupByLetters(words)

  def getGroupByLetters(letters: String): Option[Seq[String]] = {
    wordsByKey.get(RotateWords.makeKey(letters))
  }

  def getGroupByRotation(letters: String): Option[Seq[String]] = {
    val res = getGroupByLetters(letters)
      .map((seq: Seq[String]) => {
        seq.foldLeft(Seq[Seq[String]]())((acc, word) => {
            val accWithWordMaybe :Seq[Seq[String]]= acc.map((grouped: Seq[String]) => {
              if (RotateWords.matchRotation(grouped.head, word)) {
                 word +: grouped
              } else {
                grouped
              }
            })
            if (!accWithWordMaybe.flatten.contains(word)) {
              Seq(word) +: accWithWordMaybe
            } else {
              accWithWordMaybe
          }
        })
      })
    res.map((ss: Seq[Seq[String]]) => {
      ss.filter(s => s.contains(letters))
    }).get.headOption
  }
}


object RotateWords {
  def apply(words: List[String]): RotateWords = new RotateWords(words)
  def apply(words: String*): RotateWords = RotateWords(words.toList)

  def groupByLetters(words: List[String]): Map[Set[Char], List[String]] = {
    words.groupBy(makeKey)
  }

  def makeKey(word: String) = {
    word.toLowerCase.toCharArray.toSet
  }

  def matchRotation(str1: String, str2: String): Boolean = {
    val str1Versions = rotatedVersions(str1)
    val str2Versions = rotatedVersions(str2)

    str1Versions.intersect(str2Versions).nonEmpty
  }

  private def rotatedVersions(str1: String) = {
    0.until(str1.length).map(i => {
      rotateWord(str1.toLowerCase(), i)
    })
  }

  def rotateWord(str: String, i: Int): String = {
    str.substring(i) + str.substring(0, i)
  }
}
