import RotateWords.makeKey

class RotateWords(words: List[String]) {

  def groupBySameLetters(letters: String): Option[Seq[String]] = {
    groupByLetters.get(RotateWords.makeKey(letters))
  }

  def getGroupByRotation(word: String): Option[Seq[String]] = {
    val groups = buildGroupsByRotation(word)
      groups.map((ss: Seq[Seq[String]]) => {
      ss.filter(s => s.contains(word))
    }).get.headOption
  }

  private def buildGroupsByRotation(word: String): Option[Seq[Seq[String]]] = {
    def addWordToMatchingGroup(acc: Seq[Seq[String]], word: String): Seq[Seq[String]] = {
      acc.map(group => {
        if (RotateWords.matchRotation(group.head, word)) {
          word +: group
        } else {
          group
        }
      })
    }

    def addWordToOwnGroupIfNotMatched(withGroupIfMatched: Seq[Seq[String]], word: String): Seq[Seq[String]] = {
      if (!withGroupIfMatched.flatten.contains(word)) {
        Seq(word) +: withGroupIfMatched
      } else {
        withGroupIfMatched
      }
    }

    groupBySameLetters(word)
      .map((wordsWithSameLetters: Seq[String]) => {
        wordsWithSameLetters.foldLeft(Seq[Seq[String]]())((acc, word) => {
          val withGroupIfMatched = addWordToMatchingGroup(acc, word)
          addWordToOwnGroupIfNotMatched(withGroupIfMatched, word)
        })
      })
  }

  private def groupByLetters: Map[Set[Char], List[String]] = {
    words.groupBy(makeKey)
  }
}

object RotateWords {
  def apply(words: List[String]): RotateWords = new RotateWords(words)
  def apply(words: String*): RotateWords = RotateWords(words.toList)

  def makeKey(word: String): Set[Char] = {
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
