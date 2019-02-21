object RotateWords {
  def matchWords(word: String*): List[List[String]] = ???


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
