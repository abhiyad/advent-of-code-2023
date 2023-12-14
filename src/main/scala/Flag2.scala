import java.nio.file.{Files, Paths}

object Flag2 {

  def main(args: Array[String]): Unit = {
    val digitsMap = Map(
      ("0", 0),
      ("1", 1),
      ("2", 2),
      ("3", 3),
      ("4", 4),
      ("5", 5),
      ("6", 6),
      ("7", 7),
      ("8", 8),
      ("9", 9),
      ("one", 1),
      ("two", 2),
      ("three", 3),
      ("four", 4),
      ("five", 5),
      ("six", 6),
      ("seven", 7),
      ("eight", 8),
      ("nine", 9)
    )

    val re =
      """0|1|2|3|4|5|6|7|8|9|one|two|three|four|five|six|seven|eight|nine""".r

    def getFirstDigit(input: String): Int = {
      val k = re.findFirstMatchIn(input).get
      digitsMap(k.matched)
    }

    def getLastDigit2(input: String): Int = {
      input.tails.toList.reverse
        .map(f => re.findFirstMatchIn(f))
        .filter(_.isDefined)
        .map(_.get)
        .map(x => digitsMap(x.matched))
        .head
    }

    val lines =
      Files.readString(Paths.get(getClass.getResource("flag2.txt").toURI))

    val newLines = lines.split('\n')
    val answer =
      newLines.map(line => getFirstDigit(line) * 10 + getLastDigit2(line)).sum
    println(answer)
  }
}
