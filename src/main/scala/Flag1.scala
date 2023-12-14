import java.nio.file.{Files, Paths}

object Flag1 {
  def main(args: Array[String]): Unit = {
    val lines =
      Files.readString(Paths.get(getClass.getResource("flag1.txt").toURI))
    val digitsInEachLine = lines
      .split('\n')
      .map(line =>
        line
          .filter(char => char.isDigit)
          .map(x => Integer.valueOf(x) - Integer.valueOf('0'))
          .toList
      )
      .toList
    val answer = digitsInEachLine.map(digits => digits.head * 10 + digits.reverse.head).sum
    print(answer)
  }
}
