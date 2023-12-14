import java.nio.file.{Files, Paths}
import scala.math._

object Day4P1 {
  def main(args: Array[String]): Unit = {

    def nums(input: String): List[Int] =
      input.trim.split(' ').filter(_.nonEmpty).map(_.toInt).toList

    val lines =
      Files.readString(Paths.get(getClass.getResource("day4.txt").toURI))

    val info = lines
      .split('\n')
      .map(line => {
        val (winning, have) = (
          line.split(':').tail.head.split('|').head,
          line.split(':').tail.head.split('|').tail.head
        )
        (nums(winning), nums(have))
      })
      .toList
    val numberOfWins = info.map { case (winning, have) =>
      have.count(num => winning.toSet.contains(num))
    }
    val answer = numberOfWins.map(i => if (i == 0) 0 else pow(2, i - 1).toInt).sum
    println(answer)
  }
}
