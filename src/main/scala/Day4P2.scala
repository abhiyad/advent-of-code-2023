import java.nio.file.{Files, Paths}
import java.util.stream.IntStream
import scala.collection.mutable

object Day4P2 {
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
      have.toSet.intersect(winning.toSet).size
    }
    val initial = List
      .fill(info.size)(1)
      .zipWithIndex
      .map { case (zero, index) => (index, zero) }
      .toMap
    val multiplicity: mutable.Map[Int, Int] =
      mutable.Map()
    multiplicity.addAll(initial)
    numberOfWins.zipWithIndex.foreach(curr => {
      val currentMultiplicity = multiplicity(curr._2)
      val wins = curr._1
      IntStream.range(curr._2 + 1, curr._2 + wins + 1).forEach(i => multiplicity(i) += currentMultiplicity)
    })
    val answer = multiplicity.values.sum
    println(answer)
  }
}
