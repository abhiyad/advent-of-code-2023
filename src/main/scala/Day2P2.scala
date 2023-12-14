import java.nio.file.{Files, Paths}

object Day2P2 {
  def main(args: Array[String]): Unit = {
    def max(a: Int, b: Int) = if (a > b) a else b
    def getRBG(play: String): (Int, Int, Int) = {
      var r, b, g = 0
      val ballsInfo = play.split(',').map(_.trim)
      ballsInfo.foreach(info => {
        if (info.split(' ').tail.head == "red")
          r = r + info.split(' ').head.toInt
        else if (info.split(' ').tail.head == "blue")
          b = b + info.split(' ').head.toInt
        else g = g + info.split(' ').head.toInt
      })
      (r, b, g)
    }

    val lines =
      Files.readString(Paths.get(getClass.getResource("day2.txt").toURI))
    val powers = lines
      .split('\n')
      .map(game => {
        val cubesInfo = game.substring(game.indexOf(":") + 1).trim
        val rbgList = cubesInfo.split(";").map(getRBG)
        val totalRbgForThisGame = rbgList.foldLeft(0, 0, 0)((acc, curr) =>
          (max(acc._1, curr._1), max(acc._2, curr._2), max(acc._3, curr._3))
        )
        totalRbgForThisGame._1 * totalRbgForThisGame._2 * totalRbgForThisGame._3
      })
    val answer = powers.sum
    println(answer)

  }
}
