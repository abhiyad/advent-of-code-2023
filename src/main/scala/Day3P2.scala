import java.nio.file.{Files, Paths}

object Day3P2 {

  def main(args: Array[String]): Unit = {

    case class NumberLocation(row: Int, start: Int, end: Int, number: Int) {
      def isValid(
          symbolLocations: List[SymbolLocations]
      ): Boolean = {

        symbolLocations.exists(loc =>
          abs(loc.row, row) <= 1 && (abs(loc.column, start) <= 1 || abs(
            loc.column,
            end
          ) <= 1)
        )
      }

    }
    case class SymbolLocations(row: Int, column: Int) {
      def getGear(numberLocations: List[NumberLocation]): Option[Int] = {
        val locationsOfNumberAdjacent = numberLocations.filter(loc =>
          abs(loc.row, row) <= 1 && (abs(loc.start, column) <= 1 || abs(
            loc.end,
            column
          ) <= 1)
        )
        if (locationsOfNumberAdjacent.size == 2)
          Some(
            locationsOfNumberAdjacent.head.number * locationsOfNumberAdjacent.tail.head.number
          )
        else None
      }
    }
    def abs(a: Int, b: Int) = if (a - b >= 0) a - b else b - a

    def findNumberWithRowCol(line: String, index: Int): List[NumberLocation] = {
      val re = """\d+""".r
      val matches = re.findAllMatchIn(line)
      val output = for (nums <- matches) yield (nums)
      output
        .map(o => NumberLocation(index, o.start, o.end - 1, o.matched.toInt))
        .toList
    }

    val lines =
      Files.readString(Paths.get(getClass.getResource("day3.txt").toURI))
    val numberLocations =
      lines
        .split('\n')
        .zipWithIndex
        .flatMap { case (line, index) =>
          findNumberWithRowCol(line, index)
        }
        .toList

    val symbols = lines
      .split('\n')
      .zipWithIndex
      .flatMap { case (line, row) =>
        line.zipWithIndex.flatMap { case (char, col) =>
          if (!char.isDigit && char == '*') SymbolLocations(row, col) :: Nil
          else Nil
        }
      }
      .toList
    val answer =
      symbols.map(_.getGear(numberLocations)).filter(_.isDefined).map(_.get).sum

    println(answer)
  }

}
