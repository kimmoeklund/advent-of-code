import scala.io.Source

object Day1:

  val inputs =
    Source
      .fromFile("src/main/scala/day1/input.txt")
      .getLines
      .toList
      .map(_.toInt)

  def part1 =
    inputs
      .foldLeft((0, 0))((acc, value) =>
        if (acc._2 < value) then (acc._1 + 1, value)
        else (acc._1, value)
      )
      ._1

  def part2 =
    inputs
      .sliding(3, 1)
      .toList
      .foldLeft((0, List[Int](0)))((acc, value) => {
        if (acc._2.sum < value.sum) then (acc._1 + 1, value)
        else (acc._1, value)
      })
      ._1
