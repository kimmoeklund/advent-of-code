import scala.io.Source

object Day1 {

  val inputs =
    Source
      .fromFile("input.txt")
      .getLines
      .toList
      .map(_.toInt)

  def puzzle1 =
    inputs.foldLeft((0, 0))((acc, value) =>
      if (acc._2 > 0 && acc._2 < value) { (acc._1 + 1, value) }
      else (acc._1, value)
    )
  def puzzle2 = {
    inputs
      .sliding(3, 1)
      .toList
      .foldLeft((0, List[Int](0)))((acc, value) => {
        if (acc._2.sum < value.sum) {
          (acc._1 + 1, value)
        } else (acc._1, value)
      })
  }
}
object Day_1 {
  def part1(input: List[Int]): Int =
    input
      .sliding(2)
      .count { case Seq(x, y) => y > x }

  def part2(input: List[Int]): Int =
    input
      .sliding(3)
      .map(_.sum)
      .sliding(2)
      .count { case Seq(x, y) => y > x }
}
