import scala.io.Source

object Day3:

  enum Criteria:
    case MostCommon, LeastCommon

  val inputs = Source
    .fromFile("src/main/scala/day3/input.txt")
    .getLines
    .toArray

  val cols = inputs.maxBy(_.length).length
  val rows = inputs.length
  val array = Array.tabulate(rows, cols)((y, x) => inputs(y)(x).toString.toInt)

  def part1 =
    val transposed = array.transpose
    def gamma = transposed.map(digits => if digits.sum > rows / 2 then 1 else 0).map(_.toString)
    def epsilon = transposed
      .map(digits => if digits.sum > rows / 2 then 0 else 1)
      .map(_.toString)
    Integer.parseInt(epsilon.mkString, 2) * Integer.parseInt(gamma.mkString, 2)

  def checkNumber(
      numbers: Array[Array[Int]],
      index: Int,
      c: Criteria
  ): (Array[Array[Int]]) =
    if numbers.length == 1 || index > cols - 1 then (numbers)
    else
      val t = numbers.transpose
      val sum = t(index).sum
      val mostCommonBit = if sum >= numbers.length / 2.0 then 1 else 0
      val leastCommonBit = if sum < numbers.length / 2.0 then 1 else 0
      val refValue = c match {
        case Criteria.MostCommon  => mostCommonBit
        case Criteria.LeastCommon => leastCommonBit
      }
      checkNumber(
        numbers.filter(o => o(index) == refValue),
        index + 1,
        c
      )

  def part2 = {
    val oxygen = checkNumber(array, 0, Criteria.MostCommon)
    val co2 = checkNumber(array, 0, Criteria.LeastCommon)
    Integer.parseInt(oxygen(0).mkString, 2) * Integer.parseInt(co2(0).mkString, 2)
  }
