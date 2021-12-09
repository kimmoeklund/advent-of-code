import scala.io.Source
import scala.annotation.tailrec

// got stuck with part2, and used https://github.com/kbielefe/advent-of-code/blob/2e29162701c0149877661fa9969db3dd667bcd6e/src/main/scala/2021/Day6.scala
object Day6:
  val input = Source
    .fromFile("src/main/scala/day6/input.txt")
    .getLines
    .flatMap(_.split(","))
    .map(_.toInt)
    .toList
    .groupMapReduce(identity)(_ => 1L)(_ + _)

  def part1 = (1 to 80).foldLeft(input)((fish, day) => nextDayCount(fish)).values.sum
  def part2 = (1 to 256).foldLeft(input)((fish, day) => nextDayCount(fish)).values.sum

  def nextDayCount(fish: Map[Int, Long]) =
    fish.flatMap {
      case (0 -> c) => Map(6 -> (fish.getOrElse(7, 0L) + c), 8 -> c)
      case (7 -> c) => Map(6 -> (fish.getOrElse(0, 0L) + c))
      case (n -> c) => Map(n - 1 -> c)
    }
