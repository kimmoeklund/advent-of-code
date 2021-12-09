import scala.io.Source

object Day7:
  val input = Source.fromFile("src/main/scala/day7/input.txt").getLines.flatMap(_.split(",")).map(_.toInt).toList
  val example = List(16, 1, 2, 0, 4, 2, 7, 1, 2, 14)

  def part1 = solve(input, (a, b) => (a - b).abs)
  def part2 = solve(input, (a, b) => (1 to (a - b).abs).sum)

  def solve(input: List[Int], cost: (Int, Int) => Int) =
    val crabSubs = input.groupMapReduce(v => v)(_ => 1)(_ + _)
    (input.min to input.max)
      .flatMap(n => crabSubs.keys.map(n2 => (n, n2, cost(n, n2))))
      .toList
      .groupMapReduce(_._1)((t: (Int, Int, Int)) => t._3 * crabSubs.getOrElse(t._2, input.length))(_ + _)
      .minBy(_._2)
