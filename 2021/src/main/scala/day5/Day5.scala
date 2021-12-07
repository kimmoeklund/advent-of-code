import scala.io.Source

case class Line(start: (Int, Int), end: (Int, Int))

object Day5:
  val lines = Source
    .fromFile("src/main/scala/day5/input.txt")
    .getLines
    .map(l => {
      l.split(" -> ")
        .map(_.split(","))
        .reduce((a, b) => a ++ b)
    })
    .map((s: Array[String]) => Line((s(0).toInt, s(1).toInt), (s(2).toInt, s(3).toInt)))
    .toArray

  def expandLine(line: Line): Array[(Int, Int)] =
    val xDelta = line.end._1 - line.start._1
    val yDelta = line.end._2 - line.start._2
    (for (d <- 0 to (xDelta.abs max yDelta.abs))
      yield (
        (line.start._1 + (d * xDelta.sign)),
        (line.start._2 + (d * yDelta.sign))
      )).toArray

  def calculateOverlaps(expandedLines: Array[Array[(Int, Int)]]): Int =
    expandedLines.flatten.groupBy(identity).values.count(_.size > 1)

  def part1 =
    val part1Lines = lines.filter(v => v.start._1 == v.end._1 || v.start._2 == v.end._2)
    println(
      calculateOverlaps(
        part1Lines
          .map(expandLine)
      )
    )

  def part2 =
    println(calculateOverlaps(lines.map(expandLine)))
