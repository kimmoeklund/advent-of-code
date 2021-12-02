import scala.io.Source

sealed trait Move
case class Forward(value: Int) extends Move
case class Depth(value: Int) extends Move

case class Position(depth: Int, horizontal: Int, aim: Int)

object Day2:
  val moves = Source
    .fromFile("src/main/scala/day2/input.txt")
    .getLines
    .map(_ match {
      case s"forward $value" => Forward(value.toInt)
      case s"down $value"    => Depth(value.toInt)
      case s"up $value"      => Depth(value.toInt * -1)
    })
    .toList
  val part1 = moves.foldLeft(Position(0, 0, 0))((position, move) =>
    (position, move) match {
      case (Position(d, h, _), Forward(f)) => Position(d, h + f, 0)
      case (Position(d, h, _), Depth(d2))  => Position(d2 + d, h, 0)
    }
  )
  val part2 =
    moves.foldLeft(Position(0, 0, 0))((position, move) =>
      (position, move) match {
        case (Position(d, h, a), Forward(f)) => Position(d + f * a, h + f, a)
        case (Position(d, h, a), Depth(d2))  => Position(d, h, a + d2)
      }
    )

  def solve =
    println(s"part1: $part1, ${part1.depth * part1.horizontal}")
    println(s"part2: $part2, ${part2.depth * part2.horizontal}")
