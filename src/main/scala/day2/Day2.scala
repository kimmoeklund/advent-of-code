import scala.io.Source

trait Semigroup[A] {
  def op(x: A, y: A): A
}
trait Monoid[A] extends Semigroup[A] {
  def zero: A
}
sealed trait Move
case class Forward(value: Int) extends Move
case class Depth(value: Int) extends Move
case class Position(depth: Int, forward: Int) extends Move

object Day2 {
  val moveMonoid = new Monoid[Move] {
    def op(x: Move, y: Move): Move = (x, y) match {
      case (Forward(a), Forward(b))         => Position(0, a + b)
      case (Forward(a), Position(d, f))     => Position(d, f + a)
      case (Forward(a), Depth(d))           => Position(d, a)
      case (Depth(a), Depth(b))             => Position(a + b, 0)
      case (Depth(a), Position(d, f))       => Position(d + a, f)
      case (Depth(a), Forward(f))           => Position(a, f)
      case (Position(d, f), Forward(a))     => Position(d, f + a)
      case (Position(d, f), Depth(a))       => Position(d + a, f)
      case (Position(a, b), Position(c, d)) => Position(a + c, b + d)
    }
    def zero = Position(0, 0)
  }

  val moves = Source
    .fromFile("src/main/scala/day2/input.txt")
    .getLines
    .map(_ match {
      case s"forward $value" => Forward(value.toInt)
      case s"down $value"    => Depth(value.toInt)
      case s"up $value"      => Depth(value.toInt * -1)
      case input @ _         => Position(0, 0)
    })
  val move = moves.foldLeft[Move](moveMonoid.zero)(moveMonoid.op(_, _))
  println(s"position: $move")
}
