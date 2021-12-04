import scala.io.Source

type Board = List[List[Int]]

object Day4:
  val inputs = Source.fromFile("src/main/scala/day4/input.txt").getLines.toList
  val (numbersInput, boardsInput) = inputs.splitAt(1)
  val numbers = numbersInput.flatMap(s => s.split(",").map(_.toInt))
  val boards = boardsInput.filter(_ != "").toList.grouped(5).toList.map(_.map(_.grouped(3).toList.map(_.trim.toInt)))

  def firstWin(boards: List[Board], inputs: List[Int]): Option[Board] =
    boards
      .find(board => board.exists(line => line.forall(i => inputs.contains(i))))
      .orElse(boards.find(board => board.transpose.exists(line => line.forall(i => inputs.contains(i)))))

  def remainingBoards(boards: List[Board], inputs: List[Int]): List[Board] =
    boards
      .filter(board => firstWin(List(board), inputs).isEmpty)

  def score(inputs: List[Int], board: Board): Int =
    board.flatten.filter(item => !inputs.contains(item)).sum * inputs.last

  def part1(index: Int = 4): Int = {
    val (inputs, remainingInputs) = numbers.splitAt(index)
    (remainingInputs, firstWin(boards, inputs)) match {
      case (_, Some(b: Board)) => score(inputs, b)
      case (h :: _, None)      => part1(index + 1)
      case (Nil, _)            => 0
    }
  }

  def part2(index: Int = 4, remaining: List[Board] = boards): Int = {
    val (inputs, remainingInputs) = numbers.splitAt(index)
    (remainingInputs, remainingBoards(remaining, inputs)) match {
      case (_, Nil) => score(inputs, remaining.last)
      case (_, a)   => part2(index + 1, a)
    }
  }
