def input1 = scala.io.Source.fromFile("inputs/input1").mkString

object Day1 {
  def main(args: Array[String]): Unit = {
    val lines = input1
      .split("\n")
      .filter((s: String) => !s.isEmpty())
      .map((s: String) =>
        val pair = s.split(" ")
        (pair(0).toInt, pair(3).toInt)
      )
    val (first, second) = lines.unzip()

    // Part 1

    val sorted_fst = first.sorted()
    val sorted_snd = second.sorted()
    val sum =
      sorted_fst.zip(sorted_snd).map((a: Int, b: Int) => (a - b).abs).sum
    println("Day 1, Part 1: " + sum)

    // Part 2

    val sum2 =
      sorted_fst
        .map((a: Int) => second.filter((x: Int) => x == a).length * a)
        .sum
    println("Day 1, Part 2: " + sum2)

  }
}
