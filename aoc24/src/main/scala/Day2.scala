def input2 = scala.io.Source.fromFile("inputs/input2").mkString

object Day2 {
  def main(args: Array[String]): Unit = {
    val lines = input2
      .split("\n")
      .filter((s: String) => !s.isEmpty())
      .map((s: String) => s.split(" ").map((s: String) => s.toInt))

    // Part 1
    val is_safe = (report: Array[Int]) => {
      val is_sorted_asc =
        !report.zip(report.tail).exists { case (x, y) => x > y }
      val is_sorted_desc =
        !report.zip(report.tail).exists { case (x, y) => x < y }
      val is_gap =
        !report.zip(report.tail).exists { case (x, y) =>
          val diff = (x - y).abs
          !(1 <= diff && diff <= 3)
        }
      is_gap && (is_sorted_desc || is_sorted_asc)
    }

    val count = lines.count(is_safe)
    println("Day 2, Part 1: " + count)

    // Part 2
    val count2 = lines.count((report: Array[Int]) => {
      println(report.mkString(" "))
      (0 to report.length)
        .map((idx: Int) => {
          report.zipWithIndex
            .filter(_._2 != idx)
            .map(_._1)
        })
        .exists(is_safe)
    })

    println("Day 2, Part 2: " + count2)

  }
}
