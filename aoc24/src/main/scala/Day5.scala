import scala.util.matching.Regex
import scala.compiletime.ops.boolean

def input5 = scala.io.Source.fromFile("inputs/input5").mkString

object Day5 {
  def main(args: Array[String]): Unit = {

    val chars = input5.split("\n\n")
    val pairs =
      chars(0).split("\n").map({ case s"${a}|${b}" => (a.toInt, b.toInt) })
    println(chars(0).mkString)
    var lut = Array.ofDim[Boolean](100, 100)
    for (p <- pairs) {
      lut(p._1)(p._2) = true
    }

    val updates = chars(1).split("\n").map(_.split(",").map(_.toInt))
    val middle_page_sum = updates
      .filter(is_valid(lut, _))
      .map(arr => {
        arr(arr.length / 2)
      })
      .sum
    println(s"Day 5, Part 1: $middle_page_sum")

    val incorrect_fixed =
      updates
        .filter(!is_valid(lut, _))
        .map(arr => {
          val sorted = arr.toList.sortWith((l, u) => lut(l)(u))
          sorted(arr.length / 2)
        })
        .sum
    println(s"Day 5, Part 2: $incorrect_fixed")
  }
}

def is_valid(lut: Array[Array[Boolean]], pages: Array[Int]): Boolean = {
  val len = pages.length
  val pairs =
    (for
      l <- 0 until len
      u <- 0 until len if l < u
    yield lut(pages(l))(pages(u)))
  pairs.forall(identity)
}
