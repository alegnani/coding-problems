import scala.util.matching.Regex
import scala.compiletime.ops.boolean

def input3 = scala.io.Source.fromFile("inputs/input3").mkString

object Day3 {
  def main(args: Array[String]): Unit = {

    val mult_regex = """mul\((\d+),(\d+)\)""".r
    val matches = mult_regex.findAllMatchIn(input3).map(_.matched)

    val sum = matches.map({ case s"mul(${a},${b})" => a.toInt * b.toInt }).sum

    println("Day 3, Part 1: " + sum)

    val regex = """(mul\((\d+),(\d+)\))|do\(\)|don't\(\)""".r
    val matches2 = regex.findAllMatchIn(input3).map(_.matched)
    val sum2 = matches2
      .scanLeft((true, 0)) { (acc, s) =>
        val (enabled, _) = acc
        s match {
          case s"mul(${a},${b})" =>
            (enabled, if (enabled) a.toInt * b.toInt else 0)
          case s"do()"    => (true, 0)
          case s"don't()" => (false, 0)
        }
      }
      .map(_._2)
      .sum
    println(s"Day 3, Part 2: $sum2")
  }
}
