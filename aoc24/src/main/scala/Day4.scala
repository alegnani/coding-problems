import scala.util.matching.Regex
import scala.compiletime.ops.boolean

def input3 = scala.io.Source.fromFile("inputs/input4").mkString

object Day3 {
  def main(args: Array[String]): Unit = {

    val chars = input3.split("\n").map(_.toCharArray.toList).toList

    val col_amount = chars(0).length
    val row_amount = chars.length
    val max_elems = col_amount.min(row_amount)

    val cols = chars
    val rev_cols = cols.map(_.reverse)
    val rows = chars.transpose
    val rev_rows = rows.map(_.reverse)

    val se_lower_diags =
      for (row_offset <- 0 until row_amount)
        yield for (
          elem <- 0 until max_elems
          if elem + row_offset < row_amount && elem < col_amount
        )
          yield chars(elem + row_offset)(elem)

    val se_upper_diags =
      for (col_offset <- 1 until col_amount)
        yield for (
          elem <- 0 until max_elems
          if elem + col_offset < col_amount && elem < row_amount
        )
          yield chars(elem)(elem + col_offset)

    val sw_lower_diags =
      for (row_offset <- 0 until row_amount)
        yield for (
          elem <- 0 until max_elems
          if elem + row_offset < row_amount && elem < col_amount
        )
          yield chars(row_amount - (elem + row_offset + 1))(elem)

    val sw_upper_diags =
      for (col_offset <- 1 until col_amount)
        yield for (
          elem <- 0 until max_elems
          if elem + col_offset < col_amount && elem < row_amount
        )
          yield
            val row = row_amount - elem - 1
            val idx = elem + col_offset
            println(s"char[$row][$idx]")
            chars(row_amount - (elem + 1))(elem + col_offset)

    val se_diags = (se_lower_diags ++ se_upper_diags).map(_.toList).toList
    val sw_diags = (sw_lower_diags ++ sw_upper_diags).map(_.toList).toList
    val rev_se_diags = se_diags.map(_.reverse)
    val rev_sw_diags = sw_diags.map(_.reverse)

    val all_lines =
      cols ++ rev_cols ++ rows ++ rev_rows ++ se_diags ++ sw_diags ++ rev_se_diags ++ rev_sw_diags

    val occurences = all_lines.map(count).sum
    println(s"Day 4, Part 1: $occurences")

    val centers =
      (for
        row <- 1 until row_amount - 1
        col <- 1 until col_amount - 1
      yield (row, col))

    val x_count = centers.count(p => is_x(chars, p._1, p._2))
    println(s"Day 4, Part 2: $x_count")

  }
}

def count(list: List[Char]): Int = list match {
  case 'X' :: 'M' :: 'A' :: 'S' :: next => 1 + count(next)
  case head :: next                     => count(next)
  case _                                => 0
}

def is_x(chars: List[List[Char]], row: Int, col: Int): Boolean = {
  val tl = chars(row - 1)(col - 1)
  val tr = chars(row - 1)(col + 1)
  val bl = chars(row + 1)(col - 1)
  val br = chars(row + 1)(col + 1)
  chars(row)(col) == 'A' &&
  (tl == 'M' && br == 'S' || tl == 'S' && br == 'M') &&
  (bl == 'M' && tr == 'S' || bl == 'S' && tr == 'M')
}
