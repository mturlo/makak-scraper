import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

sealed trait Grade {
  def value: Double
  def increment: Grade = {
    this match {
      case BorderGrade(_, upper)            => upper
      case FullGrade(number, "C", true)     => BorderGrade(this, FullGrade(number + 1, "A", plus = false))
      case FullGrade(number, letter, true)  => BorderGrade(this, FullGrade(number, (letter.head.toInt + 1).toChar.toString, plus = false))
      case FullGrade(number, letter, false) => FullGrade(number, letter, plus = true)
      case UnknownGrade                     => UnknownGrade
    }
  }
}

case class FullGrade(number: Int, letter: String, plus: Boolean) extends Grade {
  override def toString: String = s"$number$letter${if (plus) "+" else ""}"
  override def value: Double = number.toDouble * 10 + (letter.head.toDouble - 65) + (if (plus) 0.5 else 0)
}

case class BorderGrade(lower: Grade, upper: Grade) extends Grade {
  override def toString: String = s"$lower/$upper"
  override def value: Double = (lower.value + upper.value) / 2
}

case object UnknownGrade extends Grade {
  override def value: Double = 0
}

object Grade {
  implicit val ordering: Ordering[Grade] = Ordering.by(_.value)
  object pattern {
    val number = "\\d"
    val letter = "[A-C]"
    val full: Regex = s"($number)($letter)?(\\+)?".r
    val border: Regex = s"$full/($number)?($letter)(\\+)?".r
    val unknown: String = "?"
  }
  def parse(str: String): Grade = {
    str match {
      case pattern.full(number, null, plus)                                          =>
        FullGrade(number.toInt, "A", plus == "+")
      case pattern.full(number, letter, plus)                                        =>
        FullGrade(number.toInt, letter, plus == "+")
      case pattern.border(number, letter, plus, upperNumber, upperLetter, upperPlus) =>
        val lowerGrade = FullGrade(number.toInt, letter, plus == "+")
        val upperGrade = (upperNumber, upperLetter, upperPlus) match {
          case (null, l, p) => lowerGrade.copy(letter = l, plus = p == "+")
          case (n, null, p) => lowerGrade.copy(number = n.toInt, plus = p == "+")
          case (n, l, null) => FullGrade(n.toInt, l, plus = false)
          case _            => ???
        }
        BorderGrade(lowerGrade, upperGrade)
      case pattern.unknown                                                           =>
        UnknownGrade
      case ""                                                                        =>
        UnknownGrade
    }
  }
  def fromValue(value: Double): Grade = {
    all.map { g =>
        g -> Math.abs(value - g.value)
    }.minBy(_._2)._1
  }
  def range(from: Grade, to: Grade): Seq[Grade] = {
    var tmp = from
    var buffer = ListBuffer.empty[Grade]
    while (tmp != to) {
      buffer += tmp
      tmp = tmp.increment
    }
    buffer.toList
  }
  def all: Seq[Grade] = range(min, max)
  implicit class GradeStringOps(inner: String) {
    def grade: Grade = Grade.parse(inner)
  }
  val min: Grade = "4A".grade
  val max: Grade = "9C".grade
}
