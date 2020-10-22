import scala.util.matching.Regex

sealed trait Grade {
  def value: Double
}

case class FullGrade(number: Int, letter: String, plus: Boolean) extends Grade {
  override def toString: String = s"$number$letter${if (plus) "+" else ""}"
  override def value: Double = number.toDouble * 10 + (letter.head.toDouble - 65) + (if (plus) 0.5 else 0)
}

case class BorderGrade(lower: Grade, upper: Grade) extends Grade {
  override def toString: String = s"$lower/$upper"
  override def value: Double = lower.value + 0.1
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
}
