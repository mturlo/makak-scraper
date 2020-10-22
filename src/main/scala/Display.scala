
trait Display {

  type Paint = Any => String

  private def paint(colour: String)(any: Any): String = s"$colour$any${Console.RESET}"

  val red: Paint = paint(Console.RED)
  val yellow: Paint = paint(Console.YELLOW)
  val green: Paint = paint(Console.GREEN)
  val blue: Paint = paint(Console.BLUE)
  val magenta: Paint = paint(Console.MAGENTA)
  val cyan: Paint = paint(Console.CYAN)

  private val b = '▉'
  private val l = '│'

  private def line(num: Int, char: Char, paint: Paint): String = paint(Seq.fill(num)(char).mkString)

  def display(countPerGrade: List[(Grade, Map[String, Int])]): String = {
    val pyramid = countPerGrade.map {
      case (grade, a) =>
        val onsights = a.getOrElse("OS", 0)
        val flashes = a.getOrElse("FL", 0)
        val redpoints = a.getOrElse("RP", 0)
        val row = line(onsights, b, green) + line(flashes, b, yellow) + line(redpoints, b, red)
        val stats = s" ${green(onsights.toString.reverse.padTo(2, ' ').reverse)} $l ${red(redpoints.toString.reverse.padTo(2, ' ').reverse)} "
        val gradeStr = cyan(grade).padTo(16, ' ')
        s"$gradeStr$l$stats$l $row"
    }
    val longestRowLen = pyramid.map(_.length).max
    "┌─Grade──┬─OS─┬─RP─┬".padTo(longestRowLen - 51, '─') + "┐\n" +
    pyramid
      .map(row => s"$l ${row.padTo(longestRowLen, ' ')} $l")
      .mkString("", "\n", "\n") +
    "└────────┴────┴────┴".padTo(longestRowLen - 51, '─') + "┘\n"
  }

}