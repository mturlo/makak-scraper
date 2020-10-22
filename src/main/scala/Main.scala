import cats.implicits._
import cats.effect.{ExitCode, IO, IOApp}
import com.typesafe.scalalogging.Logger
import net.ruippeixotog.scalascraper.browser.JsoupBrowser

import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._

import scala.util.matching.Regex

object Main extends IOApp {

  private val logger = Logger("main")
  private val browser = JsoupBrowser.typed()

  case class Route(name: String, authorGrade: Grade, communityGrade: Grade, style: Option[String]) {
    def isSent: Boolean = style.isDefined
  }

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

  type Paint = Any => String
  private def paint(colour: String)(any: Any): String = s"$colour$any${Console.RESET}"
  val red: Paint = paint(Console.RED)
  val yellow: Paint = paint(Console.YELLOW)
  val green: Paint = paint(Console.GREEN)
  val blue: Paint = paint(Console.BLUE)
  val magenta: Paint = paint(Console.MAGENTA)
  val cyan: Paint = paint(Console.CYAN)

  def routesFromUrl(url: String): IO[List[Route]] = {
    for {
      doc <- IO(browser.get(s"https://arenamakak.pl/ranking-drog/$url"))
      _ <- IO(logger.info(s"Reading HTML from $url"))
      routes <- IO {
        (doc >> elementList(".ranking-element")).map { element =>
          val children = element.children.toList
          val name = element >> text(".name h4")
          val authorGrade = children(1) >> text("h4")
          val communityGrade = (children(2) >> text("h4")).split(" ").head
          val style = element >?> text(".btn-active")
          Route(name, Grade.parse(authorGrade), Grade.parse(communityGrade), style)
        }
      }
    } yield {
      routes
    }
  }

  def routesArchived: IO[List[Route]] = {
        (5 to 7)
          .toList
          .map(l => s"archive?level=at-least-$l&author=&rating=")
          .map(routesFromUrl)
          .sequence
          .map(_.flatten)
//    IO(Nil)
  }

  val b = '▉'
  val l = '│'
  def draw(num: Int, char: Char, paint: Paint) = paint(Seq.fill(num)(char).mkString)

  override def run(args: List[String]): IO[ExitCode] = {
    browser.setCookie("arenamakak.pl", "user_email", args.head)
    for {
      routesArchived <- routesArchived
      routesCurrent <- routesFromUrl("index.php?level=&author=&rating=")
      routes = routesArchived ++ routesCurrent
      sentRoutes = routes.filter(_.isSent)
      _ <- IO(logger.info(s"Got ${routes.size} routes"))
      _ <- IO(logger.info(s"Got ${sentRoutes.size} ascents"))
      _ <- IO(logger.debug(s"Sample:\n${sentRoutes.take(5).mkString("\n")}"))
      perGrade = sentRoutes.groupBy(_.authorGrade)
      countPerGrade = perGrade.view.mapValues { routes =>
        routes.groupBy(_.style.get).view.mapValues(_.size).toMap
      }.toList.sortBy(_._1).reverse
      _ <- IO(logger.debug(s"Ascents per grade: $countPerGrade"))
      pyramid = countPerGrade.map {
        case (grade, a) =>
          val onsights = a.getOrElse("OS", 0)
          val flashes = a.getOrElse("FL", 0)
          val redpoints = a.getOrElse("RP", 0)
          val row = draw(onsights, b, green) + draw(flashes, b, yellow) + draw(redpoints, b, red)
          val stats = s" ${green(onsights.toString.reverse.padTo(2, ' ').reverse)} $l ${red(redpoints.toString.reverse.padTo(2, ' ').reverse)} "
          val gradeStr = cyan(grade).padTo(16, ' ')
          s"$gradeStr$l$stats$l $row"
      }
      longestRowLen = pyramid.map(_.length).max
      box =
      "┌─Grade──┬─OS─┬─RP─┬".padTo(longestRowLen - 51, '─') + "┐\n" +
      pyramid
        .map(row => s"$l ${row.padTo(longestRowLen, ' ')} $l")
        .mkString("\n") +
      "\n└────────┴────┴────┴".padTo(longestRowLen - 50, '─') + "┘\n"
      _ <- IO(logger.info(s"Pyramid:\n$box"))
    } yield {
      ExitCode.Success
    }
  }

}
