import cats.effect.{ExitCode, IO, IOApp}
import com.typesafe.scalalogging.Logger
import net.ruippeixotog.scalascraper.browser.JsoupBrowser

import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._

import scala.util.matching.Regex

object Main extends IOApp {

  private val logger = Logger("main")
  private val browser = JsoupBrowser()

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

  def routesFromFile(fileName: String): IO[List[Route]] = {
    for {
      doc <- IO(browser.parseFile(s"src/main/resources/$fileName"))
      _ <- IO(logger.info(s"Hello, reading from ${doc.title}"))
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

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      routesArchived5 <- routesFromFile("makak-archive-5.html")
      routesArchived6 <- routesFromFile("makak-archive-6.html")
      routesArchived7 <- routesFromFile("makak-archive-7.html")
      routesCurrent <- routesFromFile("makak-current.html")
      routesArchived = routesArchived5 ++ routesArchived6 ++ routesArchived7
      routes = routesArchived ++ routesCurrent
      sentRoutes = routes.filter(_.isSent)
      perGrade = sentRoutes.groupBy(_.authorGrade)
      countPerGrade = perGrade.view.mapValues { routes =>
        routes.groupBy(_.style.get).view.mapValues(_.size).toMap
      }.toList.sortBy(_._1).reverse
      maxCountPerGrade = countPerGrade.map(_._2.values.sum).max
      pyramid = countPerGrade.map {
        case (grade, a) =>
          val onsights = a.getOrElse("OS", 0)
          val flashes = a.getOrElse("FL", 0)
          val redpoints = a.getOrElse("RP", 0)
          def foo(num: Int, char: Char, paint: Paint) = paint(Seq.fill(num)(char).mkString)
          val row = foo(redpoints, '-', red) + foo(flashes, '~', yellow) + foo(onsights * 2, '=', green) + foo(flashes, '~', yellow) + foo(redpoints, '-', red)
          val spaces = Seq.fill(maxCountPerGrade * 2 - row.length / 2 - maxCountPerGrade / 2)(" ").mkString
          val padding = s"\t$spaces"
          s"${cyan(grade)}:$padding$row"
      }.mkString("\n")
      _ <- IO(logger.info(s"Got ${routes.size} routes"))
      _ <- IO(logger.info(s"Got ${sentRoutes.size} ascents"))
      _ <- IO(logger.debug(s"Sample:\n${sentRoutes.take(5).mkString("\n")}"))
      _ <- IO(logger.debug(s"Ascents per grade: $countPerGrade"))
      _ <- IO(logger.info(s"Pyramid:\n$pyramid"))
    } yield {
      ExitCode.Success
    }
  }

}
