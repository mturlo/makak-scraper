import cats.effect.{ExitCode, IO, IOApp}
import com.typesafe.scalalogging.Logger
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._
import net.ruippeixotog.scalascraper.browser.JsoupBrowser

import scala.util.matching.Regex

object Main extends IOApp {

  private val logger = Logger("main")
  private val browser = JsoupBrowser()

  case class Route(name: String, authorGrade: Grade, communityGrade: Grade, style: Option[String]) {
    def isSent: Boolean = style.isDefined
  }

  sealed trait Grade
  case class FullGrade(number: Int, letter: String, plus: Boolean) extends Grade {
    override def toString: String = s"$number$letter${if(plus)"+" else ""}"
  }
  case class BorderGrade(lower: Grade, upper: Grade) extends Grade {
    override def toString: String = s"$lower/$upper"
  }
  case object UnknownGrade extends Grade
  object Grade {
    implicit val ordering: Ordering[Grade] = {
      Ordering.fromLessThan {
        case (l: FullGrade, r: FullGrade) => false
        case _                            => false
      }
    }
    object pattern {
      val number = "\\d"
      val letter = "[A-C]"
      val full: Regex = s"($number)($letter)?(\\+)?".r
      val border: Regex = s"$full/($number)?($letter)(\\+)?".r
      println(full)
      println(border)
      val unknown: String = "?"
    }
    def parse(str: String): Grade = {
      println(s"===str = '${str}'")
      str match {
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

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      doc <- IO(browser.parseFile("src/main/resources/makak-archive.html"))
      _ <- IO(logger.info(s"Hello, reading from ${doc.title}"))
      routes <- IO {
        (doc >> elementList(".ranking-element")).map { element =>
          val children = element.children.toList
          val name = element >> text(".name h4")
          val authorGrade = children(1) >> text("h4")
          val communityGrade = (children(2) >> text("h4")).split(" ").head
          println(name)
          val style = element >?> text(".btn-active")
          Route(name, Grade.parse(authorGrade), Grade.parse(communityGrade), style)
        }
      }
      sentRoutes = routes.filter(_.isSent)
      perGrade = sentRoutes.groupBy(_.authorGrade)
      countPerGrade = perGrade.view.mapValues(_.size).toList.sortBy(_._1)
      _ <- IO(logger.info(s"Got ${routes.size} routes in document"))
      _ <- IO(logger.info(s"Got ${sentRoutes.size} ascents in document"))
      _ <- IO(logger.info(s"Sample:\n${sentRoutes.take(5).mkString("\n")}"))
      _ <- IO(logger.info(s"Ascents per grade: $countPerGrade"))
    } yield {
      ExitCode.Success
    }
  }

}
