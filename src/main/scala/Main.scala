import cats.effect.{ExitCode, IO, IOApp}
import com.typesafe.scalalogging.Logger
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._
import net.ruippeixotog.scalascraper.browser.JsoupBrowser

object Main extends IOApp {

  private val logger = Logger("main")
  private val browser = JsoupBrowser()

  case class Route(name: String, authorGrade: String, communityGrade: String, style: Option[String]) {
    def isSent: Boolean = style.isDefined
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
          val communityGrade = children(2) >> text("h4")
          val style = element >?> text(".btn-active")
          Route(name, authorGrade, communityGrade, style)
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
