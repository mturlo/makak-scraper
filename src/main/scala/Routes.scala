import cats.Parallel
import cats.effect.IO
import cats.implicits._
import com.typesafe.scalalogging.Logger
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL._

trait Routes {

  protected def logger: Logger

  private val browser = JsoupBrowser.typed()

  case class Route(name: String,
                   authorGrade: Grade,
                   communityGrade: Grade,
                   style: Option[String]) {
    def isSent: Boolean = style.isDefined
    def communityGradeWithFallback: Grade = {
      communityGrade match {
        case UnknownGrade => authorGrade
        case notUnknown => notUnknown
      }
    }
  }

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
      routes.filter(_.isSent)
    }
  }

  def routesArchived(implicit parallel: Parallel[IO]): IO[List[Route]] = {
    (5 to 7)
      .toList
      .map(l => s"archive?level=at-least-$l&author=&rating=")
      .map(routesFromUrl)
      .parSequence
      .map(_.flatten)
  }

  def countPerGrade(routes: List[Route])(grouping: Route => Grade): List[(Grade, Map[String, Int])] = {
    routes
      .groupBy(grouping)
      .view
      .mapValues(_.groupBy(_.style.get).view.mapValues(_.size).toMap)
      .toList
      .sortBy(_._1)
      .reverse
  }

  def getRoutes(user: String)(implicit parallel: Parallel[IO]): IO[(List[Route], List[Route])] = {
    browser.setCookie("arenamakak.pl", "user_email", user)
    (routesArchived, routesFromUrl("index.php?level=&author=&rating=")).parTupled
  }

}
