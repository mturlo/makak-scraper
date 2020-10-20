import cats.effect.{ExitCode, IO, IOApp}
import com.typesafe.scalalogging.Logger
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._
import net.ruippeixotog.scalascraper.browser.JsoupBrowser

object Main extends IOApp {

  private val logger = Logger("main")
  private val browser = JsoupBrowser()

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      doc <- IO(browser.parseFile("src/main/resources/makak-archive.html"))
      _ <- IO(logger.info(s"Hello, reading from ${doc.title}"))
    } yield {
      ExitCode.Success
    }
  }

}
