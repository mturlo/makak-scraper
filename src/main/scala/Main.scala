import cats.effect.{ExitCode, IO, IOApp}
import com.typesafe.scalalogging.Logger

object Main extends IOApp {

  private val logger = Logger("main")

  override def run(args: List[String]): IO[ExitCode] = {
    IO(logger.info("Hello")).as(ExitCode.Success)
  }

}
