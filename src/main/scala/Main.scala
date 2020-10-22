import cats.effect.{ExitCode, IO, IOApp}
import com.typesafe.scalalogging.Logger

object Main
  extends IOApp
    with Display
    with Routes {

  protected val logger: Logger = Logger("main")

  override def run(args: List[String]): IO[ExitCode] = {

    for {
      routes <- getRoutes(args.head)
      sentRoutes = routes.filter(_.isSent)
      _ <- IO(logger.info(s"Got ${routes.size} routes"))
      _ <- IO(logger.info(s"Got ${sentRoutes.size} ascents"))
      _ <- IO(logger.debug(s"Sample:\n${sentRoutes.take(5).mkString("\n")}"))
      countPerGrade = {
        sentRoutes
          .groupBy(_.authorGrade)
          .view
          .mapValues(_.groupBy(_.style.get).view.mapValues(_.size).toMap)
          .toList
          .sortBy(_._1)
          .reverse
      }
      _ <- IO(logger.debug(s"Ascents per grade: $countPerGrade"))
      _ <- IO(logger.info(s"Pyramid:\n${display(countPerGrade)}"))
    } yield {
      ExitCode.Success
    }
  }

}
