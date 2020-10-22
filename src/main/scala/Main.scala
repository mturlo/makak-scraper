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
      _ <- IO(logger.info("By author grade:\n" + display(countPerGrade(sentRoutes)(_.authorGrade))))
      _ <- IO(logger.info("By community grade:\n" + display(countPerGrade(sentRoutes)(_.communityGradeWithFallback))))
    } yield {
      ExitCode.Success
    }
  }

}
