import cats.effect.{ExitCode, IO, IOApp}
import com.typesafe.scalalogging.Logger

object Main
  extends IOApp
    with Display
    with Routes {

  protected val logger: Logger = Logger("main")

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      tup <- getRoutes(args.head)
      (oldRoutes, currentRoutes) = tup
      allRoutes = oldRoutes ++ currentRoutes
      _ <- IO(logger.info(s"Got ${allRoutes.size} ascents"))
      _ <- IO(logger.info("Current routes - by author grade:\n" + display(countPerGrade(currentRoutes)(_.authorGrade))))
      _ <- IO(logger.info("Current routes - by community grade:\n" + display(countPerGrade(currentRoutes)(_.communityGradeWithFallback))))
      _ <- IO(logger.info("All routes - by author grade:\n" + display(countPerGrade(allRoutes)(_.authorGrade))))
      _ <- IO(logger.info("All routes - by community grade:\n" + display(countPerGrade(allRoutes)(_.communityGradeWithFallback))))
    } yield {
      ExitCode.Success
    }
  }

}
