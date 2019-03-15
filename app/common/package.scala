import concurrent.{ ExecutionContext, Future }

package object common {
  type SecDetLR = Either[String, (String, Long, String)]
  type FutLR = Future[Either[String, String]]
  
  def error(errMsg: String): String = s"""{ "errors": [{ "message": "$errMsg" }] }"""
  def futJsonL(errMsg: String)(implicit ec: ExecutionContext): Future[Either[String, String]] =
    Future.successful(Left(error(errMsg)))
}