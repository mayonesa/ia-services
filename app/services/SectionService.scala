package services

import javax.inject.Inject
import models.Env
import play.api.libs.ws.WSClient
import scala.concurrent.{ ExecutionContext, Future }
import play.api.http.Status.OK
import com.fasterxml.jackson.core.JsonParseException
import common.{ SecDetLR, error }
import play.api.Logger

class SectionService @Inject() (ws: WSClient)(implicit ec: ExecutionContext) {

  def sectionDetails(sectionId: Long, env: Env): Future[SecDetLR] = {
    try {
      val futInstallationIdLR = ws.url(env.installationIdUrl(sectionId)).get.map { resp =>
        val bod = resp.body
        if (resp.status == OK) Right(bod)
        else Left(error("installation ID unavailable. Is section ID correct? is LMS server up? " + bod))
      }
      val futInstructorAndProductIdLR = ws.url(env.sectionDetailsUrl(sectionId)).get.map { resp =>
        lazy val instructAndProductIdErr = Left(error("instructor/product ID unavailable. Is openapi up?"))
        if (resp.status == OK) {
          try {
            val json = resp.json
            Right((json \ "sectionDetail" \ "instructor").as[String], ((json \ "books")(0) \ "isbn").as[String])
          } catch {
            case jpe: JsonParseException =>
              Logger.error(instructAndProductIdErr + ":" + jpe.getMessage)
              instructAndProductIdErr
          }
        } else instructAndProductIdErr
      }

      for {
        installationIdLR <- futInstallationIdLR
        instructorAndProductIdLR <- futInstructorAndProductIdLR
      } yield for {
        installationId <- installationIdLR
        instructorAndProductId <- instructorAndProductIdLR
      } yield instructorAndProductId match {
        case (instructorIdStr, productId) => (installationId, instructorIdStr.toLong, productId)
      }
    } catch {
      case e: Exception =>
        Future.successful(Left(error("section details unavailable. Are openapi/lms webservers up? " + e.getMessage)))
    }
  }
}