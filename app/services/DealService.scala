package services

import javax.inject.Inject
import models.Env
import play.api.libs.ws.WSClient
import concurrent.{ Future, ExecutionContext }
import common.error
import play.api.Logger

class DealService @Inject() (ws: WSClient)(implicit ec: ExecutionContext) {

  def dealExists(sectionId: Long,
                 installationId: String,
                 instructorId: Long,
                 productId: String,
                 lmsUserIdOpt: Option[String],
                 sisUserIdOpt: Option[String],
                 env: Env): Future[String] =
    try {
      ws.url(env.dealExistsUrl(sectionId, installationId, instructorId, productId, lmsUserIdOpt, sisUserIdOpt))
        .get.map(_.body)
    } catch {
      case e: Exception =>
        val base = "deal confirmation unavailable. Is deal webserver up? "
        Logger.error(base + e.getMessage)
        Future.successful(base)
    }
}