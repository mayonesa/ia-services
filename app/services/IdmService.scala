package services

import javax.inject.Inject
import play.api.libs.ws.WSClient
import concurrent.ExecutionContext
import play.api.libs.json._
import play.api.Logger
import play.api.http.Status.OK

import models.Env
import common.FutLR

class IdmService @Inject() (ws: WSClient)(implicit ec: ExecutionContext) {
  def getToken(env: Env): FutLR =
    ws.url(env.jwtUrl).post(env.jwtFormData).map { jwtResp =>
      val tokenAttr = "access_token"
      val resp = jwtResp.json
      if (jwtResp.status == OK)
        (resp \ tokenAttr).asOpt[String].toRight {
          val errorMsg = s"IDM service did not include `$tokenAttr` in response. Is reseller JWT secret correct?"
          Logger.error(errorMsg + " jwt form data: " + env.jwtFormData)
          errorMsg
        }
      else {
        val errMsg = "Bad IDM web service. Is reseller JWT secret correct?"
        Logger.error(s"$errMsg: $resp")
        Left(errMsg)
      }
    }
}