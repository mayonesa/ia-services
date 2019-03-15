package services

import javax.inject.Inject
import play.api.libs.ws.WSClient
import concurrent.ExecutionContext
import play.api.libs.json._
import play.api.http.Status.OK
import play.api.Logger

import models.Env
import common.{ FutLR, futJsonL }

class OptService @Inject() (idmServ: IdmService, ws: WSClient)(implicit ec: ExecutionContext) {

  def optin(json: JsValue, env: Env): FutLR = opt(json, env, env.optinUrl)
  def optout(json: JsValue, env: Env): FutLR = opt(json, env, env.optoutUrl)

  private def opt(json: JsValue, env: Env, optUrl: String) =
    try {
      idmServ.getToken(env).flatMap {
        _ match {
          case Right(token) =>
            ws.url(optUrl)
              .addHttpHeaders(("Authorization", "bearer" + token), ("Content-Type" -> "application/json"))
              .post(json).map { resp =>
                val body = resp.body
                if (resp.status == OK) Right(body) else Left(body)
              }
          case Left(errMsg) => futJsonL(errMsg)
        }
      }
    } catch {
      case e: Exception =>
        val errMsg = "jwt/opt service unavailable. Deal webserver up? "
        Logger.error(errMsg + e.getMessage)
        futJsonL(errMsg)
    }
}