package controllers

import javax.inject.Inject
import java.lang.Long.{ valueOf => longValue }
import play.api.mvc._
import models._
import services._
import scala.concurrent.{ ExecutionContext, Future }
import play.api.libs.json._
import Json.JsValueWrapper
import common._

class OptController @Inject() (cc: ControllerComponents,
                               optService: OptService,
                               dealService: DealService,
                               secService: SectionService,
                               envs: Envs)(implicit ec: ExecutionContext) extends AbstractController(cc) {

  def resellers = Action {
    envs.resellerListOpt match {
      case Right(resellerList) => Ok(Json.obj("resellers" -> resellerList.map { reseller =>
        Json.obj("name" -> reseller._1, "envs" -> reseller._2)
      }))
      case Left(errMsg) => InternalServerError(error(errMsg))
    }
  }

  def optin(reseller: String, env: String, sectionIdOpt: Option[String]) =
    opt(reseller, env, optService.optin, sectionIdOpt)

  def optout(reseller: String, env: String, sectionIdOpt: Option[String]) =
    opt(reseller, env, optService.optout, sectionIdOpt)

  private def opt(reseller: String,
                  envStr: String,
                  optServ: (JsValue, Env) => FutLR,
                  sectionIdOpt: Option[String]) = Action.async { request =>
    envs.resellerEnvMap match {
      case Right(resEnvMap) =>
        lazy val resellrNotAvail = s"$reseller not available as a reseller (i.e., ${resEnvMap.keys.mkString(", ")})"
        resEnvMap.get(reseller).fold(badReq(resellrNotAvail)) { envMap =>
          lazy val resellrEnvNotAvail = s"env, $envStr, not one of $reseller's configured env(s) (i.e., ${envMap.keys.mkString(", ")})"
          envMap.get(envStr).fold(badReq(resellrEnvNotAvail)) { env =>
            request.body.asJson.fold(badReq("Missing JSON")) { json =>
              def option(att: String) = (json \ att).asOpt[String]
              val lmsUserIdOpt = option("lmsUserId")
              val sisUserIdOpt = option("sisUserId")

              def handleOpt(optBod: String) = {
                val optJs: (String, JsValueWrapper) = "opt" -> Json.parse(optBod)
                val secIdAndFutSecDetailsOpt = sectionIdOpt.flatMap { sectionIdStr =>
                  if (sectionIdStr.isEmpty) None
                  else
                    try {
                      val sectionId = longValue(sectionIdStr)
                      Some(sectionId, secService.sectionDetails(sectionId, env))
                    } catch {
                      case _: NumberFormatException => Some(null, Future.successful(Left("section ID must be an integer")))
                    }
                }

                def withOpt(msg: String) =
                  Json.obj(optJs, "deal" -> Json.parse(
                    if (msg != "") msg
                    else error("empty deal response. is deal service up?")))

                secIdAndFutSecDetailsOpt.fold(Future.successful(Json.obj(optJs))) {
                  case (secId, futSecDetailsLR) =>
                    def handleSecDetails(secDetailsLR: SecDetLR) =
                      secDetailsLR.fold(errMsg => Future.successful(error(errMsg)), {
                        case (installationId, instructorId, productId) =>
                          dealService.dealExists(secId, installationId, instructorId, productId, lmsUserIdOpt, sisUserIdOpt, env)
                      })

                    for {
                      secDetailsLR <- futSecDetailsLR
                      dealStr <- handleSecDetails(secDetailsLR)
                    } yield withOpt(dealStr)
                }.map(Ok(_))
              }

              if (lmsUserIdOpt.getOrElse("").isEmpty && sisUserIdOpt.getOrElse("").isEmpty)
                badReq("lms and/or sis user ID req'd")
              else
                optServ(json, env).flatMap(_.fold(errBod => Future.successful(InternalServerError(optJsonify(errBod))), handleOpt))
            }
          }
        }
      case Left(errMsg) => internalServErr(errMsg)
    }
  }

  private def internalServErr(msg: String) = Future.successful(InternalServerError(optJsonify(error(msg))))
  private def optJsonify(msg: String) = Json.parse(s"""{ "opt": $msg }""")
  private def badReq(msg: String) = Future.successful(BadRequest(optJsonify(error(msg))))
}