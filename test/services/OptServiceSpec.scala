package services

import org.scalatestplus.play.PlaySpec
import play.core.server.Server
import play.api.routing.sird._
import play.api.mvc._
import Results._
import play.api.libs.json._
import play.api.test._

import concurrent.{ Await, Future }
import concurrent.duration._
import concurrent.ExecutionContext.Implicits.global

import org.mockito.Mockito.{ mock, when }
import org.scalatest.mockito.MockitoSugar

import models.Env
import common.{ FutLR, error }

class OptServiceSpec extends PlaySpec with MockitoSugar {
  private val optUrl = "/opt_url"
  private val minJson = Json.parse("{}")
  private val errMsg = "errMsg"
  private val errJson = s"""{ "errors": [{ "message": "$errMsg" }] }"""
  private val token1 = "token1"
  private val msg = "msg"

  
  "OptService.optin" should {
    val servOpt = (optServ: OptService, json: JsValue, env: Env) => optServ.optin(json, env)
    val envOptUrlF = (env: Env) => env.optinUrl

    happyOpt(servOpt, envOptUrlF)
    noToken(servOpt, envOptUrlF)
    badResp(servOpt, envOptUrlF)
    exception(servOpt, envOptUrlF)
  }

  "OptService.optout" should {
    val servOpt = (optServ: OptService, json: JsValue, env: Env) => optServ.optout(json, env)
    val envOptUrlF = (env: Env) => env.optoutUrl

    happyOpt(servOpt, envOptUrlF)
    noToken(servOpt, envOptUrlF)
    badResp(servOpt, envOptUrlF)
    exception(servOpt, envOptUrlF)
  }

  private def noToken(optServ: (OptService, JsValue, Env) => FutLR, envOptUrlF: Env => String) = {
    "return error msg when no token" in {
        val lErrMsg = Left(errMsg)
        val mockEnv = mock[Env]
        when(envOptUrlF(mockEnv)).thenReturn(optUrl)
        val mockIdmServ = mock[IdmService]
        when(mockIdmServ.getToken(mockEnv)).thenReturn(Future.successful(lErrMsg))
        val result = Await.result(optServ(new OptService(mockIdmServ, null), null, mockEnv), 2.seconds)
        result mustBe Left(errJson)
    }
  }

  private def exception(optServ: (OptService, JsValue, Env) => FutLR, envOptUrlF: Env => String) = {
    "return error when exception from IDM" in {
        val exMsg = "jwt/opt service unavailable. Deal webserver up? "
        val mockEnv = mock[Env]
        when(envOptUrlF(mockEnv)).thenReturn(optUrl)
        val mockIdmServ = mock[IdmService]
        when(mockIdmServ.getToken(mockEnv)).thenThrow(new RuntimeException(errMsg))
        val result = Await.result(optServ(new OptService(mockIdmServ, null), null, mockEnv), 2.seconds)
        result mustBe Left(error(exMsg))
    }
  }

  private def badResp(servOpt: (OptService, JsValue, Env) => FutLR, envOptUrlF: Env => String) = {
    "return error from WS" in {
      opt(servOpt, envOptUrlF, InternalServerError(errMsg), Left(errMsg))
    }
  }

  private def happyOpt(servOpt: (OptService, JsValue, Env) => FutLR, envOptUrlF: Env => String) = {
    "return body from WS" in {
      opt(servOpt, envOptUrlF, Ok(token1), Right(token1))
    }
  }

  private def opt(optServ: (OptService, JsValue, Env) => FutLR,
                  envOptUrlF: Env => String,
                  servRes: => Result,
                  expected: Either[String, String]) =
    Server.withRouterFromComponents() { components =>
      import components.{ defaultActionBuilder => Action }
      {
        case POST(optUrl) => Action {
          servRes
        }
      }
    } { implicit port =>
      WsTestClient.withClient { client =>
        val mockEnv = mock[Env]
        when(envOptUrlF(mockEnv)).thenReturn(optUrl)
        val mockIdmServ = mock[IdmService]
        when(mockIdmServ.getToken(mockEnv)).thenReturn(Future.successful(Right(token1)))
        val result = Await.result(optServ(new OptService(mockIdmServ, client), minJson, mockEnv), 2.seconds)
        result mustBe expected
      }
    }
}