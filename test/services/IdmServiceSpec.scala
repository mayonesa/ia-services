package services

import org.scalatestplus.play.PlaySpec
import play.core.server.Server
import play.api.routing.sird._
import play.api.mvc._
import Results._
import play.api.libs.json._
import play.api.test._

import concurrent.Await
import concurrent.duration._
import concurrent.ExecutionContext.Implicits.global

import org.mockito.Mockito.{ mock, when }
import org.scalatest.mockito.MockitoSugar

import models.Env

class IdmServiceSpec extends PlaySpec with MockitoSugar {
  "IdmService.getToken" should {
    val tokenAttr = "access_token"
    val tokenMissing = s"IDM service did not include `$tokenAttr` in response. Is reseller JWT secret correct?"
    val errMsg = Json.parse("""{"error": "errMsg"}""")
    val minJson = Json.parse("{}")
    "return token" in {
      val token1 = "token1"
      getToken(Json.obj(tokenAttr -> token1), Right(token1))
    }
    "return error when no token available" in {
      getToken(minJson, Left(tokenMissing))
    }
    "return error when web service not OK" in {
      getToken(InternalServerError(errMsg), Left("Bad IDM web service. Is reseller JWT secret correct?"))
    }
  }

  private def getToken(json: JsValue, expected: Either[String, String]): Unit = getToken(Ok(json), expected)

  private def getToken(servRes: => Result, expected: Either[String, String]) =
    Server.withRouterFromComponents() { components =>
      import components.{ defaultActionBuilder => Action }
      {
        case POST(idmUrl) => Action {
          servRes
        }
      }
    } { implicit port =>
      WsTestClient.withClient { client =>
        val mockEnv = mock[Env]
        val optUrl = "/opt_url"
        val jwtFormData = Map("grant_type" -> Seq("client_creds"))
        when(mockEnv.jwtUrl).thenReturn(optUrl)
        when(mockEnv.jwtFormData).thenReturn(jwtFormData)
        val result = Await.result(new IdmService(client).getToken(mockEnv), 2.seconds)
        result mustBe expected
      }
    }
}