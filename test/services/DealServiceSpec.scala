package services

import org.scalatestplus.play.PlaySpec
import play.core.server.Server
import play.api.routing.sird._
import play.api.mvc._
import Results._
import play.api.libs.json._
import play.api.test._
import play.api.libs.ws.WSClient

import concurrent.Await
import concurrent.duration._
import concurrent.ExecutionContext.Implicits.global

import org.mockito.Mockito.{ mock, when }
import org.scalatest.mockito.MockitoSugar

import models.Env
import common.error

class DealServiceSpec extends PlaySpec with MockitoSugar {
  "DealService.dealExists" should {
    val sectionId = 1L
    val installationId = "installation_id1"
    val instructorId = 2L
    val productId = "product_id1"
    val lmsUserIdOpt = Some("lui1")
    val sisUserIdOpt = Some("sui1")
    val url = "/url"
    val msg = "msg"
    
    "return deal exists" in {
      Server.withRouterFromComponents() { components =>
        import components.{ defaultActionBuilder => Action }
        {
          case GET(url) => Action {
            Ok(msg)
          }
        }
      } { implicit port =>
        WsTestClient.withClient { client =>
          val mockEnv = mock[Env]
          when(mockEnv.dealExistsUrl(sectionId, installationId, instructorId, productId, lmsUserIdOpt, sisUserIdOpt)).
              thenReturn(url)
          val result = Await.result(
            new DealService(client).dealExists(sectionId, installationId, instructorId, productId, lmsUserIdOpt, sisUserIdOpt, mockEnv),
            2.seconds)
          result mustBe msg
        }
      }
    }
    
    "return error when exception" in {
      val mockEnv = mock[Env]
      when(mockEnv.dealExistsUrl(sectionId, installationId, instructorId, productId, lmsUserIdOpt, sisUserIdOpt)).
          thenReturn(url)
      val mockClient = mock[WSClient]
      when(mockClient.url(url)).thenThrow(new RuntimeException(msg))
      val result = Await.result(
        new DealService(mockClient).dealExists(sectionId, installationId, instructorId, productId, lmsUserIdOpt, sisUserIdOpt, mockEnv),
        2.seconds)
      result mustBe "deal confirmation unavailable. Is deal webserver up? "
    }
  }
}