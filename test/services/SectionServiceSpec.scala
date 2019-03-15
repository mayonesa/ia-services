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
import common.{ error, SecDetLR }

class SectionServiceSpec extends PlaySpec with MockitoSugar {
  "SectionService.sectionDetails" should {
    val sectionId = 1L
    val installationId = "installation_id1"
    val instructorId = 2L
    val productId = "product_id1"
    val installationUrl = "/installation"
    val secDetUrl = "/sec_det"
    val msg = "msg"
    val insProdIdJson = Json.parse(s"""{"sectionDetail": {"instructor": "$instructorId"}, "books": [{"isbn": "$productId"}]}""")
    
    "return section details" in {
      secDet(Ok(installationId), Ok(insProdIdJson), Right((installationId, instructorId, productId)))
    }
    
    "return error when bad installation" in {
      secDet(InternalServerError(msg), Ok(insProdIdJson), Left(error("installation ID unavailable. Is section ID correct? is LMS server up? " + msg)))
    }
    
    "return error when bad instructor/product ID" in {
      secDet(Ok(installationId), Ok(msg), Left(error("instructor/product ID unavailable. Is openapi up?")))
    }
    
    "return error when bad instructor/product ID JSON" in {
      secDet(Ok(installationId), InternalServerError(msg), Left(error("instructor/product ID unavailable. Is openapi up?")))
    }
    
    "return error when exception" in {
      val mockEnv = mock[Env]
      when(mockEnv.installationIdUrl(sectionId)).thenReturn(installationUrl)
      when(mockEnv.sectionDetailsUrl(sectionId)).thenReturn(secDetUrl)
      val mockClient = mock[WSClient]
      when(mockClient.url(installationUrl)).thenThrow(new RuntimeException(msg))
      val result = Await.result(new SectionService(mockClient).sectionDetails(sectionId, mockEnv), 2.seconds)
      result mustBe Left(error("section details unavailable. Are openapi/lms webservers up? " + msg))
    }
    
    def secDet(installServ: Result, instructProdServ: Result, expected: SecDetLR) =
      Server.withRouterFromComponents() { components =>
        import components.{ defaultActionBuilder => Action }
        {
           case GET(url) => Action {
             if (url.path == installationUrl) installServ else instructProdServ
          }
       }
      } { implicit port =>
        WsTestClient.withClient { client =>
          val mockEnv = mock[Env]
          when(mockEnv.installationIdUrl(sectionId)).thenReturn(installationUrl)
          when(mockEnv.sectionDetailsUrl(sectionId)).thenReturn(secDetUrl)
          val result = Await.result(new SectionService(client).sectionDetails(sectionId, mockEnv), 2.seconds)
          result mustBe expected
        }
      }
  }
}