package controllers

import org.scalatestplus.play.PlaySpec
import play.api.test._
import Helpers._
import org.scalatest.mockito.MockitoSugar
import org.mockito.Mockito.{ mock, when }
import play.api.libs.json._
import java.lang.Long.{ valueOf => longVal }
import concurrent.Future
import concurrent.ExecutionContext.Implicits.global
import play.api.mvc.{ Action, AnyContent }

import models._
import services._
import common.FutLR

class OptControllerSpec extends PlaySpec with StubControllerComponentsFactory with MockitoSugar {
  private val rs1 = "rs1"
  private val errMsg = "errMsg"
  private val envStr1 = "env1"

  "Opt#resellers" must {
    val rs2 = "rs2"
    val rs3 = "rs3"
    val env2 = "env2"
    val env3 = "env3"
    val expResllrs = Seq((rs1, Seq(envStr1, env3)), (rs2, Seq(envStr1, env2, env3)), (rs3, Seq(env2, env3)))
    val expJson = Json.obj("resellers" -> Json.arr(Json.obj("name" -> rs1, "envs" -> Json.arr(envStr1, env3)),
                                                   Json.obj("name" -> rs2, "envs" -> Json.arr(envStr1, env2, env3)),
                                                   Json.obj("name" -> rs3, "envs" -> Json.arr(env2, env3))))
    val res = resellers(Right(expResllrs))
    "return OK" in {
      status(res) mustBe OK
    }
    "return proper JSON" in {
      contentAsJson(res) mustBe expJson
    }
    val badRes = resellers(Left(errMsg))
    val whenPhrase = " when in bad config"
    "return internal server error" + whenPhrase in {
      status(badRes) mustBe INTERNAL_SERVER_ERROR
    }
    "return error msg" + whenPhrase in {
      (contentAsJson(badRes) \ "errors" \ 0 \ "message").as[String] mustBe errMsg
    }
  }
  
  private def resellers(envsRet: Either[String, Seq[(String, Seq[String])]]) = {
    val mockEnvs = mock[Envs]
    when(mockEnvs.resellerListOpt).thenReturn(envsRet)
    val optController = new OptController(stubControllerComponents(), null, null, null, mockEnvs)(null)
    optController.resellers(FakeRequest())
  }

  private val optBod = """{"externalOpt": "i is opt"}"""
  private val optRespJson = Json.parse(optBod)
  private val optRet = Future.successful(Right(optBod))
  private val lmsUserId1 = "lms_user_id1"
  private val sisUserId1 = "sis_user_id1"
  private val lmsUserId1Opt = Some(lmsUserId1)
  private val sisUserId1Opt = Some(sisUserId1)
  private val urlStub = Seq("part1", "part2")
  private val env1 = new Env(envStr1, Map(("jwtUrl" -> urlStub), ("optUrl" -> urlStub), ("openapi" -> urlStub)),
                             Map.empty[String, String], null, null)
  private val resEnvMap = Right(Map(rs1 -> Map(envStr1 -> env1)))
  private val sectionIdStr = "42"
  private val sectionIdOpt = Some(sectionIdStr)
  private val secId = longVal(sectionIdStr)
  private val installId = "install_id1"
  private val instructorId = 777
  private val productId = "prod_id1"
  private val secDets: Future[Either[String, (String, Long, String)]] = Future.successful(Right((installId, instructorId,
                                                                                                 productId)))
  private val dealStr = """{"msg": "i'm kinda big deal"}"""
  private val dealRet = Future.successful(dealStr)
  private val lmsUserIdNm = "lmsUserId"
  private val marginalReqJson = Json.obj(lmsUserIdNm -> lmsUserId1)
  private val errJsonStr = """{"error": "big mistake"}"""

  "Opt#in" must {
    val optinContF = (optCont: OptController, reslr: String, envStr: String, secIdOpt: Option[String]) => optCont.optin(reslr,
                                                                                                                        envStr,
                                                                                                                        secIdOpt)
    val servOptF = (optServ: OptService, reqJson: JsValue, env: Env) => optServ.optin(reqJson, env)

    happyOpt(optinContF, servOptF, lmsUserId1Opt, None, "LMS user ID")
    happyOpt(optinContF, servOptF, None, sisUserId1Opt, "SIS user ID")
    happyOpt(optinContF, servOptF, lmsUserId1Opt, sisUserId1Opt, "LMS and SIS user IDs")
    badResEnvs(optinContF)
    badRes(optinContF)
    badEnv(optinContF)
    missingJson(optinContF)
    noUserId(optinContF)
    badOptServ(optinContF, servOptF)
    noSectionId(optinContF, servOptF)
    badSectionId(optinContF, servOptF)
    badSecServ(optinContF, servOptF)
    emptyDeal(optinContF, servOptF)
  }

  "Opt#out" must {
    val optoutContF = (optCont: OptController, reslr: String, envStr: String, secIdOpt: Option[String]) => optCont.optout(reslr,
                                                                                                                          envStr,
                                                                                                                          secIdOpt)
    val servOptF = (optServ: OptService, reqJson: JsValue, env: Env) => optServ.optout(reqJson, env)

    happyOpt(optoutContF, servOptF, lmsUserId1Opt, None, "LMS user ID")
    happyOpt(optoutContF, servOptF, None, sisUserId1Opt, "SIS user ID")
    happyOpt(optoutContF, servOptF, lmsUserId1Opt, sisUserId1Opt, "LMS and SIS user IDs")
    badResEnvs(optoutContF)
    badRes(optoutContF)
    badEnv(optoutContF)
    missingJson(optoutContF)
    noUserId(optoutContF)
    badOptServ(optoutContF, servOptF)
    noSectionId(optoutContF, servOptF)
    badSectionId(optoutContF, servOptF)
    badSecServ(optoutContF, servOptF)
    emptyDeal(optoutContF, servOptF)
  }

  private def emptyDeal(opt: (OptController, String, String, Option[String]) => Action[AnyContent],
                        servOpt: (OptService, JsValue, Env) => FutLR) = {
    val mockEnvs = mock[Envs]
    when(mockEnvs.resellerEnvMap).thenReturn(resEnvMap)
    val mockOptServ = mock[OptService]
    when(servOpt(mockOptServ, marginalReqJson, env1)).thenReturn(optRet)
    val mockSecServ = mock[SectionService]
    when(mockSecServ.sectionDetails(secId, env1)).thenReturn(secDets)
    val mockDealServ = mock[DealService]
    when(mockDealServ.dealExists(secId, installId, instructorId, productId, lmsUserId1Opt, None, env1)).
      thenReturn(Future.successful(""))
    val optController = new OptController(stubControllerComponents(), mockOptServ, mockDealServ, mockSecServ, mockEnvs)
    val res = opt(optController, rs1, envStr1, sectionIdOpt).apply(FakeRequest().withJsonBody(marginalReqJson))
    val whenPhrase = " when deal response is empty"
    "return OK" + whenPhrase in {
      status(res) mustBe OK
    }
    val respJson = contentAsJson(res)
    "return error in deal" + whenPhrase in {
      (respJson \ "deal" \ "errors" \ 0 \ "message").as[String] mustBe "empty deal response. is deal service up?"
    }
    "return proper option body" + whenPhrase in {
      (respJson \ "opt").get mustBe optRespJson
    }
  }

  private def badSecServ(opt: (OptController, String, String, Option[String]) => Action[AnyContent],
                         servOpt: (OptService, JsValue, Env) => FutLR) = {
    val mockEnvs = mock[Envs]
    when(mockEnvs.resellerEnvMap).thenReturn(resEnvMap)
    val mockOptServ = mock[OptService]
    when(servOpt(mockOptServ, marginalReqJson, env1)).thenReturn(optRet)
    val mockSecServ = mock[SectionService]
    when(mockSecServ.sectionDetails(secId, env1)).thenReturn(Future.successful(Left(errMsg)))
    val optController = new OptController(stubControllerComponents(), mockOptServ, null, mockSecServ, mockEnvs)
    val res = opt(optController, rs1, envStr1, sectionIdOpt).apply(FakeRequest().withJsonBody(marginalReqJson))
    val whenPhrase = " when section section details service is bad"
    "return OK" + whenPhrase in {
      status(res) mustBe OK
    }
    val respJson = contentAsJson(res)
    "return error in deal" + whenPhrase in {
      (respJson \ "deal" \ "errors" \ 0 \ "message").as[String] mustBe errMsg
    }
    "return proper option body" + whenPhrase in {
      (respJson \ "opt").get mustBe optRespJson
    }
  }

  private def badSectionId(opt: (OptController, String, String, Option[String]) => Action[AnyContent],
                           servOpt: (OptService, JsValue, Env) => FutLR) = {
    val mockEnvs = mock[Envs]
    when(mockEnvs.resellerEnvMap).thenReturn(resEnvMap)
    val mockOptServ = mock[OptService]
    when(servOpt(mockOptServ, marginalReqJson, env1)).thenReturn(optRet)
    val optController = new OptController(stubControllerComponents(), mockOptServ, null, null, mockEnvs)
    val res = opt(optController, rs1, envStr1, Some("1.4")).apply(FakeRequest().withJsonBody(marginalReqJson))
    val whenPhrase = " when section ID is not an integer"
    "return OK" + whenPhrase in {
      status(res) mustBe OK
    }
    val respJson = contentAsJson(res)
    "return error in deal" + whenPhrase in {
      (respJson \ "deal" \ "errors" \ 0 \ "message").as[String] mustBe "section ID must be an integer"
    }
    "return proper option body" + whenPhrase in {
      (respJson \ "opt").get mustBe optRespJson
    }
  }

  private def noSectionId(opt: (OptController, String, String, Option[String]) => Action[AnyContent],
                          servOpt: (OptService, JsValue, Env) => FutLR) = {
    val mockEnvs = mock[Envs]
    when(mockEnvs.resellerEnvMap).thenReturn(resEnvMap)
    val mockOptServ = mock[OptService]
    when(servOpt(mockOptServ, marginalReqJson, env1)).thenReturn(optRet)
    val optController = new OptController(stubControllerComponents(), mockOptServ, null, null, mockEnvs)
    val res = opt(optController, rs1, envStr1, None).apply(FakeRequest().withJsonBody(marginalReqJson))
    val whenPhrase = s" when valid and section ID is not included"
    "return OK" + whenPhrase in {
      status(res) mustBe OK
    }
    val respJson = contentAsJson(res)
    "not return deal" + whenPhrase in {
      (respJson \ "deal").asOpt mustBe None
    }
    "return proper option body" + whenPhrase in {
      (respJson \ "opt").get mustBe optRespJson
    }
  }

  private def badOptServ(opt: (OptController, String, String, Option[String]) => Action[AnyContent],
                         servOpt: (OptService, JsValue, Env) => FutLR) = {
    val mockEnvs = mock[Envs]
    when(mockEnvs.resellerEnvMap).thenReturn(resEnvMap)
    val mockOptServ = mock[OptService]
    when(servOpt(mockOptServ, marginalReqJson, env1)).thenReturn(Future.successful(Left(errJsonStr)))
    val optController = new OptController(stubControllerComponents(), mockOptServ, null, null, mockEnvs)
    val res = opt(optController, rs1, envStr1, null).apply(FakeRequest().withJsonBody(marginalReqJson))
    val whenPhrase = " when there's an issue w/ the opt service"
    "return internal server error" + whenPhrase in {
      status(res) mustBe INTERNAL_SERVER_ERROR
    }
    "return error msg" + whenPhrase in {
      (contentAsJson(res) \ "opt").get mustBe Json.parse(errJsonStr)
    }
  }

  private def noUserId(opt: (OptController, String, String, Option[String]) => Action[AnyContent]) = {
    val mockEnvs = mock[Envs]
    when(mockEnvs.resellerEnvMap).thenReturn(resEnvMap)
    val optController = new OptController(stubControllerComponents(), null, null, null, mockEnvs)
    val res = opt(optController, rs1, envStr1, null).apply(FakeRequest().withJsonBody(Json.obj("msg" -> "i'm json. i will kill you.")))
    val whenPhrase = " when all user IDs are missing"
    "return Bad Request" + whenPhrase in {
      status(res) mustBe BAD_REQUEST
    }
    "return error msg" + whenPhrase in {
      (contentAsJson(res) \ "opt" \ "errors" \ 0 \ "message").as[String] mustBe "lms and/or sis user ID req'd"
    }
  }

  private def missingJson(opt: (OptController, String, String, Option[String]) => Action[AnyContent]) = {
    val mockEnvs = mock[Envs]
    when(mockEnvs.resellerEnvMap).thenReturn(resEnvMap)
    val optController = new OptController(stubControllerComponents(), null, null, null, mockEnvs)
    val res = opt(optController, rs1, envStr1, null).apply(FakeRequest())
    val whenPhrase = " when request JSON is missing"
    "return Bad Request" + whenPhrase in {
      status(res) mustBe BAD_REQUEST
    }
    "return error msg" + whenPhrase in {
      (contentAsJson(res) \ "opt" \ "errors" \ 0 \ "message").as[String] mustBe "Missing JSON"
    }
  }

  private def badEnv(opt: (OptController, String, String, Option[String]) => Action[AnyContent]) = {
    val mockEnvs = mock[Envs]
    when(mockEnvs.resellerEnvMap).thenReturn(resEnvMap)
    val optController = new OptController(stubControllerComponents(), null, null, null, mockEnvs)
    val badEnv = "bad_env"
    val res = opt(optController, rs1, badEnv, null).apply(FakeRequest())
    val whenPhrase = " when bad reseller env"
    "return Bad Request" + whenPhrase in {
      status(res) mustBe BAD_REQUEST
    }
    "return error msg" + whenPhrase in {
      (contentAsJson(res) \ "opt" \ "errors" \ 0 \ "message").as[String] mustBe s"env, $badEnv, not one of $rs1's configured env(s) (i.e., $envStr1)"
    }
  }

  private def badRes(opt: (OptController, String, String, Option[String]) => Action[AnyContent]) = {
    val mockEnvs = mock[Envs]
    when(mockEnvs.resellerEnvMap).thenReturn(resEnvMap)
    val optController = new OptController(stubControllerComponents(), null, null, null, mockEnvs)
    val badReseller = "bad_reseller"
    val res = opt(optController, badReseller, null, null).apply(FakeRequest())
    val whenPhrase = " when bad reseller"
    "return Bad Request" + whenPhrase in {
      status(res) mustBe BAD_REQUEST
    }
    "return error msg" + whenPhrase in {
      (contentAsJson(res) \ "opt" \ "errors" \ 0 \ "message").as[String] mustBe s"$badReseller not available as a reseller (i.e., $rs1)"
    }
  }

  private def badResEnvs(opt: (OptController, String, String, Option[String]) => Action[AnyContent]) = {
    val mockEnvs = mock[Envs]
    when(mockEnvs.resellerEnvMap).thenReturn(Left(errMsg))
    val optController = new OptController(stubControllerComponents(), null, null, null, mockEnvs)
    val res = opt(optController, null, null, null).apply(FakeRequest())
    val whenPhrase = " when bad config"
    "return internal server error" + whenPhrase in {
      status(res) mustBe INTERNAL_SERVER_ERROR
    }
    "return error msg" + whenPhrase in {
      (contentAsJson(res) \ "opt" \ "errors" \ 0 \ "message").as[String] mustBe errMsg
    }
  }

  private def happyOpt(opt: (OptController, String, String, Option[String]) => Action[AnyContent],
                       servOpt: (OptService, JsValue, Env) => FutLR,
                       lmsUserIdOpt: Option[String],
                       sisUserIdOpt: Option[String],
                       userIdBlurb: String) = {
    val mockEnvs = mock[Envs]
    when(mockEnvs.resellerEnvMap).thenReturn(resEnvMap)
    val reqJson = JsObject(toJsonSeq(lmsUserIdOpt, lmsUserIdNm) ++ toJsonSeq(sisUserIdOpt, "sisUserId"))
    val mockOptServ = mock[OptService]
    when(servOpt(mockOptServ, reqJson, env1)).thenReturn(optRet)
    val mockSecServ = mock[SectionService]
    when(mockSecServ.sectionDetails(secId, env1)).thenReturn(secDets)
    val mockDealServ = mock[DealService]
    when(mockDealServ.dealExists(secId, installId, instructorId, productId, lmsUserIdOpt, sisUserIdOpt, env1)).thenReturn(dealRet)
    val optController = new OptController(stubControllerComponents(), mockOptServ, mockDealServ, mockSecServ, mockEnvs)
    val res = opt(optController, rs1, envStr1, sectionIdOpt).apply(FakeRequest().withJsonBody(reqJson))
    val whenPhrase = s" when valid and section ID and $userIdBlurb are included"
    "return OK" + whenPhrase in {
      status(res) mustBe OK
    }
    val respJson = contentAsJson(res)
    "return proper deal" + whenPhrase in {
      (respJson \ "deal").get mustBe Json.parse(dealStr)
    }
    "return proper option body" + whenPhrase in {
      (respJson \ "opt").get mustBe optRespJson
    }
  }

  private def toJsonSeq(strOpt: Option[String], att: String) = strOpt.map(att -> JsString(_)).toSeq
}