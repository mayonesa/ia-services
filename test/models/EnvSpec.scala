package models

import org.scalatestplus.play.PlaySpec

class EnvSpec extends PlaySpec {
  private val jwtUrl1 = "jwt_url_1"
  private val jwtUrl2 = "jwt_url_2"
  private val optUrl1 = "opt_url_1"
  private val optUrl2 = "opt_url_2"
  private val oa1 = "oq1"
  private val oa2 = "oq2"
  private val iiu1 = "iiu1"
  private val iiu2 = "iiu2"
  private val iiu3 = "iiu3"
  private val sdu1 = "sdu1"
  private val sdu2 = "sdu2"
  private val sdu3 = "sdu3"
  private val deu1 = "deu1"
  private val deu2 = "deu2"
  private val deu3 = "deu3"
  private val deu4 = "deu4"
  private val deu5 = "deu5"
  private val deu6 = "deu6"
  private val paths = Map("jwtUrl" -> Seq(jwtUrl1, jwtUrl2),
                          "optUrl" -> Seq(optUrl1, optUrl2),
                          "openapi" -> Seq(oa1, oa2),
                          "installationIdUrl" -> Seq(iiu1, iiu2, iiu3),
                          "sectionDetailsUrl" -> Seq(sdu1, sdu2, sdu3),
                          "dealExistsUrl" -> Seq(deu1, deu2, deu3, deu4, deu5, deu6))
  private val cc = "cc"
  private val pfu = "pfu"
  private val jwtFormDataTemplate = Map("grant_type" -> cc, "scope" -> pfu)
  private val clientId = "client_id"
  private val clientSecret = "cs"
  private val envStr = "env1"
  private val env1 = new Env(envStr, paths, jwtFormDataTemplate, clientId, clientSecret)
  private val sectionId = 1L
  private val openapi = oa1 + envStr + oa2

  "Env.optinUrl" should {
    "return optin URL" in {
      env1.optinUrl mustBe optUrl1 + envStr + optUrl2 + "in"
    }
  }

  "Env.optoutUrl" should {
    "return optout URL" in {
      env1.optoutUrl mustBe optUrl1 + envStr + optUrl2 + "out"
    }
  }

  "Env.jwtUrl" should {
    "return jwt URL" in {
      env1.jwtUrl mustBe jwtUrl1 + envStr + jwtUrl2
    }
  }

  "Env.jwtFormData" should {
    "return jwt form data" in {
      env1.jwtFormData mustBe jwtFormDataTemplate.mapValues(Seq(_)) + ("client_secret" -> Seq(clientSecret)) +
        ("client_id" -> Seq(clientId))
    }
  }

  "Env.installationIdUrl" should {
    "return installation ID URL" in {
      env1.installationIdUrl(sectionId) mustBe iiu1 + envStr + iiu2 + sectionId + iiu3
    }
  }

  "Env.sectionDetailsUrl" should {
    "return section details URL" in {
      env1.sectionDetailsUrl(sectionId) mustBe sdu1 + openapi + sdu2 + sectionId + sdu3
    }
  }

  "Env.dealExistsUrl" should {
    val installationId = "installId"
    val instructorId = 6L
    val productId = "pId"
    val lmsUserId = "luId"
    val sisUserId = "suId"

    dealExistsUrl(Some(lmsUserId), Some(sisUserId), "LMS and SIS user ID")
    dealExistsUrl(Some(lmsUserId), None, "LMS user ID")
    dealExistsUrl(None, Some(sisUserId), "SIS user ID")

    def dealExistsUrl(lmsUserIdOpt: Option[String], sisUserIdOpt: Option[String], whenClause: String) =
      "return deal-exists URL, given " + whenClause in {
        env1.dealExistsUrl(sectionId, installationId, instructorId, productId, lmsUserIdOpt, sisUserIdOpt) mustBe
          deu1 + openapi + deu2 + sectionId + deu3 + installationId + deu4 + instructorId + deu5 + productId + deu6 + 
          lmsUserIdOpt.fold("")("&lmsUserId=" + _) + sisUserIdOpt.fold("")("&sisUserId=" + _)
      }
  }
}