package models

class Env(env: String, paths: Map[String, Seq[String]], jwtFormDataTemplate: Map[String, String], clientId: String,
          clientSecret: String) {
  private val jwtUrlVal = url("jwtUrl", env)
  private val jwtFormDataVal = jwtFormDataTemplate.mapValues(Seq(_)) + ("client_secret" -> Seq(clientSecret)) + ("client_id" -> Seq(clientId))
  private val optUrl = url("optUrl", env)
  private val optinUrlVal = optUrl + "in"
  private val optoutUrlVal = optUrl + "out"
  private val openapi = url("openapi", env)

  def optinUrl: String = optinUrlVal
  def optoutUrl: String = optoutUrlVal
  def jwtUrl: String = jwtUrlVal
  def jwtFormData: Map[String, Seq[String]] = jwtFormDataVal
  def installationIdUrl(sectionId: Long): String = url("installationIdUrl", env, sectionId.toString)
  def sectionDetailsUrl(sectionId: Long): String = url("sectionDetailsUrl", openapi, sectionId.toString)
  def dealExistsUrl(sectionId: Long,
                    installationId: String,
                    instructorId: Long,
                    productId: String,
                    lmsUserIdOpt: Option[String],
                    sisUserIdOpt: Option[String]): String = {
    val baseUrl = url("dealExistsUrl", openapi, sectionId.toString, installationId, instructorId.toString, productId)
    baseUrl + lmsUserIdOpt.fold("")("&lmsUserId=" + _) + sisUserIdOpt.fold("")("&sisUserId=" + _)
  }

  private def url(k: String, r: String*) = StringContext(paths(k): _*).s(r: _*)
}

import javax.inject.Inject
import play.api.Configuration
import pureconfig.loadConfig
import play.api.Logger

class Envs @Inject() (conf: Configuration) {
  private val paths = conf.get[Map[String, Seq[String]]]("paths")
  private val jwtFormData = conf.get[Map[String, String]]("jwtFormData")
  private val resellerConfigs = loadConfig[Seq[ResellerConfig]]("resellers").left.map { srcErrMsg =>
    val errMsg = "reseller configuration is bad: " + srcErrMsg
    Logger.error(errMsg)
    errMsg
  }
  private val resellersOpt = resellerConfigs.map(_.map { resellerConf =>
    resellerConf.name -> resellerConf.envs.map(_.env)
  })
  private val rslrEnvMap = resellerConfigs.map(_.map {
    case ResellerConfig(name, envCreds) =>
      name -> envCreds.map {
        case ResellerEnv(env, clientId, clientSecret) =>
          env -> new Env(env, paths, jwtFormData, clientId, clientSecret)
      }.toMap
  }.toMap)

  // vals wrapped in fns for mocking purposes
  def resellerEnvMap: Either[String, Map[String, Map[String, Env]]] = rslrEnvMap
  def resellerListOpt: Either[String, Seq[(String, Seq[String])]] = resellersOpt
}

private case class ResellerConfig(name: String, envs: Seq[ResellerEnv])
private case class ResellerEnv(env: String, clientId: String, clientSecret: String)