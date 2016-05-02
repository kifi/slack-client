package com.keepit.slack.models

import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.mvc.{PathBindable, QueryStringBindable}

case class SlackTeamId(value: String)
object SlackTeamId {
  implicit val format: Format[SlackTeamId] = Format(Reads(_.validate[String].map(SlackTeamId(_))), Writes(x => JsString(x.value)))
  implicit val pathBinder = new PathBindable[SlackTeamId] {
    override def bind(key: String, value: String): Either[String, SlackTeamId] = Right(SlackTeamId(value))
    override def unbind(key: String, obj: SlackTeamId): String = obj.value
  }

  implicit def queryStringBinder(implicit stringBinder: QueryStringBindable[String]) = new QueryStringBindable[SlackTeamId] {
    override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, SlackTeamId]] = {
      stringBinder.bind(key, params) map {
        case Right(value) => Right(SlackTeamId(value))
        case _ => Left("Unable to bind a SlackTeamId")
      }
    }
    override def unbind(key: String, teamId: SlackTeamId): String = {
      stringBinder.unbind(key, teamId.value)
    }
  }
}
case class SlackTeamName(value: String)
object SlackTeamName {
  implicit val format: Format[SlackTeamName] = Format(Reads(_.validate[String].map(SlackTeamName(_))), Writes(x => JsString(x.value)))
}
case class SlackTeamEmailDomain(value: String)

case class SlackTeamDomain(value: String)
object SlackTeamDomain {
  implicit val format = Format[SlackTeamDomain](
    Reads { value =>
      value.validate[String].map {
        domainStr => if (domainStr.endsWith(".slack.com")) SlackTeamDomain(domainStr) else SlackTeamDomain(domainStr + ".slack.com")
      }
    },
    Writes(domain => JsString(domain.value))
  )
}

object SlackTeamIconReads extends Reads[Map[Int, String]] {
  private val sizePattern = """^image_(\d+)$""".r
  def reads(value: JsValue) = value.validate[JsObject].map { obj =>
    val isDefaultImage = (obj \ "image_default").asOpt[Boolean].getOrElse(false)
    if (isDefaultImage) Map.empty[Int, String] else obj.value.collect { case (sizePattern(size), JsString(imageUrl)) => size.toInt -> imageUrl }.toMap
  }
}

sealed trait SlackTeamInfo {
  def id: SlackTeamId
  def name: SlackTeamName
}

object SlackTeamInfo {
  implicit val slackReads = Reads[SlackTeamInfo] { jsValue =>
    FullSlackTeamInfo.slackReads.reads(jsValue) orElse
      PartialSlackTeamInfo.slackReads.reads(jsValue)
  }
}

case class FullSlackTeamInfo(
  id: SlackTeamId,
  name: SlackTeamName,
  domain: SlackTeamDomain,
  emailDomains: Seq[SlackTeamEmailDomain],
  icon: Map[Int, String]) extends SlackTeamInfo

object FullSlackTeamInfo {
  implicit val slackReads: Reads[FullSlackTeamInfo] = (
    (__ \ 'id).read[SlackTeamId] and
    (__ \ 'name).read[SlackTeamName] and
    (__ \ 'domain).read[SlackTeamDomain] and
    (__ \ 'email_domain).read[String].map(domains => domains.split(',').toList.map(_.trim).collect { case domain if domain.nonEmpty => SlackTeamEmailDomain(domain) }) and
    (__ \ 'icon).read(SlackTeamIconReads)
  )(FullSlackTeamInfo.apply _)
}

case class PartialSlackTeamInfo(id: SlackTeamId, name: SlackTeamName, domain: SlackTeamDomain, icon: Map[Int, String]) extends SlackTeamInfo
object PartialSlackTeamInfo {
  implicit val slackReads: Reads[PartialSlackTeamInfo] = (
    (__ \ 'id).read[SlackTeamId] and
    (__ \ 'name).read[SlackTeamName] and
    (__ \ 'domain).read[SlackTeamDomain] and
    SlackTeamIconReads
  )(PartialSlackTeamInfo.apply _)
}

case class BasicSlackTeamInfo(id: SlackTeamId, name: SlackTeamName) extends SlackTeamInfo