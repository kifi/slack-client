package com.kifi.slack.models

import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.mvc.{PathBindable, QueryStringBindable}

case class SlackUserId(value: String) {
  def asChannel: SlackChannelId.User = SlackChannelId.User(value)
}
object SlackUserId {
  implicit val format: Format[SlackUserId] = Format(Reads(_.validate[String].map(SlackUserId(_))), Writes(x => JsString(x.value)))
  implicit val pathBinder = new PathBindable[SlackUserId] {
    override def bind(key: String, value: String): Either[String, SlackUserId] = Right(SlackUserId(value))
    override def unbind(key: String, obj: SlackUserId): String = obj.value
  }

  implicit def queryStringBinder(implicit stringBinder: QueryStringBindable[String]) = new QueryStringBindable[SlackUserId] {
    override def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, SlackUserId]] = {
      stringBinder.bind(key, params) map {
        case Right(value) => Right(SlackUserId(value))
        case _ => Left("Unable to bind a SlackUserId")
      }
    }
    override def unbind(key: String, userId: SlackUserId): String = {
      stringBinder.unbind(key, userId.value)
    }
  }
}
final case class SlackUsername(value: String) {
  def asChannelName = SlackChannelName(this.value)
  def equalsIgnoreCase(that: SlackUsername): Boolean = this.value equalsIgnoreCase that.value
}
object SlackUsername {
  implicit val format: Format[SlackUsername] = Format(Reads(_.validate[String].map(SlackUsername(_))), Writes(x => JsString(x.value)))
}

sealed trait SlackAccessToken {
  def token: String
}
case class SlackUserAccessToken(token: String) extends SlackAccessToken
case class SlackBotAccessToken(token: String) extends SlackAccessToken
object SlackAccessToken {
  implicit val userReads: Reads[SlackUserAccessToken] = Reads(_.validate[String].map(SlackUserAccessToken(_)))
  implicit val botReads: Reads[SlackBotAccessToken] = Reads(_.validate[String].map(SlackBotAccessToken(_)))
}

case class SlackTokenWithScopes(token: SlackUserAccessToken, scopes: Set[SlackAuthScope])

final case class EmailAddress(address: String) extends AnyVal
object EmailAddress {
  implicit val format: Format[EmailAddress] = Format(
    Reads { js => js.validate[String].filter(_.contains('@')).map(EmailAddress(_)) },
    Writes { x => JsString(x.address) }
  )
}

sealed trait SlackUserInfo {
  def id: SlackUserId
  def firstName: Option[String]
  def lastName: Option[String]
  def fullName: Option[String]
  def emailAddress: Option[EmailAddress]
  def avatarUrl: Option[String]
  def bot: Boolean

  def isFull: Boolean
}

object SlackUserInfo {
  private val reads = Reads[SlackUserInfo] { jsValue =>
    FullSlackUserInfo.format.reads(jsValue) orElse
      PartialSlackUserInfo.format.reads(jsValue)
  }

  private val writes: Writes[SlackUserInfo] = Writes {
    case fullInfo: FullSlackUserInfo => FullSlackUserInfo.format.writes(fullInfo)
    case partialInfo: PartialSlackUserInfo => PartialSlackUserInfo.format.writes(partialInfo)
  }

  implicit val format = Format[SlackUserInfo](reads, writes)
}

case class FullSlackUserInfo(
    id: SlackUserId,
    username: SlackUsername,
    profile: FullSlackUserInfo.Profile,
    deleted: Boolean,
    admin: Boolean,
    owner: Boolean,
    bot: Boolean,
    restricted: Boolean,
    originalJson: JsValue) extends SlackUserInfo {
  def firstName: Option[String] = profile.firstName
  def lastName: Option[String] = profile.lastName
  def fullName: Option[String] = profile.fullName
  def emailAddress: Option[EmailAddress] = profile.emailAddress
  def avatarUrl: Option[String] = profile.avatarUrl
  def isFull: Boolean = true
}

object FullSlackUserInfo {
  case class Profile(
    firstName: Option[String],
    lastName: Option[String],
    fullName: Option[String],
    emailAddress: Option[EmailAddress],
    avatarUrl: Option[String],
    originalJson: JsValue)

  private val profileReads: Reads[Profile] = (
    (__ \ 'first_name).readNullable[String].map(_.filter(_.nonEmpty)) and
    (__ \ 'last_name).readNullable[String].map(_.filter(_.nonEmpty)) and
    (__ \ 'real_name).readNullable[String].map(_.filter(_.nonEmpty)) and
    (__ \ 'email).readNullable[EmailAddress] and
    (__ \ "image_original").readNullable[String] and
    Reads(JsSuccess(_))
  )(Profile.apply _)

  private val reads: Reads[FullSlackUserInfo] = (
    (__ \ 'id).read[SlackUserId] and
    (__ \ 'name).read[SlackUsername] and
    (__ \ 'profile).read(profileReads) and
    (__ \ 'deleted).readNullable[Boolean].map(_.contains(true)) and
    (__ \ 'is_admin).readNullable[Boolean].map(_.contains(true)) and
    (__ \ 'is_owner).readNullable[Boolean].map(_.contains(true)) and
    (__ \ 'is_bot).readNullable[Boolean].map(_.contains(true)) and
    (__ \ 'is_restricted).readNullable[Boolean].map(_.contains(true)) and
    Reads(JsSuccess(_))
  )(FullSlackUserInfo.apply _)

  private val writes: Writes[FullSlackUserInfo] = Writes(_.originalJson)

  implicit val format: Format[FullSlackUserInfo] = Format(reads, writes)
}

case class PartialSlackUserInfo(
    id: SlackUserId,
    name: String,
    emailAddress: Option[EmailAddress],
    avatarUrl: Option[String],
    originalJson: JsValue) extends SlackUserInfo {
  def firstName: Option[String] = None
  def lastName: Option[String] = None
  def fullName: Option[String] = Some(name)
  def bot: Boolean = false
  def isFull: Boolean = false
}

object PartialSlackUserInfo {
  private val reads: Reads[PartialSlackUserInfo] = (
    (__ \ 'id).read[SlackUserId] and
    (__ \ 'name).read[String] and
    (__ \ 'email).readNullable[EmailAddress] and
    (__ \ 'image_original).readNullable[String] and
    Reads(JsSuccess(_))
  )(PartialSlackUserInfo.apply _)

  private val writes: Writes[PartialSlackUserInfo] = Writes(_.originalJson)

  implicit val format: Format[PartialSlackUserInfo] = Format(reads, writes)
}
