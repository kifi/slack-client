package com.kifi.slack.models

import org.joda.time.DateTime
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.reflect.ClassTag
import scala.util.{ Failure, Success, Try }

sealed abstract class SlackChannelId(prefix: String) {
  require(value.startsWith(prefix), s"Invalid ${this.getClass.getSimpleName}: $value")
  def value: String
}

object SlackChannelId {
  case class Public(value: String) extends SlackChannelId("C")
  case class Private(value: String) extends SlackChannelId("G")
  case class DM(value: String) extends SlackChannelId("D")
  case class User(value: String) extends SlackChannelId("U")

  private def parseChannelId(value: String): Try[SlackChannelId] = value.headOption match {
    case Some('C') => Success(Public(value))
    case Some('G') => Success(Private(value))
    case Some('D') => Success(DM(value))
    case Some('U') => Success(User(value))
    case _ => Failure(new IllegalArgumentException(s"Invalid SlackChannelId: $value"))
  }


  def apply(value: String): SlackChannelId = parseChannelId(value).get

  def parse[T <: SlackChannelId](value: String)(implicit m: ClassTag[T]): Try[T] = parseChannelId(value) match {
    case Success(channelId: T) => Success(channelId)
    case _ => Failure(new IllegalArgumentException(s"Invalid ${m.runtimeClass.getSimpleName} channel id: $value"))
  }

  implicit def format[T <: SlackChannelId](implicit m: ClassTag[T]) = Format[T](
    Reads(_.validate[String].flatMap(parse[T](_).map(JsSuccess(_)).recover { case error => JsError(error.getMessage) }.get)),
    Writes(channelId => JsString(channelId.value))
  )

  def isPublic(channelId: SlackChannelId): Boolean = channelId match {
    case Public(_) => true
    case _ => false
  }
}

case class SlackChannelName(value: String)
object SlackChannelName {
  private val reads = Reads[SlackChannelName](_.validate[String].map(value => SlackChannelName(value.stripPrefix("#").stripPrefix("@"))))
  private val writes = Writes[SlackChannelName](name => JsString(name.value))
  implicit val format = Format(reads, writes)
}

case class SlackChannelTopic(value: String)
object SlackChannelTopic {
  implicit val reads: Reads[SlackChannelTopic] = Reads(_.validate[String].map(SlackChannelTopic(_)))
}
case class SlackChannelPurpose(value: String)
object SlackChannelPurpose {
  implicit val reads: Reads[SlackChannelPurpose] = Reads(_.validate[String].map(SlackChannelPurpose(_)))
}

case class SlackChannelIdAndName(id: SlackChannelId, name: SlackChannelName)
object SlackChannelIdAndName {
  implicit val reads = Json.reads[SlackChannelIdAndName]
}
case class SlackChannelIdAndPrettyName(id: SlackChannelId, name: Option[SlackChannelName])
object SlackChannelIdAndPrettyName {
  def from(channelId: SlackChannelId, name: SlackChannelName): SlackChannelIdAndPrettyName = channelId match {
    case SlackChannelId.Public(_) if !name.value.startsWith("#") => SlackChannelIdAndPrettyName(channelId, Some(name.copy(value = "#" + name.value)))
    case SlackChannelId.Public(_) | SlackChannelId.Private(_) => SlackChannelIdAndPrettyName(channelId, Some(name))
    case _ => SlackChannelIdAndPrettyName(channelId, None)
  }

  def from(channelIdAndName: SlackChannelIdAndName): SlackChannelIdAndPrettyName = SlackChannelIdAndPrettyName.from(channelIdAndName.id, channelIdAndName.name)
}

sealed trait SlackChannelInfo {
  def createdAt: DateTime
  def channelId: SlackChannelId
  def channelName: SlackChannelName
  def channelIdAndName = SlackChannelIdAndName(channelId, channelName)
  def members: Set[SlackUserId]
}

// There is more stuff than just this returned
case class SlackPublicChannelInfo(
    channelId: SlackChannelId.Public,
    channelName: SlackChannelName,
    creator: SlackUserId,
    createdAt: DateTime,
    isArchived: Boolean,
    isGeneral: Boolean,
    members: Set[SlackUserId],
    topic: Option[SlackChannelTopic],
    purpose: Option[SlackChannelPurpose]) extends SlackChannelInfo
object SlackPublicChannelInfo {
  implicit val reads: Reads[SlackPublicChannelInfo] = (
    (__ \ 'id).read[SlackChannelId.Public] and
    (__ \ 'name).read[SlackChannelName] and
    (__ \ 'creator).read[SlackUserId] and
    (__ \ 'created).read[DateTime] and
    (__ \ 'is_archived).read[Boolean] and
    (__ \ 'is_general).read[Boolean] and
    (__ \ 'members).read[Set[SlackUserId]] and
    (__ \ 'topic \ 'value).readNullable[SlackChannelTopic].map(_.filter(_.value.nonEmpty)) and
    (__ \ 'purpose \ 'value).readNullable[SlackChannelPurpose].map(_.filter(_.value.nonEmpty))
  )(SlackPublicChannelInfo.apply _)
}

// There is more stuff than just this returned
case class SlackPrivateChannelInfo(
  channelId: SlackChannelId.Private,
  channelName: SlackChannelName,
  creator: SlackUserId,
  createdAt: DateTime,
  isArchived: Boolean,
  isMultipartyDM: Boolean,
  members: Set[SlackUserId],
  topic: Option[SlackChannelTopic],
  purpose: Option[SlackChannelPurpose]) extends SlackChannelInfo
object SlackPrivateChannelInfo {
  implicit val reads: Reads[SlackPrivateChannelInfo] = (
    (__ \ 'id).read[SlackChannelId.Private] and
    (__ \ 'name).read[SlackChannelName] and
    (__ \ 'creator).read[SlackUserId] and
    (__ \ 'created).read[DateTime] and
    (__ \ 'is_archived).read[Boolean] and
    (__ \ 'is_mpim).read[Boolean] and
    (__ \ 'members).read[Set[SlackUserId]] and
    (__ \ 'topic \ 'value).readNullable[SlackChannelTopic].map(_.filter(_.value.nonEmpty)) and
    (__ \ 'purpose \ 'value).readNullable[SlackChannelPurpose].map(_.filter(_.value.nonEmpty))
  )(SlackPrivateChannelInfo.apply _)
}

// There is more stuff than just this returned
final case class SlackIMChannelInfo(
  channelId: SlackChannelId.DM,
  userId: SlackUserId,
  originalJson: JsValue)

object SlackIMChannelInfo {
  implicit val reads: Reads[SlackIMChannelInfo] = (
    (__ \ 'id).read[SlackChannelId.DM] and
    (__ \ 'user).read[SlackUserId] and
    __.read[JsValue]
  )(SlackIMChannelInfo.apply _)
}
