package com.kifi.slack.models

import org.joda.time.DateTime
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.util.{ Failure, Success, Try }

final case class SlackAppCredentials(id: SlackAppCredentials.Id, secret: SlackAppCredentials.Secret, commandToken: SlackCommandToken)
object SlackAppCredentials {
  final case class Id(value: String) extends AnyVal
  final case class Secret(value: String) extends AnyVal
}

case class SlackTimestamp(value: String) extends Ordered[SlackTimestamp] { // channel-specific timestamp
  def compare(that: SlackTimestamp) = value compare that.value
  def toDateTime: DateTime = Try {
    new DateTime(value.split('.').head.toLong * 1000) // "The bit before the . is a unix timestamp, the bit after is a sequence to guarantee uniqueness."
  }.getOrElse(throw new Exception(s"Could not parse a date-time out of $value"))
}
object SlackTimestamp {
  implicit val format: Format[SlackTimestamp] = Format(
    Reads { js => js.validate[String].filter(_.contains('.')).map(SlackTimestamp(_)) },
    Writes { x => JsString(x.value) }
  )
}

case class SlackMessageType(value: String)
object SlackMessageType {
  implicit val reads: Reads[SlackMessageType] = Reads(_.validate[String].map(SlackMessageType(_)))
}

case class SlackAttachment(
    fallback: Option[String] = None,
    color: Option[String] = None,
    pretext: Option[String] = None,
    service: Option[String] = None,
    author: Option[SlackAttachment.Author] = None,
    title: Option[SlackAttachment.Title] = None,
    text: Option[String] = None,
    fields: Seq[SlackAttachment.Field] = Seq.empty,
    fromUrl: Option[String] = None,
    imageUrl: Option[String] = None,
    thumbUrl: Option[String] = None,
    markdownIn: Option[Set[String]] = None) {
  def withFullMarkdown = this.copy(markdownIn = Some(Set("text")))
  def withColor(newColor: String) = this.copy(color = Some(newColor))
  def withColorMaybe(colorMaybe: Option[String]) = this.copy(color = colorMaybe)
  def withText(str: String) = this.copy(text = Some(str))
  def withImageUrl(newImageUrl: String) = this.copy(imageUrl = Some(newImageUrl))
}

object SlackAttachment {
  case class Author(name: String, link: Option[String], icon: Option[String])
  case class Title(value: String, link: Option[String])
  case class Field(title: String, value: JsValue, short: Option[Boolean])
  private implicit val authorFormat = Json.format[Author]
  private implicit val titleFormat = Json.format[Title]
  private implicit val fieldFormat = Json.format[Field]

  def applyFromSlack(
    fallback: Option[String],
    color: Option[String],
    pretext: Option[String],
    service: Option[String],
    authorName: Option[String],
    authorLink: Option[String],
    authorIcon: Option[String],
    titleValue: Option[String],
    titleLink: Option[String],
    text: Option[String],
    fields: Option[Seq[SlackAttachment.Field]],
    fromUrl: Option[String],
    imageUrl: Option[String],
    thumbUrl: Option[String],
    markdownIn: Option[Set[String]]): SlackAttachment = {
    val author = authorName.map(Author(_, authorLink, authorIcon))
    val title = titleValue.map(Title(_, titleLink))
    SlackAttachment(fallback, color, pretext, service, author, title, text, fields.getOrElse(Seq.empty), fromUrl, imageUrl, thumbUrl, markdownIn)
  }

  def unapplyToSlack(attachment: SlackAttachment) = Some((
    attachment.fallback: Option[String],
    attachment.color: Option[String],
    attachment.pretext: Option[String],
    attachment.service: Option[String],
    attachment.author.map(_.name): Option[String],
    attachment.author.flatMap(_.link): Option[String],
    attachment.author.flatMap(_.icon): Option[String],
    attachment.title.map(_.value): Option[String],
    attachment.title.flatMap(_.link): Option[String],
    attachment.text: Option[String],
    Some(attachment.fields).filter(_.nonEmpty): Option[Seq[SlackAttachment.Field]],
    attachment.fromUrl: Option[String],
    attachment.imageUrl: Option[String],
    attachment.thumbUrl: Option[String],
    attachment.markdownIn: Option[Set[String]]
  ))

  implicit val format: Format[SlackAttachment] = (
    (__ \ 'fallback).formatNullable[String] and
    (__ \ 'color).formatNullable[String] and
    (__ \ 'pretext).formatNullable[String] and
    (__ \ 'service_name).formatNullable[String] and
    (__ \ 'author_name).formatNullable[String] and
    (__ \ 'author_link).formatNullable[String] and
    (__ \ 'author_icon).formatNullable[String] and
    (__ \ 'title).formatNullable[String] and
    (__ \ 'title_link).formatNullable[String] and
    (__ \ 'text).formatNullable[String] and
    (__ \ 'fields).formatNullable[Seq[SlackAttachment.Field]] and
    (__ \ 'from_url).formatNullable[String] and
    (__ \ 'image_url).formatNullable[String] and
    (__ \ 'thumb_url).formatNullable[String] and
    (__ \ 'mrkdwn_in).formatNullable[Set[String]]
  )(applyFromSlack, unlift(unapplyToSlack))
}

case class SlackMessage(
    messageType: SlackMessageType,
    userId: SlackUserId,
    username: SlackUsername,
    timestamp: SlackTimestamp,
    channel: SlackChannelIdAndName,
    text: String,
    attachments: Seq[SlackAttachment],
    permalink: String,
    originalJson: JsValue)

object SlackMessage {
  private val reads: Reads[SlackMessage] = (
    (__ \ "type").read[SlackMessageType] and
    (__ \ "user").read[SlackUserId] and
    (__ \ "username").read[SlackUsername] and
    (__ \ "ts").read[SlackTimestamp] and
    (__ \ "channel").read[SlackChannelIdAndName] and
    (__ \ "text").read[String] and
    (__ \ "attachments").readNullable[Seq[SlackAttachment]].map[Seq[SlackAttachment]](_ getOrElse Seq.empty) and
    (__ \ "permalink").read[String] and
    __.read[JsValue]
  )(SlackMessage.apply _)

  private val writes: Writes[SlackMessage] = Writes(r => r.originalJson)
  implicit val format: Format[SlackMessage] = Format(reads, writes)

  // Slack has a few isolated cases where they send total garbage instead of a valid message
  // I do not know WHY or HOW this happens, only that it is repeatable
  // We don't want to skip parse errors in general, but these get in the way
  def weKnowWeCannotParse(jsv: JsValue): Boolean = {
    jsv.asOpt[Boolean].isDefined || jsv.asOpt[JsObject].exists { obj =>
      (obj \ "username").asOpt[String].exists(_.isEmpty) &&
        (obj \ "text").asOpt[String].exists(_.isEmpty) &&
        (obj \ "ts").asOpt[SlackTimestamp].exists(_.value == "0000000000.000000")
    }
  }
}

final case class SlackHistoryMessage(
  userId: Option[SlackUserId],
  text: String,
  attachments: Seq[SlackAttachment],
  timestamp: SlackTimestamp,
  originalJson: JsValue)
object SlackHistoryMessage {
  implicit val reads: Reads[SlackHistoryMessage] = (
    (__ \ 'user).readNullable[SlackUserId] and
    (__ \ 'text).read[String] and
    (__ \ 'attachments).readNullable[Seq[SlackAttachment]].map(_ getOrElse Seq.empty) and
    (__ \ 'ts).read[SlackTimestamp] and
    __.read[JsValue]
  )(SlackHistoryMessage.apply _)
}

case class SlackReaction(value: String)
object SlackReaction {
  val checkMark = SlackReaction("heavy_check_mark")
  val robotFace = SlackReaction("robot_face")
}

case class SlackEmoji(value: String)
object SlackEmoji {
  val arrowsCounterclockwise = SlackEmoji(":arrows_counterclockwise:")
  val bee = SlackEmoji(":bee:")
  val books = SlackEmoji(":books:")
  val clipboard = SlackEmoji(":clipboard:")
  val constructionWorker = SlackEmoji(":construction_worker:")
  val fireworks = SlackEmoji(":fireworks:")
  val gear = SlackEmoji(":gear:")
  val hourglass = SlackEmoji(":hourglass:")
  val magnifyingGlass = SlackEmoji(":mag_right:")
  val mailboxWithMail = SlackEmoji(":mailbox_with_mail:")
  val newspaper = SlackEmoji(":newspaper:")
  val pencil = SlackEmoji(":pencil:")
  val robotFace = SlackEmoji(":robot_face:")
  val rocket = SlackEmoji(":rocket:")
  val speechBalloon = SlackEmoji(":speech_balloon:")
  val speakNoEvil = SlackEmoji(":speak_no_evil:")
  val star = SlackEmoji(":star:")
  val sweatSmile = SlackEmoji(":sweat_smile:")
  val wave = SlackEmoji(":wave:")
  val zipperMouthFace = SlackEmoji(":zipper_mouth_face:")
}

sealed abstract class SlackCommand(val value: String)
object SlackCommand {
  case class UnknownSlackCommandException(command: String) extends Exception(s"Unknown Slack command: $command")

  case object Kifi extends SlackCommand("/kifi")
  val all: Seq[SlackCommand] = Seq(Kifi)

  def fromString(commandStr: String): Try[SlackCommand] = {
    all.collectFirst {
      case command if command.value equalsIgnoreCase commandStr => Success(command)
    } getOrElse Failure(UnknownSlackCommandException(commandStr))
  }

  implicit val format = Format[SlackCommand](
    Reads(_.validate[String].flatMap(command => SlackCommand.fromString(command).map(JsSuccess(_)).recover { case error => JsError(error.getMessage) }.get)),
    Writes(command => JsString(command.value))
  )
}

case class SlackCommandToken(value: String)
object SlackCommandToken {
  implicit val format: Format[SlackCommandToken] = Format(Reads(_.validate[String].map(SlackCommandToken(_))), Writes(x => JsString(x.value)))
}
case class SlackCommandRequest(
  token: SlackCommandToken,
  teamId: SlackTeamId,
  teamDomain: SlackTeamDomain,
  channelId: SlackChannelId,
  channelName: SlackChannelName,
  userId: SlackUserId,
  username: SlackUsername,
  command: SlackCommand,
  text: String,
  responseUrl: String)

object SlackCommandRequest {
  implicit val slackReads = (
    (__ \ "token").read[SlackCommandToken] and
    (__ \ "team_id").read[SlackTeamId] and
    (__ \ "team_domain").read[SlackTeamDomain] and
    (__ \ "channel_id").read[SlackChannelId] and
    (__ \ "channel_name").read[SlackChannelName] and
    (__ \ "user_id").read[SlackUserId] and
    (__ \ "user_name").read[SlackUsername] and
    (__ \ "command").read[SlackCommand] and
    (__ \ "text").read[String] and
    (__ \ "response_url").read[String]
  )(apply _)

  case class InvalidSlackCommandRequest(commandForm: Map[String, Seq[String]], missingKey: String) extends Exception(s"Invalid Slack command request, $missingKey not found: $commandForm")

  def fromSlack(commandForm: Map[String, Seq[String]]): Try[SlackCommandRequest] = Try {

    def get(key: String): String = commandForm.get(key).flatMap(_.headOption).getOrElse { throw new InvalidSlackCommandRequest(commandForm, key) }

    SlackCommandRequest(
      SlackCommandToken(get("token")),
      SlackTeamId(get("team_id")),
      SlackTeamDomain(get("team_domain")),
      SlackChannelId(get("channel_id")),
      SlackChannelName(get("channel_name")),
      SlackUserId(get("user_id")),
      SlackUsername(get("user_name")),
      SlackCommand.fromString(get("command")).get,
      get("text"),
      get("response_url")
    )
  }
}

case class SlackCommandResponse(
  responseType: SlackCommandResponse.ResponseType,
  text: String,
  attachments: Seq[SlackAttachment])

object SlackCommandResponse {
  sealed abstract class ResponseType(val value: String)
  object ResponseType {
    case object InChannel extends ResponseType("in_channel")
    case object Ephemeral extends ResponseType("ephemeral")
    implicit val writes = Writes[ResponseType](responseType => JsString(responseType.value))
  }

  implicit val slackWrites = (
    (__ \ "response_type").write[ResponseType] and
    (__ \ "text").write[String] and
    (__ \ "attachments").write[Seq[SlackAttachment]]
  )(unlift(unapply))
}

