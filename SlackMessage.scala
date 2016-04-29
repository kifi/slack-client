package com.keepit.slack.models

import com.keepit.common.cache.{ CacheStatistics, FortyTwoCachePlugin, JsonCacheImpl, Key }
import com.keepit.common.db.Id
import com.keepit.common.strings.AbbreviateString
import com.keepit.common.logging.AccessLog
import com.keepit.common.reflection.Enumerator
import com.keepit.common.strings.StringWithReplacements
import com.keepit.common.util.DescriptionElements
import com.keepit.model.KeepAttributionType._
import com.keepit.model.{ Library, LibrarySpace }
import com.kifi.macros.json
import org.joda.time.DateTime
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.concurrent.duration.Duration
import scala.util.{ Failure, Success, Try }

object KifiSlackApp {
  val SLACK_CLIENT_ID = "2348051170.15031499078"
  val SLACK_CLIENT_SECRET = "ad688ad730192eabe0bdc6675975f3fc"
  val SLACK_COMMAND_TOKEN = SlackCommandToken("g4gyK5XEFCDm4RqgsyjGKPCD")

  val BrewstercorpTeamId = SlackTeamId("T0FUL04N4")
  val KifiSlackTeamId = SlackTeamId("T02A81H50")
}

@json case class SlackTimestamp(value: String) extends Ordered[SlackTimestamp] { // channel-specific timestamp
  def compare(that: SlackTimestamp) = value compare that.value
  def toDateTime: DateTime = Try {
    new DateTime(value.split('.').head.toLong * 1000) // "The bit before the . is a unix timestamp, the bit after is a sequence to guarantee uniqueness."
  }.getOrElse(throw new Exception(s"Could not parse a date-time out of $value"))
}

@json case class SlackMessageType(value: String)

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
  @json case class Field(title: String, value: JsValue, short: Option[Boolean])

  def simple(text: DescriptionElements): SlackAttachment = {
    SlackAttachment(
      text = Some(DescriptionElements.formatForSlack(text)),
      fallback = Some(DescriptionElements.formatPlain(text).abbreviate(40))
    ).withFullMarkdown
  }

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

  import com.keepit.common.json.ReadIfPossible._
  implicit val format: Format[SlackAttachment] = (
    (__ \ 'fallback).formatIfPossible[String] and
    (__ \ 'color).formatIfPossible[String] and
    (__ \ 'pretext).formatIfPossible[String] and
    (__ \ 'service_name).formatIfPossible[String] and
    (__ \ 'author_name).formatIfPossible[String] and
    (__ \ 'author_link).formatIfPossible[String] and
    (__ \ 'author_icon).formatIfPossible[String] and
    (__ \ 'title).formatIfPossible[String] and
    (__ \ 'title_link).formatIfPossible[String] and
    (__ \ 'text).formatIfPossible[String] and
    (__ \ 'fields).formatIfPossible[Seq[SlackAttachment.Field]] and
    (__ \ 'from_url).formatIfPossible[String] and
    (__ \ 'image_url).formatIfPossible[String] and
    (__ \ 'thumb_url).formatIfPossible[String] and
    (__ \ 'mrkdwn_in).formatIfPossible[Set[String]]
  )(applyFromSlack, unlift(unapplyToSlack))
}

case class SlackMessage(
    messageType: SlackMessageType,
    userId: SlackUserId,
    username: SlackUsername,
    timestamp: SlackTimestamp,
    channel: SlackChannelIdAndName,
    rawText: String,
    attachments: Seq[SlackAttachment],
    permalink: String,
    originalJson: JsValue) {
  def text: String = rawText.replaceAllLiterally("&amp;" -> "&")
  // NB: the below line is technically more correct, but our ingestion relies on "dumb" regular expressions that may break
  // if we unescape the angle-brackets. If we decide that this is a relevant concern, fix the regexs and switch to that line
  // def text: String = rawText.replaceAllLiterally("&lt;" -> "<", "&gt;" -> ">", "&amp;" -> "&")
}

object SlackMessage {
  private val reads: Reads[SlackMessage] = (
    (__ \ "type").read[SlackMessageType] and
    (__ \ "user").read[SlackUserId] and
    (__ \ "username").read[SlackUsername] and
    (__ \ "ts").read[SlackTimestamp] and
    (__ \ "channel").read[SlackChannelIdAndName] and
    (__ \ "text").read[String] and
    (__ \ "attachments").readNullable[Seq[SlackAttachment]].map[Seq[SlackAttachment]](_.getOrElse(Seq.empty)) and
    (__ \ "permalink").read[String] and
    Reads(JsSuccess(_))
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

@json
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
object SlackCommand extends Enumerator[SlackCommand] {
  case class UnknownSlackCommandException(command: String) extends Exception(s"Unknown Slack command: $command")

  case object Kifi extends SlackCommand("/kifi")
  def all = _all

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

@json
case class SlackCommandToken(value: String)

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

@json
case class SlackChannelIntegrations(
  teamId: SlackTeamId,
  channelId: SlackChannelId,
  allLibraries: Set[Id[Library]],
  toLibraries: Set[Id[Library]],
  fromLibraries: Set[Id[Library]])

object SlackChannelIntegrations {
  def none(teamId: SlackTeamId, channelId: SlackChannelId) = SlackChannelIntegrations(teamId, channelId, Set.empty, Set.empty, Set.empty)
}

case class SlackChannelIntegrationsKey(teamId: SlackTeamId, channelId: SlackChannelId) extends Key[SlackChannelIntegrations] {
  override val version = 3
  val namespace = "slack_channel_integrations"
  def toKey(): String = s"${teamId.value}-${channelId.value}"
}

class SlackChannelIntegrationsCache(stats: CacheStatistics, accessLog: AccessLog, innermostPluginSettings: (FortyTwoCachePlugin, Duration), innerToOuterPluginSettings: (FortyTwoCachePlugin, Duration)*)
  extends JsonCacheImpl[SlackChannelIntegrationsKey, SlackChannelIntegrations](stats, accessLog, innermostPluginSettings, innerToOuterPluginSettings: _*)
