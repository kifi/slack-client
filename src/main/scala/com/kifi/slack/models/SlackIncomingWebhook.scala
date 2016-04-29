package com.kifi.slack.models

import com.kifi.slack.SlackAPI.Param
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class SlackIncomingWebhook(
  channelId: SlackChannelId,
  channelName: SlackChannelName,
  url: String,
  configUrl: String)
object SlackIncomingWebhook {
  implicit val reads: Reads[SlackIncomingWebhook] = (
    (__ \ 'channel_id).read[SlackChannelId] and
    (__ \ 'channel).read[SlackChannelName] and
    (__ \ 'url).read[String] and
    (__ \ 'configuration_url).read[String]
  )(SlackIncomingWebhook.apply _)
}

final case class SlackMessageRequest( // https://api.slack.com/incoming-webhooks
    text: String,
    username: String,
    iconUrl: String,
    attachments: Seq[SlackAttachment],
    asUser: Boolean,
    unfurlLinks: Boolean,
    unfurlMedia: Boolean,
    parseMode: String) {
  def quiet = this.copy(unfurlLinks = false, unfurlMedia = false)
  def fromUser = this.copy(asUser = true)
  def withAttachments(newAttachments: Seq[SlackAttachment]) = this.copy(attachments = newAttachments)
  def asUrlParams: Seq[Param] = {
    import com.kifi.slack.SlackAPI.SlackParams._
    Seq[Param](
      "text" -> text,
      "attachments" -> Json.stringify(Json.toJson(attachments)),
      "username" -> username,
      "icon_url" -> iconUrl,
      "as_user" -> asUser.toString,
      "unfurl_links" -> unfurlLinks.toString,
      "unfurl_media" -> unfurlMedia.toString,
      "parse" -> parseMode
    )
  }
}

object SlackMessageRequest {
  implicit val format: Format[SlackMessageRequest] = (
    (__ \ 'text).format[String] and
    (__ \ 'username).format[String] and
    (__ \ 'icon_url).format[String] and
    (__ \ 'attachments).format[Seq[SlackAttachment]] and
    (__ \ 'as_user).format[Boolean] and
    (__ \ 'unfurl_links).format[Boolean] and
    (__ \ 'unfurl_media).format[Boolean] and
    (__ \ 'parse).format[String]
  )(SlackMessageRequest.apply, unlift(SlackMessageRequest.unapply))
}

final case class SlackMessageUpdateRequest(
    text: String,
    attachments: Seq[SlackAttachment],
    unfurlLinks: Boolean,
    unfurlMedia: Boolean,
    parseMode: String) {
  def asUrlParams: Seq[Param] = {
    import com.kifi.slack.SlackAPI.SlackParams._
    Seq(
      "text" -> text,
      "attachments" -> Json.stringify(Json.toJson(attachments)),
      "unfurl_links" -> unfurlLinks.toString,
      "unfurl_media" -> unfurlMedia.toString,
      "parse" -> parseMode
    )
  }
}

object SlackMessageUpdateRequest {
  def fromMessageRequest(msg: SlackMessageRequest): SlackMessageUpdateRequest = SlackMessageUpdateRequest(
    text = msg.text,
    attachments = msg.attachments,
    unfurlLinks = msg.unfurlLinks,
    unfurlMedia = msg.unfurlMedia,
    parseMode = msg.parseMode
  )
}

case class SlackMessageResponse(
    slackChannelId: SlackChannelId,
    timestamp: SlackTimestamp,
    text: String,
    originalJson: JsValue) {
  def sentByAnonymousBot = {
    (originalJson \ "message" \ "user").asOpt[String].isEmpty &&
      (originalJson \ "message" \ "bot_id").asOpt[String].exists(_.nonEmpty)
  }
}
object SlackMessageResponse {
  implicit val reads: Reads[SlackMessageResponse] = (
    (__ \ 'channel).read[SlackChannelId] and
    (__ \ 'ts).read[SlackTimestamp] and
    (__ \ 'message \ 'text).read[String] and
    __.read[JsValue]
  )(SlackMessageResponse.apply _)
}
