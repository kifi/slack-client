package com.keepit.slack.models

import play.api.http.Status

import play.api.http.Status._
import play.api.libs.json._

// Failures on our end that deal with slack
sealed abstract class SlackFail(val status: Int, val code: String, val extra: JsValue = JsNull) extends Exception(s"$code: ${Json.stringify(extra)}") {
  import play.api.mvc.Results.Status
  def asResponse = Status(status)(Json.obj("error" -> code, "extra" -> extra))
}
object SlackFail {
  case object NoValidWebhooks extends SlackFail(CONFLICT, "no_valid_webhooks")
  case object NoValidToken extends SlackFail(CONFLICT, "no_valid_token")
  case object NoValidBotToken extends SlackFail(CONFLICT, "no_valid_bot_token")
  case object NoValidPushMethod extends SlackFail(BAD_REQUEST, "no_valid_push_method")
  case object NoAuthCode extends SlackFail(BAD_REQUEST, "no_auth_code")
  case object InvalidAuthState extends SlackFail(BAD_REQUEST, "invalid_auth_state")
  case class SlackUserNotFound(username: SlackUsername, teamId: SlackTeamId) extends SlackFail(NOT_FOUND, "slack_user_not_found", Json.obj("username" -> username, "teamId" -> teamId))
  case class NoSuchMembership(slackTeamId: SlackTeamId, slackUserId: SlackUserId) extends SlackFail(NOT_FOUND, "no_such_membership", Json.obj("team" -> slackTeamId, "user" -> slackUserId))
  case class MalformedPayload(payload: JsValue) extends SlackFail(BAD_REQUEST, "malformed_payload", payload)
  case class MalformedState(state: SlackAuthState) extends SlackFail(BAD_REQUEST, "malformed_state", JsString(state.state))
  case class SlackResponse(response: SlackAPIErrorResponse) extends SlackFail(response.status, response.error, Json.toJson(response))
}

// Failures that Slack can give
case class SlackAPIErrorResponse(status: Int, error: String, payload: JsValue) extends Exception(s"$status response: $error ($payload)")
object SlackAPIErrorResponse {
  implicit val format: Format[SlackAPIErrorResponse] = Json.format[SlackAPIErrorResponse]

  def ApiError(status: Int, payload: JsValue) = SlackAPIErrorResponse(status, (payload \ "error").asOpt[String] getOrElse "api_error", payload)
  val TokenRevoked = SlackAPIErrorResponse(Status.NOT_FOUND, SlackErrorCode.TOKEN_REVOKED, JsNull)
  val WebhookRevoked = SlackAPIErrorResponse(Status.NOT_FOUND, SlackErrorCode.WEBHOOK_REVOKED, JsNull)
}
object SlackErrorCode {
  def unapply(fail: SlackAPIErrorResponse) = Some(fail.error)
  val ACCOUNT_INACTIVE = "account_inactive"
  val ALREADY_REACTED = "already_reacted"
  val CANT_UPDATE_MESSAGE = "cant_update_message"
  val CHANNEL_NOT_FOUND = "channel_not_found"
  val EDIT_WINDOW_CLOSED = "edit_window_closed"
  val INVALID_AUTH = "invalid_auth"
  val IS_ARCHIVED = "is_archived"
  val MESSAGE_NOT_FOUND = "message_not_found"
  val NOT_IN_CHANNEL = "not_in_channel"
  val RESTRICTED_ACTION = "restricted_action"
  val TOKEN_REVOKED = "token_revoked"
  val WEBHOOK_REVOKED = "webhook_revoked"
}

