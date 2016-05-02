package com.keepit.slack.models

import java.util.UUID
import com.keepit.slack.models.SlackUserPresenceState.{ Away, Active }
import org.joda.time.DateTime
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class SlackAuthScope(value: String)
object SlackAuthScope {
  val Identify = SlackAuthScope("identify")
  val Bot = SlackAuthScope("bot")
  val Commands = SlackAuthScope("commands")
  val ChannelsWrite = SlackAuthScope("channels:write")
  val ChannelsHistory = SlackAuthScope("channels:history")
  val ChannelsRead = SlackAuthScope("channels:read")
  val ChatWrite = SlackAuthScope("chat:write")
  val ChatWriteBot = SlackAuthScope("chat:write:bot")
  val ChatWriteUser = SlackAuthScope("chat:write:user")
  val EmojiRead = SlackAuthScope("emoji:read")
  val FilesWriteUser = SlackAuthScope("files:write:user")
  val FilesRead = SlackAuthScope("files:read")
  val GroupsWrite = SlackAuthScope("groups:write")
  val GroupsHistory = SlackAuthScope("groups:history")
  val GroupsRead = SlackAuthScope("groups:read")
  val IncomingWebhook = SlackAuthScope("incoming-webhook")
  val ImWrite = SlackAuthScope("im:write")
  val ImHistory = SlackAuthScope("im:history")
  val ImRead = SlackAuthScope("im:read")
  val MpimWrite = SlackAuthScope("mpim:write")
  val MpimHistory = SlackAuthScope("mpim:history")
  val MpimRead = SlackAuthScope("mpim:read")
  val PinsWrite = SlackAuthScope("pins:write")
  val PinsRead = SlackAuthScope("pins:read")
  val ReactionsWrite = SlackAuthScope("reactions:write")
  val ReactionsRead = SlackAuthScope("reactions:read")
  val SearchRead = SlackAuthScope("search:read")
  val StarsWrite = SlackAuthScope("stars:write")
  val StarsRead = SlackAuthScope("stars:read")
  val TeamRead = SlackAuthScope("team:read")
  val UsersRead = SlackAuthScope("users:read")
  val UsersWrite = SlackAuthScope("users:write")

  // scopes covering APIs that can use a human-user token or bot-user token transparently
  val inheritableBotScopes: Set[SlackAuthScope] = Set(Bot, UsersRead, TeamRead, ChannelsRead)

  def setFromString(str: String): Set[SlackAuthScope] = str.split(',').filter(_.nonEmpty).map(SlackAuthScope(_)).toSet
  def stringifySet(scopes: Set[SlackAuthScope]) = scopes.map(_.value).mkString(",")

  val slackFormat: Format[Set[SlackAuthScope]] = Format(
    Reads { j => j.validate[String].map(setFromString) },
    Writes { scopes => JsString(stringifySet(scopes)) }
  )
}

case class SlackAuthorizationCode(code: String)
case class SlackAuthState(state: String)
object SlackAuthState {
  def apply(): SlackAuthState = SlackAuthState(UUID.randomUUID().toString)
}

case class SlackAuthorizationRequest(
  url: String,
  scopes: Set[SlackAuthScope],
  uniqueToken: String,
  redirectUri: Option[String])

case class SlackBotUserAuthorization(
  userId: SlackUserId,
  accessToken: SlackBotAccessToken)
object SlackBotUserAuthorization {
  implicit val reads: Reads[SlackBotUserAuthorization] = (
    (__ \ 'bot_user_id).read[SlackUserId] and
    (__ \ 'bot_access_token).read[SlackBotAccessToken]
  )(SlackBotUserAuthorization.apply _)
}

sealed trait SlackAuthorizationResponse {
  def accessToken: SlackUserAccessToken
  def scopes: Set[SlackAuthScope]
  def teamId: SlackTeamId
}

object SlackAuthorizationResponse {
  implicit val reads: Reads[SlackAuthorizationResponse] = {
    SlackAppAuthorizationResponse.reads.map[SlackAuthorizationResponse](identity) orElse
      SlackIdentityAuthorizationResponse.reads.map[SlackAuthorizationResponse](identity)
  }
}

case class SlackIdentityAuthorizationResponse(
  accessToken: SlackUserAccessToken,
  scopes: Set[SlackAuthScope],
  teamId: SlackTeamId,
  userId: SlackUserId,
  userFullName: String) extends SlackAuthorizationResponse

object SlackIdentityAuthorizationResponse {
  val reads: Reads[SlackIdentityAuthorizationResponse] = (
    (__ \ 'access_token).read[SlackUserAccessToken] and
    (__ \ 'scope).read[Set[SlackAuthScope]](SlackAuthScope.slackFormat) and
    (__ \ 'team \ 'id).read[SlackTeamId] and
    (__ \ 'user \ 'id).read[SlackUserId] and
    (__ \ 'user \ 'name).read[String]
  )(SlackIdentityAuthorizationResponse.apply _)
}

case class SlackAppAuthorizationResponse(
  accessToken: SlackUserAccessToken,
  scopes: Set[SlackAuthScope],
  teamId: SlackTeamId,
  teamName: SlackTeamName,
  incomingWebhook: Option[SlackIncomingWebhook],
  botAuth: Option[SlackBotUserAuthorization]) extends SlackAuthorizationResponse
object SlackAppAuthorizationResponse {
  val reads: Reads[SlackAppAuthorizationResponse] = (
    (__ \ 'access_token).read[SlackUserAccessToken] and
    (__ \ 'scope).read[Set[SlackAuthScope]](SlackAuthScope.slackFormat) and
    (__ \ 'team_id).read[SlackTeamId] and
    (__ \ 'team_name).read[SlackTeamName] and
    (__ \ 'incoming_webhook).readNullable[SlackIncomingWebhook] and
    (__ \ 'bot).readNullable[SlackBotUserAuthorization]
  )(SlackAppAuthorizationResponse.apply _)
}

case class SlackIdentifyResponse(
  url: String,
  teamName: SlackTeamName,
  userName: SlackUsername,
  teamId: SlackTeamId,
  userId: SlackUserId)
object SlackIdentifyResponse {
  implicit val reads: Reads[SlackIdentifyResponse] = (
    (__ \ 'url).read[String] and
    (__ \ 'team).read[SlackTeamName]and
    (__ \ 'user).read[SlackUsername] and
    (__ \ 'team_id).read[SlackTeamId] and
    (__ \ 'user_id).read[SlackUserId]
  )(SlackIdentifyResponse.apply _)
}

case class SlackUserIdentityResponse(user: PartialSlackUserInfo, teamId: SlackTeamId, team: Option[PartialSlackTeamInfo]) {
  def userId = user.id
}
object SlackUserIdentityResponse {
  implicit val reads = (
    (__ \ 'user).read[PartialSlackUserInfo] and
    (__ \ 'team \ 'id).read[SlackTeamId] and
    (__ \ 'team).readNullable[PartialSlackTeamInfo].orElse(Reads.pure(None))
  )(SlackUserIdentityResponse.apply _)
}

sealed abstract class SlackUserPresenceState(val name: String)

object SlackUserPresenceState {
  case object Active extends SlackUserPresenceState("active")
  case object Away extends SlackUserPresenceState("away")
  case object Unknown extends SlackUserPresenceState("unknown")
  case object ERROR extends SlackUserPresenceState("error")
}

case class SlackUserPresence(
  state: SlackUserPresenceState,
  lastActivity: Option[DateTime],
  originalJson: JsValue)

object SlackUserPresence {
  val reads: Reads[SlackUserPresence] = new Reads[SlackUserPresence] {
    def reads(jsVal: JsValue): JsResult[SlackUserPresence] = {
      val json = jsVal.as[JsObject]
      val state = (json \ "presence").asOpt[String] map {
        case Active.name => Active
        case Away.name => Away
      } getOrElse SlackUserPresenceState.Unknown
      val lastActivity = (json \ "last_activity").asOpt[DateTime]
      JsSuccess(SlackUserPresence(state, lastActivity, json))
    }
  }

  val UnknownPresence = SlackUserPresence(SlackUserPresenceState.Unknown, None, JsObject(Seq.empty))
  private val writes: Writes[SlackUserPresence] = Writes(_.originalJson)
  implicit val format: Format[SlackUserPresence] = Format(reads, writes)
}

