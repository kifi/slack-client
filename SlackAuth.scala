package com.keepit.slack.models

import java.util.UUID
import com.keepit.common.cache._
import com.keepit.common.time._
import com.keepit.common.logging.AccessLog
import com.keepit.slack.models.SlackUserPresenceState.{ Away, Active }
import com.kifi.macros.json
import org.joda.time.DateTime
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.concurrent.duration.Duration

case class SlackAuthScope(value: String)
object SlackAuthScope {
  def containsIdentityScope(scopes: Set[SlackAuthScope]): Boolean = scopes.exists(Identity.all.contains)
  def containsAppScope(scopes: Set[SlackAuthScope]): Boolean = scopes.exists(!Identity.all.contains(_))
  def mixesScopes(scopes: Set[SlackAuthScope]): Boolean = containsIdentityScope(scopes) && containsAppScope(scopes)

  object Identity {
    // should not be mixed with other scopes
    val Basic = SlackAuthScope("identity.basic")
    val Email = SlackAuthScope("identity.email")
    val Team = SlackAuthScope("identity.team")
    val Avatar = SlackAuthScope("identity.avatar")

    val all = Set(Basic, Email, Team, Avatar)

    val signup: Set[SlackAuthScope] = Set(Basic, Email, Avatar, Team)
    val login: Set[SlackAuthScope] = Set(Basic)

    private val pilotTeams: Set[SlackTeamId] = Set(
      "T02A81H50", // Kifi
      "T0F5KKU3T", // Kinetic Inc.
      "T0GKSNJKW", // Purple
      "T0FUL04N4", // Brewstercorp
      "T06NHNNJW", // JRuff
      "T0BM8GQDA" // Slack Platform Test Team
    ).map(SlackTeamId(_))

    def areAvailableForTeam(slackTeamId: SlackTeamId): Boolean = pilotTeams.contains(slackTeamId)
  }

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

  val inheritableBotScopes: Set[SlackAuthScope] = Set(Bot, UsersRead, TeamRead, ChannelsRead) // scopes covering APIs that can use a user token or the Kifi bot token transparently
  val ignoredUserScopes: Set[SlackAuthScope] = Set(Bot, IncomingWebhook) // We rely exclusively on the Kifi bot token validity to track bot permissions, not on user scopes. IncomingWebhook asks for a brand new hook.

  val brokenPush: Set[SlackAuthScope] = Set(Commands, ChatWriteBot)
  val newPush: Set[SlackAuthScope] = Set(Commands, IncomingWebhook)
  val ingest: Set[SlackAuthScope] = Set(SearchRead, ReactionsWrite, Commands)
  val integrationSetup = newPush

  val teamSetup = Set(TeamRead)

  val pushToPublicChannels: Set[SlackAuthScope] = Set(ChannelsRead, ChannelsWrite, Bot, Commands)
  val ingestFromPublicChannels: Set[SlackAuthScope] = ingest + ChannelsRead
  val syncPublicChannels = pushToPublicChannels ++ ingestFromPublicChannels

  val pushToPrivateChannels: Set[SlackAuthScope] = Set(GroupsRead, GroupsWrite, Bot, Commands)
  val ingestFromPrivateChannels: Set[SlackAuthScope] = ingest + GroupsRead
  val syncPrivateChannels = pushToPrivateChannels ++ ingestFromPrivateChannels

  val userSignup: Set[SlackAuthScope] = Set(Identify, UsersRead, TeamRead)
  val userLogin: Set[SlackAuthScope] = Set(Identify)

  val slackFormat: Format[Set[SlackAuthScope]] = Format(
    Reads { j => j.validate[String].map(s => s.split(',').toSet.map(SlackAuthScope.apply)) },
    Writes { o => JsString(o.map(_.value).mkString(",")) }
  )
  val dbFormat: Format[SlackAuthScope] = Format(
    Reads { j => j.validate[String].map(SlackAuthScope.apply) },
    Writes { sas => JsString(sas.value) }
  )

  def setFromString(str: String): Set[SlackAuthScope] = str.split(',').filter(_.nonEmpty).map(SlackAuthScope(_)).toSet
  def stringifySet(scopes: Set[SlackAuthScope]) = scopes.map(_.value).mkString(",")
}

@json case class SlackAuthorizationCode(code: String)

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
    (__ \ 'team).read[String].map(SlackTeamName(_)) and
    (__ \ 'user).read[String].map(SlackUsername(_)) and
    (__ \ 'team_id).read[String].map(SlackTeamId(_)) and
    (__ \ 'user_id).read[String].map(SlackUserId(_))
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
      val state = (json \ "presence").asOpt[String] map { stateString =>
        stateString match {
          case Active.name => Active
          case Away.name => Away
        }
      } getOrElse SlackUserPresenceState.Unknown
      val lastActivity = (json \ "last_activity").asOpt[DateTime]
      JsSuccess(SlackUserPresence(state, lastActivity, json))
    }
  }

  val UnknownPresence = SlackUserPresence(SlackUserPresenceState.Unknown, None, JsObject(Seq.empty))

  private val writes: Writes[SlackUserPresence] = Writes(_.originalJson)

  implicit val format: Format[SlackUserPresence] = Format(reads, writes)
}

case class SlackTeamMembersKey(slackTeamId: SlackTeamId) extends Key[Seq[FullSlackUserInfo]] {
  override val version = 3
  val namespace = "slack_team_members"
  def toKey(): String = slackTeamId.value
}

class SlackTeamMembersCache(stats: CacheStatistics, accessLog: AccessLog, innermostPluginSettings: (FortyTwoCachePlugin, Duration), innerToOuterPluginSettings: (FortyTwoCachePlugin, Duration)*)
  extends JsonCacheImpl[SlackTeamMembersKey, Seq[FullSlackUserInfo]](stats, accessLog, innermostPluginSettings, innerToOuterPluginSettings: _*)

case class SlackTeamBotsKey(slackTeamId: SlackTeamId) extends Key[Set[SlackUsername]] {
  override val version = 2
  val namespace = "slack_team_bots"
  def toKey(): String = slackTeamId.value
}

class SlackTeamBotsCache(stats: CacheStatistics, accessLog: AccessLog, innermostPluginSettings: (FortyTwoCachePlugin, Duration), innerToOuterPluginSettings: (FortyTwoCachePlugin, Duration)*)
  extends JsonCacheImpl[SlackTeamBotsKey, Set[SlackUsername]](stats, accessLog, innermostPluginSettings, innerToOuterPluginSettings: _*)

case class SlackTeamMembersCountKey(slackTeamId: SlackTeamId) extends Key[Int] {
  override val version = 3
  val namespace = "slack_team_members_count"
  def toKey(): String = slackTeamId.value
}

class SlackTeamMembersCountCache(stats: CacheStatistics, accessLog: AccessLog, innermostPluginSettings: (FortyTwoCachePlugin, Duration), innerToOuterPluginSettings: (FortyTwoCachePlugin, Duration)*)
  extends PrimitiveCacheImpl[SlackTeamMembersCountKey, Int](stats, accessLog, innermostPluginSettings, innerToOuterPluginSettings: _*)

