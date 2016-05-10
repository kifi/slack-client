package com.kifi.slack

import java.net.URLEncoder

import com.kifi.slack.models._
import com.kifi.slack.models.SlackAppCredentials.Id
import play.api.http.Status
import play.api.libs.json._
import play.api.libs.ws.ning.NingWSClient
import scala.language.implicitConversions

import scala.concurrent.{ExecutionContext, Future}

object SlackAPI {
  final case class Param(key: String, value: String)
  final case class Route(path: String, params: Param*) {
    def url: String = path + "?" + params.map {
      case Param(k, v) => URLEncoder.encode(k, "UTF-8") + "=" + URLEncoder.encode(v, "UTF-8")
    }.mkString
  }

  val OK: String = "ok"
  val NoService: String = "No service"
  object SlackParams {
    implicit def fromId(id: Id): Param = Param("client_id", id.value)
    implicit def fromSecret(secret: SlackAppCredentials.Secret): Param = Param("client_secret", secret.value)
    implicit def fromTuple(kv: (String, String)): Param = Param(kv._1, kv._2)
    implicit def fromTupleOpt(kv: (String, Option[String])): Param = Param(kv._1, kv._2 getOrElse "")
    implicit def fromInt(kv: (String, Int)): Param = Param(kv._1, kv._2.toString)
    implicit def fromCode(code: SlackAuthorizationCode): Param = Param("code", code.code)
    implicit def formState(state: SlackAuthState): Param = Param("state", state.state)
    implicit def fromScope(scopes: Set[SlackAuthScope]): Param = Param("scope", scopes.map(_.value).mkString(","))
    implicit def fromToken(token: SlackAccessToken): Param = Param("token", token.token)
    implicit def fromChannelId(channelId: SlackChannelId): Param = Param("channel", channelId.value)
    implicit def fromTimestamp(timestamp: SlackTimestamp): Param = Param("ts", timestamp.value)
    implicit def fromUserId(userId: SlackUserId): Param = Param("user", userId.value)
    implicit def fromSearchParam(searchParam: SlackSearchRequest.Param): Param = Param(searchParam.name, searchParam.value getOrElse "")
  }

  import SlackParams._
  def Test(token: SlackAccessToken) = Route("https://slack.com/api/api.test", token)
  def OAuthAuthorize(id: SlackAppCredentials.Id, scopes: Set[SlackAuthScope], state: SlackAuthState, teamId: Option[SlackTeamId], redirectUri: String) =
    Route("https://slack.com/oauth/authorize", id, scopes, state, "redirect_uri" -> redirectUri, "team" -> teamId.map(_.value))
  def OAuthAccess(id: SlackAppCredentials.Id, secret: SlackAppCredentials.Secret, code: SlackAuthorizationCode, redirectUri: String) =
    Route("https://slack.com/api/oauth.access", id, secret, code, "redirect_uri" -> redirectUri)
  def Identify(token: SlackAccessToken) =
    Route("https://slack.com/api/auth.test", token)
  def UserIdentity(token: SlackAccessToken) =
    Route("https://slack.com/api/users.identity", token)
  def SearchMessages(token: SlackAccessToken, request: SlackSearchRequest) = {
    val params = Seq[Param](token, request.query) ++ request.optional.map(fromSearchParam)
    Route("https://slack.com/api/search.messages", params: _*)
  }
  def ChannelsList(token: SlackAccessToken, excludeArchived: Boolean) =
    Route("https://slack.com/api/channels.list", token, "exclude_archived" -> (if (excludeArchived) 1 else 0))
  def ChannelInfo(token: SlackAccessToken, channelId: SlackChannelId) =
    Route("https://slack.com/api/channels.info", token, channelId)
  def GroupsList(token: SlackAccessToken, excludeArchived: Boolean) =
    Route("https://slack.com/api/groups.list", token, "exclude_archived" -> (if (excludeArchived) 1 else 0))
  def GroupInfo(token: SlackAccessToken, channelId: SlackChannelId) =
    Route("https://slack.com/api/groups.info", token, channelId)
  def AddReaction(token: SlackAccessToken, reaction: SlackReaction, channelId: SlackChannelId, messageTimestamp: SlackTimestamp) =
    Route("https://slack.com/api/reactions.add", token, "name" -> reaction.value, "channel" -> channelId.value, "timestamp" -> messageTimestamp.value)
  def PostMessage(token: SlackAccessToken, channelId: SlackChannelId, msg: SlackMessageRequest) =
    Route("https://slack.com/api/chat.postMessage", Seq[Param](token, channelId) ++ msg.asUrlParams: _*)
  def UpdateMessage(token: SlackAccessToken, channelId: SlackChannelId, timestamp: SlackTimestamp, msg: SlackMessageUpdateRequest) =
    Route("https://slack.com/api/chat.update", Seq[Param](token, channelId, timestamp) ++ msg.asUrlParams: _*)
  def DeleteMessage(token: SlackAccessToken, channelId: SlackChannelId, timestamp: SlackTimestamp) =
    Route("https://slack.com/api/chat.delete", Seq[Param](token, channelId, timestamp): _*)
  def TeamInfo(token: SlackAccessToken) =
    Route("https://slack.com/api/team.info", token)
  def UserPresence(token: SlackAccessToken, userId: SlackUserId) =
    Route("https://slack.com/api/users.getPresence", token, userId)
  def UserInfo(token: SlackAccessToken, userId: SlackUserId) =
    Route("https://slack.com/api/users.info", token, userId)
  def UsersList(token: SlackAccessToken) =
    Route("https://slack.com/api/users.list", token)
  def IMList(token: SlackAccessToken) =
    Route("https://slack.com/api/im.list", token)
  def IMHistory(token: SlackAccessToken, channelId: SlackChannelId, fromTimestamp: Option[SlackTimestamp], limit: Int, inclusive: Boolean) =
    Route("https://slack.com/api/im.history", token, channelId, "oldest" -> fromTimestamp.map(_.value).getOrElse("0"), "count" -> limit, "inclusive" -> (if (inclusive) 1 else 0))
  def InviteToChannel(token: SlackAccessToken, userId: SlackUserId, channelId: SlackChannelId) =
    Route("https://slack.com/api/channels.invite", token, userId, channelId)
}

trait SlackClient {
  def pushToWebhook(url: String, msg: SlackMessageRequest): Future[Unit]
  def postToChannel(token: SlackAccessToken, channelId: SlackChannelId, msg: SlackMessageRequest): Future[SlackMessageResponse]
  def processAuthorizationResponse(code: SlackAuthorizationCode, redirectUri: String): Future[SlackAuthorizationResponse]
  def updateMessage(token: SlackAccessToken, channelId: SlackChannelId, timestamp: SlackTimestamp, newMsg: SlackMessageUpdateRequest): Future[SlackMessageResponse]
  def deleteMessage(token: SlackAccessToken, channelId: SlackChannelId, timestamp: SlackTimestamp): Future[Unit]
  def testToken(token: SlackAccessToken): Future[Unit]
  def identifyUser(token: SlackAccessToken): Future[SlackIdentifyResponse]
  def getUserIdentity(token: SlackAccessToken): Future[SlackUserIdentityResponse]
  def searchMessages(token: SlackAccessToken, request: SlackSearchRequest): Future[SlackSearchResponse]
  def addReaction(token: SlackAccessToken, reaction: SlackReaction, channelId: SlackChannelId, messageTimestamp: SlackTimestamp): Future[Unit]
  def getTeamInfo(token: SlackAccessToken): Future[FullSlackTeamInfo]
  def getPublicChannels(token: SlackAccessToken, excludeArchived: Boolean): Future[Seq[SlackPublicChannelInfo]]
  def getPublicChannelInfo(token: SlackAccessToken, channelId: SlackChannelId): Future[SlackPublicChannelInfo]
  def getPrivateChannels(token: SlackAccessToken, excludeArchived: Boolean): Future[Seq[SlackPrivateChannelInfo]]
  def getPrivateChannelInfo(token: SlackAccessToken, channelId: SlackChannelId): Future[SlackPrivateChannelInfo]
  def getUserInfo(token: SlackAccessToken, userId: SlackUserId): Future[FullSlackUserInfo]
  def getUsers(token: SlackAccessToken): Future[Seq[FullSlackUserInfo]]
  def getIMChannels(token: SlackAccessToken): Future[Seq[SlackIMChannelInfo]]
  def getIMHistory(token: SlackAccessToken, channelId: SlackChannelId, fromTimestamp: Option[SlackTimestamp], limit: Int, inclusive: Boolean = false): Future[Seq[SlackHistoryMessage]]
  def checkUserPresence(token: SlackAccessToken, user: SlackUserId): Future[SlackUserPresence]
  def inviteToChannel(token: SlackAccessToken, invitee: SlackUserId, channelId: SlackChannelId): Future[Unit]
}

class SlackClientImpl(
  app: Option[SlackAppCredentials],
  httpClient: NingWSClient,
  implicit val ec: ExecutionContext)
    extends SlackClient {

  def pushToWebhook(url: String, msg: SlackMessageRequest): Future[Unit] = {
    httpClient
      .url(url)
      .post(Json.toJson(msg))
      .map { wsResponse =>
      (wsResponse.status, wsResponse.body) match {
        case (Status.OK, SlackAPI.OK) => Future.successful(())
        case (Status.NOT_FOUND, SlackAPI.NoService) => Future.failed(SlackAPIErrorResponse.TokenRevoked)
        case (status, payload) => Future.failed(SlackAPIErrorResponse.ApiError(status, JsString(payload)))
      }
    }
  }

  def postToChannel(token: SlackAccessToken, channelId: SlackChannelId, msg: SlackMessageRequest): Future[SlackMessageResponse] = {
    slackCall[SlackMessageResponse](SlackAPI.PostMessage(token, channelId, msg))
  }

  private def slackCall[T](route: SlackAPI.Route)(implicit reads: Reads[T]): Future[T] = {
    httpClient
      .url(route.path)
      .get().flatMap { wsResponse =>
      (wsResponse.status, wsResponse.json) match {
        case (Status.OK, payload) if (payload \ "ok").asOpt[Boolean].contains(true) =>
          reads.reads(payload).map(Future.successful).getOrElse {
            Future.failed(SlackFail.MalformedPayload(payload))
          }
        case (status, payload) => Future.failed(SlackAPIErrorResponse.ApiError(status, payload))
      }
    }
  }

  def processAuthorizationResponse(code: SlackAuthorizationCode, redirectUri: String): Future[SlackAuthorizationResponse] = {
    app.map {
      case SlackAppCredentials(id, secret, _) => slackCall[SlackAuthorizationResponse](SlackAPI.OAuthAccess(id, secret, code, redirectUri))
    }.getOrElse(Future.failed(SlackFail.NoAppCredentials))
  }

  def updateMessage(token: SlackAccessToken, channelId: SlackChannelId, timestamp: SlackTimestamp, newMsg: SlackMessageUpdateRequest): Future[SlackMessageResponse] = {
    slackCall[SlackMessageResponse](SlackAPI.UpdateMessage(token, channelId, timestamp, newMsg))
  }
  def deleteMessage(token: SlackAccessToken, channelId: SlackChannelId, timestamp: SlackTimestamp): Future[Unit] = {
    slackCall[Unit](SlackAPI.DeleteMessage(token, channelId, timestamp))(Reads(_ => JsSuccess(())))
  }
  def testToken(token: SlackAccessToken): Future[Unit] = {
    slackCall[Unit](SlackAPI.Test(token))(Reads(_ => JsSuccess(())))
  }

  def searchMessages(token: SlackAccessToken, request: SlackSearchRequest): Future[SlackSearchResponse] = {
    slackCall[SlackSearchResponse](SlackAPI.SearchMessages(token, request))
  }

  def identifyUser(token: SlackAccessToken): Future[SlackIdentifyResponse] = {
    slackCall[SlackIdentifyResponse](SlackAPI.Identify(token))
  }

  def getUserIdentity(token: SlackAccessToken): Future[SlackUserIdentityResponse] = {
    slackCall[SlackUserIdentityResponse](SlackAPI.UserIdentity(token))
  }

  def addReaction(token: SlackAccessToken, reaction: SlackReaction, channelId: SlackChannelId, messageTimestamp: SlackTimestamp): Future[Unit] = {
    slackCall[Unit](SlackAPI.AddReaction(token, reaction, channelId, messageTimestamp))(Reads(_ => JsSuccess(())))
  }

  def getTeamInfo(token: SlackAccessToken): Future[FullSlackTeamInfo] = {
    slackCall[FullSlackTeamInfo](SlackAPI.TeamInfo(token))((__ \ 'team).read)
  }

  def checkUserPresence(token: SlackAccessToken, user: SlackUserId): Future[SlackUserPresence] = {
    slackCall[SlackUserPresence](SlackAPI.UserPresence(token, user))
  }

  def getPublicChannels(token: SlackAccessToken, excludeArchived: Boolean): Future[Seq[SlackPublicChannelInfo]] = {
    slackCall[Seq[SlackPublicChannelInfo]](SlackAPI.ChannelsList(token, excludeArchived))((__ \ 'channels).read)
  }

  def getPublicChannelInfo(token: SlackAccessToken, channelId: SlackChannelId): Future[SlackPublicChannelInfo] = {
    slackCall[SlackPublicChannelInfo](SlackAPI.ChannelInfo(token, channelId))((__ \ 'channel).read)
  }

  def getPrivateChannels(token: SlackAccessToken, excludeArchived: Boolean): Future[Seq[SlackPrivateChannelInfo]] = {
    slackCall[Seq[SlackPrivateChannelInfo]](SlackAPI.GroupsList(token, excludeArchived))((__ \ 'groups).read)
  }

  def getPrivateChannelInfo(token: SlackAccessToken, channelId: SlackChannelId): Future[SlackPrivateChannelInfo] = {
    slackCall[SlackPrivateChannelInfo](SlackAPI.GroupInfo(token, channelId))((__ \ 'group).read)
  }

  def getUserInfo(token: SlackAccessToken, userId: SlackUserId): Future[FullSlackUserInfo] = {
    slackCall[FullSlackUserInfo](SlackAPI.UserInfo(token, userId))((__ \ 'user).read)
  }

  def getUsers(token: SlackAccessToken): Future[Seq[FullSlackUserInfo]] = {
    slackCall[Seq[FullSlackUserInfo]](SlackAPI.UsersList(token))((__ \ 'members).read)
  }
  def getIMChannels(token: SlackAccessToken): Future[Seq[SlackIMChannelInfo]] = {
    slackCall[Seq[SlackIMChannelInfo]](SlackAPI.IMList(token))((__ \ 'ims).read)
  }
  def getIMHistory(token: SlackAccessToken, channelId: SlackChannelId, fromTimestamp: Option[SlackTimestamp], limit: Int, inclusive: Boolean = false): Future[Seq[SlackHistoryMessage]] = {
    slackCall[Seq[SlackHistoryMessage]](SlackAPI.IMHistory(token, channelId, fromTimestamp, limit, inclusive))((__ \ 'messages).read)
  }

  def inviteToChannel(token: SlackAccessToken, invitee: SlackUserId, channelId: SlackChannelId): Future[Unit] = {
    slackCall[Unit](SlackAPI.InviteToChannel(token, invitee, channelId))(Reads(_ => JsSuccess(())))
  }
}
