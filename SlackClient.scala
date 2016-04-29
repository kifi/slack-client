package com.keepit.slack

import com.keepit.common.core._
import com.keepit.common.json.readUnit
import com.keepit.common.logging.Logging
import com.keepit.common.net._
import com.keepit.slack.models._
import play.api.http.Status
import play.api.libs.json._

import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Failure, Success }

object SlackAPI {
  import com.keepit.common.routes.{ GET, Method, Param, ServiceRoute }

  case class Route(method: Method, path: String, params: Param*)
  implicit def toServiceRoute(route: Route): ServiceRoute = ServiceRoute(route.method, route.path, route.params: _*)

  val OK: String = "ok"
  val NoService: String = "No service"
  object SlackParams {
    val CLIENT_ID = Param("client_id", KifiSlackApp.SLACK_CLIENT_ID)
    val CLIENT_SECRET = Param("client_secret", KifiSlackApp.SLACK_CLIENT_SECRET)
    implicit def fromCode(code: SlackAuthorizationCode): Param = Param("code", code.code)
    implicit def formState(state: SlackAuthState): Param = Param("state", state.state)
    implicit def fromScope(scopes: Set[SlackAuthScope]): Param = Param("scope", scopes.map(_.value).mkString(","))
    implicit def fromToken(token: SlackAccessToken): Param = Param("token", token.token)
    implicit def fromChannelId(channelId: SlackChannelId): Param = Param("channel", channelId.value)
    implicit def fromTimestamp(timestamp: SlackTimestamp): Param = Param("ts", timestamp.value)
    implicit def fromUserId(userId: SlackUserId): Param = Param("user", userId.value)
    implicit def fromSearchParam(searchParam: SlackSearchRequest.Param): Param = Param(searchParam.name, searchParam.value)
  }

  import com.keepit.slack.SlackAPI.SlackParams._

  def Test(token: SlackAccessToken) = Route(GET, "https://slack.com/api/api.test", token)
  def OAuthAuthorize(scopes: Set[SlackAuthScope], state: SlackAuthState, teamId: Option[SlackTeamId], redirectUri: String) = Route(GET, "https://slack.com/oauth/authorize", CLIENT_ID, scopes, state, "redirect_uri" -> redirectUri, "team" -> teamId.map(_.value))
  def OAuthAccess(code: SlackAuthorizationCode, redirectUri: String) = Route(GET, "https://slack.com/api/oauth.access", CLIENT_ID, CLIENT_SECRET, code, "redirect_uri" -> redirectUri)
  def Identify(token: SlackAccessToken) = Route(GET, "https://slack.com/api/auth.test", token)
  def UserIdentity(token: SlackAccessToken) = Route(GET, "https://slack.com/api/users.identity", token)
  def SearchMessages(token: SlackAccessToken, request: SlackSearchRequest) = {
    val params = Seq[Param](token, request.query) ++ request.optional.map(fromSearchParam)
    Route(GET, "https://slack.com/api/search.messages", params: _*)
  }
  def ChannelsList(token: SlackAccessToken, excludeArchived: Boolean) = Route(GET, "https://slack.com/api/channels.list", token, "exclude_archived" -> (if (excludeArchived) 1 else 0))
  def ChannelInfo(token: SlackAccessToken, channelId: SlackChannelId) = Route(GET, "https://slack.com/api/channels.info", token, channelId)
  def GroupsList(token: SlackAccessToken, excludeArchived: Boolean) = Route(GET, "https://slack.com/api/groups.list", token, "exclude_archived" -> (if (excludeArchived) 1 else 0))
  def GroupInfo(token: SlackAccessToken, channelId: SlackChannelId) = Route(GET, "https://slack.com/api/groups.info", token, channelId)
  def AddReaction(token: SlackAccessToken, reaction: SlackReaction, channelId: SlackChannelId, messageTimestamp: SlackTimestamp) = Route(GET, "https://slack.com/api/reactions.add", token, "name" -> reaction.value, "channel" -> channelId.value, "timestamp" -> messageTimestamp.value)
  def PostMessage(token: SlackAccessToken, channelId: SlackChannelId, msg: SlackMessageRequest) =
    Route(GET, "https://slack.com/api/chat.postMessage", Seq[Param](token, channelId) ++ msg.asUrlParams: _*)
  def UpdateMessage(token: SlackAccessToken, channelId: SlackChannelId, timestamp: SlackTimestamp, msg: SlackMessageUpdateRequest) =
    Route(GET, "https://slack.com/api/chat.update", Seq[Param](token, channelId, timestamp) ++ msg.asUrlParams: _*)
  def DeleteMessage(token: SlackAccessToken, channelId: SlackChannelId, timestamp: SlackTimestamp) =
    Route(GET, "https://slack.com/api/chat.delete", Seq[Param](token, channelId, timestamp): _*)
  def TeamInfo(token: SlackAccessToken) = Route(GET, "https://slack.com/api/team.info", token)
  def UserPresence(token: SlackAccessToken, userId: SlackUserId) = Route(GET, "https://slack.com/api/users.getPresence", token, userId)
  def UserInfo(token: SlackAccessToken, userId: SlackUserId) = Route(GET, "https://slack.com/api/users.info", token, userId)
  def UsersList(token: SlackAccessToken) = Route(GET, "https://slack.com/api/users.list", token)
  def IMList(token: SlackAccessToken) = Route(GET, "https://slack.com/api/im.list", token)
  def IMHistory(token: SlackAccessToken, channelId: SlackChannelId, fromTimestamp: Option[SlackTimestamp], limit: Int, inclusive: Boolean) =
    Route(GET, "https://slack.com/api/im.history", token, channelId, "oldest" -> fromTimestamp.map(_.value).getOrElse("0"), "count" -> limit, "inclusive" -> (if (inclusive) 1 else 0))
  def InviteToChannel(token: SlackAccessToken, userId: SlackUserId, channelId: SlackChannelId) = Route(GET, "https://slack.com/api/channels.invite", token, userId, channelId)
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

object SlackClient {
  val longTimeout = CallTimeouts(responseTimeout = Some(30000), maxWaitTime = Some(30000), maxJsonParseTime = Some(30000))
  val standardTimeout = CallTimeouts(responseTimeout = Some(10000), maxWaitTime = Some(10000), maxJsonParseTime = Some(10000))
}

class SlackClientImpl(
  httpClient: HttpClient,
  implicit val ec: ExecutionContext)
    extends SlackClient with Logging {

  def pushToWebhook(url: String, msg: SlackMessageRequest): Future[Unit] = {
    log.info(s"About to post $msg to the Slack webhook at $url")
    httpClient.postFuture(DirectUrl(url), Json.toJson(msg)).flatMap { clientResponse =>
      (clientResponse.status, clientResponse.body) match {
        case (Status.OK, SlackAPI.OK) => Future.successful(())
        case (Status.NOT_FOUND, SlackAPI.NoService) => Future.failed(SlackAPIErrorResponse.TokenRevoked)
        case (status, payload) => Future.failed(SlackAPIErrorResponse.ApiError(status, JsString(payload)))
      }
    }.recoverWith {
      case f: NonOKResponseException =>
        log.error(s"Caught a non-OK response exception to $url, recognizing that it's a revoked webhook")
        Future.failed(SlackAPIErrorResponse.WebhookRevoked)
    }.andThen {
      case Success(_) => log.info(s"[SLACK-CLIENT] Succeeded in pushing to webhook $url")
      case Failure(f) => log.error(s"[SLACK-CLIENT] Failed to post to webhook $url because $f")
    }
  }

  def postToChannel(token: SlackAccessToken, channelId: SlackChannelId, msg: SlackMessageRequest): Future[SlackMessageResponse] = {
    slackCall[SlackMessageResponse](SlackAPI.PostMessage(token, channelId, msg)).recoverWith {
      case f: NonOKResponseException => Future.failed(SlackAPIErrorResponse(f.response.status, f.response.body, JsString(f.response.body)))
    }.andThen {
      case Success(_) => log.info(s"[SLACK-CLIENT] Succeeded in pushing to $channelId via token $token")
      case Failure(f) => log.error(s"[SLACK-CLIENT] Failed to post to $channelId via token $token because of ${f.getMessage}")
    }
  }

  private def slackCall[T](route: SlackAPI.Route, callTimeouts: CallTimeouts = SlackClient.standardTimeout)(implicit reads: Reads[T]): Future[T] = {
    httpClient.withTimeout(callTimeouts).getFuture(DirectUrl(route.url)).flatMap { clientResponse =>
      (clientResponse.status, clientResponse.json) match {
        case (Status.OK, payload) if (payload \ "ok").asOpt[Boolean].contains(true) =>
          reads.reads(payload) match {
            case JsSuccess(res, _) => Future.successful(res)
            case errs: JsError => Future.failed(SlackFail.MalformedPayload(payload))
          }
        case (status, payload) => Future.failed(SlackAPIErrorResponse.ApiError(status, payload))
      }
    }
  }

  def processAuthorizationResponse(code: SlackAuthorizationCode, redirectUri: String): Future[SlackAuthorizationResponse] = {
    slackCall[SlackAuthorizationResponse](SlackAPI.OAuthAccess(code, redirectUri))
  }

  def updateMessage(token: SlackAccessToken, channelId: SlackChannelId, timestamp: SlackTimestamp, newMsg: SlackMessageUpdateRequest): Future[SlackMessageResponse] = {
    slackCall[SlackMessageResponse](SlackAPI.UpdateMessage(token, channelId, timestamp, newMsg))
  }
  def deleteMessage(token: SlackAccessToken, channelId: SlackChannelId, timestamp: SlackTimestamp): Future[Unit] = {
    slackCall[Unit](SlackAPI.DeleteMessage(token, channelId, timestamp))(readUnit)
  }
  def testToken(token: SlackAccessToken): Future[Unit] = {
    slackCall[Unit](SlackAPI.Test(token))(readUnit)
  }

  def searchMessages(token: SlackAccessToken, request: SlackSearchRequest): Future[SlackSearchResponse] = {
    slackCall[SlackSearchResponse](SlackAPI.SearchMessages(token, request), SlackClient.longTimeout)
  }

  def identifyUser(token: SlackAccessToken): Future[SlackIdentifyResponse] = {
    slackCall[SlackIdentifyResponse](SlackAPI.Identify(token))
  }

  def getUserIdentity(token: SlackAccessToken): Future[SlackUserIdentityResponse] = {
    slackCall[SlackUserIdentityResponse](SlackAPI.UserIdentity(token))
  }

  def addReaction(token: SlackAccessToken, reaction: SlackReaction, channelId: SlackChannelId, messageTimestamp: SlackTimestamp): Future[Unit] = {
    slackCall[JsValue](SlackAPI.AddReaction(token, reaction, channelId, messageTimestamp)).imap(_ => ())
  }

  def getTeamInfo(token: SlackAccessToken): Future[FullSlackTeamInfo] = {
    slackCall[FullSlackTeamInfo](SlackAPI.TeamInfo(token))((__ \ 'team).read)
  }

  def checkUserPresence(token: SlackAccessToken, user: SlackUserId): Future[SlackUserPresence] = {
    slackCall[SlackUserPresence](SlackAPI.UserPresence(token, user))
  }

  def getPublicChannels(token: SlackAccessToken, excludeArchived: Boolean): Future[Seq[SlackPublicChannelInfo]] = {
    slackCall[Seq[SlackPublicChannelInfo]](SlackAPI.ChannelsList(token, excludeArchived), SlackClient.longTimeout)((__ \ 'channels).read)
  }

  def getPublicChannelInfo(token: SlackAccessToken, channelId: SlackChannelId): Future[SlackPublicChannelInfo] = {
    slackCall[SlackPublicChannelInfo](SlackAPI.ChannelInfo(token, channelId))((__ \ 'channel).read)
  }

  def getPrivateChannels(token: SlackAccessToken, excludeArchived: Boolean): Future[Seq[SlackPrivateChannelInfo]] = {
    slackCall[Seq[SlackPrivateChannelInfo]](SlackAPI.GroupsList(token, excludeArchived), SlackClient.longTimeout)((__ \ 'groups).read)
  }

  def getPrivateChannelInfo(token: SlackAccessToken, channelId: SlackChannelId): Future[SlackPrivateChannelInfo] = {
    slackCall[SlackPrivateChannelInfo](SlackAPI.GroupInfo(token, channelId))((__ \ 'group).read)
  }

  def getUserInfo(token: SlackAccessToken, userId: SlackUserId): Future[FullSlackUserInfo] = {
    slackCall[FullSlackUserInfo](SlackAPI.UserInfo(token, userId))((__ \ 'user).read)
  }

  def getUsers(token: SlackAccessToken): Future[Seq[FullSlackUserInfo]] = {
    slackCall[Seq[FullSlackUserInfo]](SlackAPI.UsersList(token), SlackClient.longTimeout)((__ \ 'members).read)
  }
  def getIMChannels(token: SlackAccessToken): Future[Seq[SlackIMChannelInfo]] = {
    slackCall[Seq[SlackIMChannelInfo]](SlackAPI.IMList(token))((__ \ 'ims).read)
  }
  def getIMHistory(token: SlackAccessToken, channelId: SlackChannelId, fromTimestamp: Option[SlackTimestamp], limit: Int, inclusive: Boolean = false): Future[Seq[SlackHistoryMessage]] = {
    slackCall[Seq[SlackHistoryMessage]](SlackAPI.IMHistory(token, channelId, fromTimestamp, limit, inclusive))((__ \ 'messages).read)
  }

  def inviteToChannel(token: SlackAccessToken, invitee: SlackUserId, channelId: SlackChannelId): Future[Unit] = {
    slackCall[Unit](SlackAPI.InviteToChannel(token, invitee, channelId))(readUnit)
  }
}
