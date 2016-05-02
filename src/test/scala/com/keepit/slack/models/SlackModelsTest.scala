package com.keepit.slack.models

import org.joda.time.DateTime
import org.specs2.mutable.Specification
import play.api.libs.json.{JsSuccess, Json}

class SlackModelsTest extends Specification {
  "SlackModels" should {
    "deserialize valid inputs" in {
      "auth.test" in {
        Json.parse(
          """
            |{
            |  "ok": true,
            |  "url": "https://kifi.slack.com/",
            |  "team": "Kifi",
            |  "user": "ryanpbrewster",
            |  "team_id": "T02A81H50",
            |  "user_id": "U054D149J"
            |}
          """.stripMargin).validate[SlackIdentifyResponse] must haveClass[JsSuccess[_]]
      }
      "presence away" in {
        val res = Json.parse(
          """
            |{
            |    "ok": true,
            |    "presence": "away"
            |}
          """.stripMargin).validate[SlackUserPresence].get
        res.state === SlackUserPresenceState.Away
        res.lastActivity === None
      }
      "presence active" in {
        val res = Json.parse(
          """
            |{
            |    "ok": true,
            |    "presence": "active",
            |    "online": true,
            |    "auto_away": false,
            |    "manual_away": false,
            |    "connection_count": 1,
            |    "last_activity": 1457138486
            |}
          """.stripMargin).validate[SlackUserPresence].get
        res.state === SlackUserPresenceState.Active
        res.lastActivity === Some(new DateTime(1457138486L))
      }
      "presence error" in {
        val res = Json.parse(
          """
            |{
            |    "ok": false,
            |    "error": "user_not_found"
            |}
          """.stripMargin).validate[SlackUserPresence].get
        res.state === SlackUserPresenceState.Unknown
        res.lastActivity === None
      }
      "auth.access" in {
        Json.parse( // In principle, posting my personal Slack access token is a bad idea, but I like to live dangerously
          """
            |{
            |  "ok": true,
            |  "access_token": "xoxp-0123456789-0123456789-01234567890-asdfasdfas",
            |  "scope": "read,identify,post,incoming-webhook,search:read,channels:write",
            |  "team_name": "Kifi",
            |  "team_id": "T02A81H50"
            |}
          """.stripMargin).validate[SlackAuthorizationResponse] must haveClass[JsSuccess[_]]
      }
      "search.messages" in {
        Json.parse(
          """
            |{
            |    "ok": true,
            |    "query": "in:#isthispandatime",
            |    "messages": {
            |        "total": 1,
            |        "pagination": {
            |            "total_count": 1,
            |            "page": 1,
            |            "per_page": 20,
            |            "page_count": 1,
            |            "first": 1,
            |            "last": 1
            |        },
            |        "paging": {
            |            "count": 20,
            |            "total": 1,
            |            "page": 1,
            |            "pages": 1
            |        },
            |        "matches": [
            |            {
            |                "type": "message",
            |                "user": "U054D149J",
            |                "username": "ryanpbrewster",
            |                "ts": "1447265230.000005",
            |                "text": "asdf",
            |                "channel": {
            |                    "id": "C0EAK0545",
            |                    "name": "isthispandatime"
            |                },
            |                "permalink": "https://kifi.slack.com/archives/isthispandatime/p1447265230000005"
            |            }
            |        ]
            |    }
            |}
          """.stripMargin).validate[SlackSearchResponse] must haveClass[JsSuccess[_]]
      }
    }
  }
}
