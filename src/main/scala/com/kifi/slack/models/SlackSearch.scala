package com.kifi.slack.models

import org.joda.time.LocalDate
import play.api.libs.json.{Json, Reads}
import scala.language.implicitConversions

case class SlackSearchRequest(query: SlackSearchRequest.Query, optional: SlackSearchRequest.Param*)

object SlackSearchRequest {
  sealed abstract class Param(val name: String, val value: Option[String])

  case class Query(query: String) extends Param("query", Some(query))
  object Query {
    val trivial = Query("")
    def apply(queries: Option[Query]*): Query = Query(queries.flatten.map(_.query).mkString(" "))
    def in(channelName: SlackChannelName) = Query(s"in:#${channelName.value.stripPrefix("#").stripPrefix("@")}")
    def from(username: SlackUsername) = Query(s"from:${username.value}")
    def before(date: LocalDate) = Query(s"before:$date")
    def after(date: LocalDate) = Query(s"after:$date")
    val hasLink = Query(s"has:link")

    implicit val reads = Reads.of[String].map(Query(_))
  }

  sealed abstract class Sort(sort: String) extends Param("sort", Some(sort))
  object Sort {
    case object ByScore extends Sort("score")
    case object ByTimestamp extends Sort("timestamp")
  }

  sealed abstract class SortDirection(dir: String) extends Param("sort_dir", Some(dir))
  object SortDirection {
    case object Descending extends SortDirection("desc")
    case object Ascending extends SortDirection("asc")
  }

  object Highlight extends Param("highlight", Some("1"))

  case class Page(page: Int) extends Param("page", Some(page.toString))
  object Page {
    val max = 100
  }

  case class PageSize(count: Int) extends Param("count", Some(count.toString))
  object PageSize {
    val max = 1000
  }
}

case class SlackSearchResponse(query: SlackSearchRequest.Query, messages: SlackSearchResponse.Messages)
object SlackSearchResponse {
  val trivial = SlackSearchResponse(SlackSearchRequest.Query.trivial, SlackSearchResponse.Messages.empty)

  case class Paging(count: Int, total: Int, page: Int, pages: Int)
  object Paging {
    val empty = Paging(0, 0, 0, 0)
    implicit val reads = Json.reads[Paging]
  }
  case class Messages(total: Int, paging: Paging, matches: Seq[SlackMessage])
  object Messages {
    val empty = Messages(0, Paging.empty, Seq.empty)
    implicit val reads = Json.reads[Messages]
  }

  implicit val reads = Json.reads[SlackSearchResponse]
}
