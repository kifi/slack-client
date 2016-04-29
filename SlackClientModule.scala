package com.keepit.slack

import com.google.inject.{ Provides, Singleton }
import com.keepit.common.net.HttpClient
import net.codingwell.scalaguice.ScalaModule

import scala.concurrent.ExecutionContext

trait SlackClientModule extends ScalaModule

case class ProdSlackClientModule() extends SlackClientModule {
  def configure() {}

  @Singleton
  @Provides
  def slackClient(httpClient: HttpClient, ec: ExecutionContext): SlackClient = {
    new SlackClientImpl(httpClient, ec)
  }
}
