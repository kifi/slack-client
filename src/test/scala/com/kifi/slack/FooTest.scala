package com.kifi.slack

import org.specs2.mutable.Specification
import play.api.libs.json._

class FooTest extends Specification {
  "this test" should {
    "do anything" in {
      1 === 1
    }
    "do json things" in {
      Json.obj("1" -> "one", "2" -> "two") === Json.parse("""{"1":"one","2":"two"}""")
    }
  }
}

