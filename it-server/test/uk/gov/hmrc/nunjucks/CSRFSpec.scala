/*
 * Copyright 2021 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.nunjucks

import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.{FreeSpec, MustMatchers, OptionValues}
import play.api.inject.guice.GuiceApplicationBuilder
import play.api.test.CSRFTokenHelper._
import play.api.test.FakeRequest
import play.filters.csrf.CSRF

import scala.xml.XML

class CSRFSpec extends FreeSpec with MustMatchers
  with ScalaFutures with IntegrationPatience with OptionValues {

  "CSRF" - {

    lazy val app = new GuiceApplicationBuilder()

    "must render a hidden input with the relevant CSRF token" in {

      val request = FakeRequest()
        .withCSRFToken

      val token = request.attrs.get(CSRF.Token.InfoAttr).value.toToken

      val renderer = app.injector.instanceOf[NunjucksRenderer]
      val result = renderer.render("csrf.njk")(request).futureValue

      val xml = XML.loadString(result.toString)
      xml mustEqual <input value={token.value} name={token.name} type="hidden"/>
    }
  }
}
