/*
 * Copyright 2022 HM Revenue & Customs
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

import play.api.data.Form
import play.api.i18n.Messages
import play.api.libs.json.{Json, OWrites}

trait NunjucksSupport {

  protected implicit def formOWrites[A](implicit messages: Messages): OWrites[Form[A]] =
    OWrites { form =>
      form.mapping.mappings
        .map { m =>
          form.apply(m.key)
        }
        .foldLeft(Json.obj()) { (obj, field) =>
          val error = field.error
            .map { error =>
              Json.obj(
                "error" ->
                  Json.obj("text" -> messages(error.message, error.args: _*))
              )
            }
            .getOrElse(Json.obj())

          obj ++ Json.obj(
            field.name ->
              (Json.obj("value" -> field.value) ++ error)
          )
        } ++ Json.obj(
        "errors" -> form.errors.map { error =>
          Json.obj(
            "text" -> messages(error.message, error.args: _*),
            "href" -> ("#" + form(error.key).id)
          )
        }
      )
    }
}
