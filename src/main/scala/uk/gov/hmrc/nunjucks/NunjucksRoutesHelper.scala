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

import better.files._
import org.mozilla.javascript.{Context, Scriptable}
import play.api.Environment
import play.api.routing.{JavaScriptReverseRoute, JavaScriptReverseRouter}

import java.net.URLClassLoader
import javax.inject.{Inject, Singleton}
import scala.util.Try

trait NunjucksRoutesHelper {

  private val routesInitScript =
    """
      |var routes = {};
      |
      |function isObject(item) {
      |  return (item && typeof item === 'object' && !Array.isArray(item));
      |}
      |
      |function mergeDeep(target, source) {
      |
      |  for (var key in source) {
      |    if (isObject(source[key])) {
      |      if (!target[key]) {
      |        var obj = {};
      |        obj[key] = {};
      |        Object.assign(target, obj);
      |      }
      |      mergeDeep(target[key], source[key]);
      |    } else {
      |      var obj = {};
      |      obj[key] = source[key];
      |      Object.assign(target, obj);
      |    }
      |  }
      |
      |  return target;
      |}
      |""".stripMargin

  def routes: Scriptable = {
    val cx = Context.getCurrentContext

    val configuration: NunjucksConfiguration = cx
      .getThreadLocal("configuration")
      .asInstanceOf[NunjucksConfiguration]

    val scope = {
      val context = Context.enter()
      val scope   = context.initSafeStandardObjects(null, true)
      Context.exit()
      scope
    }

    cx.evaluateString(scope, routesInitScript, "init_routes", 0, null)

    // we need to batch routes as trireme fails to run the script if it's too large
    getRoutes.sliding(100).foreach { batch =>
      val script = JavaScriptReverseRouter("batch", None, configuration.absoluteBaseUrl, batch: _*).toString
      cx.evaluateString(scope, s"(function () { $script; mergeDeep(routes, batch); })();", "batch", 0, null);
    }

    cx.evaluateString(scope, "routes", "routes", 0, null)
      .asInstanceOf[Scriptable]
  }

  def getRoutes: Seq[JavaScriptReverseRoute]
}

@Singleton
class ProductionNunjucksRoutesHelper @Inject() extends NunjucksRoutesHelper {

  lazy val getRoutes: Seq[JavaScriptReverseRoute] =
    Package.getPackages
      .map(_.getName)
      .flatMap(p => Try(Class.forName(s"$p.routes$$javascript").getDeclaredFields).toOption)
      .flatten
      .flatMap { field =>
        val instance   = field.get(null)
        val fieldClass = field.getType

        fieldClass.getDeclaredMethods
          .filter {
            _.getReturnType == classOf[JavaScriptReverseRoute]
          }
          .map { method =>
            method.invoke(instance).asInstanceOf[JavaScriptReverseRoute]
          }
      }
}

class DevelopmentNunjucksRoutesHelper @Inject() (environment: Environment) extends NunjucksRoutesHelper {

  override def getRoutes: Seq[JavaScriptReverseRoute] = {

    val routesUrls = environment.rootPath.toScala.glob("target/*/classes").toList.filter(_.isDirectory).map(_.url)

    val classLoader = new URLClassLoader(routesUrls.toArray, environment.classLoader)

    environment.rootPath.toScala
      .glob("target/*/classes/**/routes.class")
      .toList
      .map(environment.rootPath.toScala.relativize)
      .map(path =>
        path.toString.replaceAll("^target/[^/]+/classes/", "").replaceAll("routes.class$", "").replaceAll("/", ".")
      )
      .flatMap(p => Try(Class.forName(s"${p}routes$$javascript", false, classLoader).getDeclaredFields).toOption)
      .flatten
      .flatMap { field =>
        val instance   = field.get(null)
        val fieldClass = field.getType

        fieldClass.getDeclaredMethods
          .filter {
            _.getReturnType == classOf[JavaScriptReverseRoute]
          }
          .map { method =>
            method.invoke(instance).asInstanceOf[JavaScriptReverseRoute]
          }
      }
  }
}
