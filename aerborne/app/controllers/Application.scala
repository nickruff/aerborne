package controllers

import play.api._
import play.api.mvc._
import play.api.libs.ws.WS
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json.{JsArray, JsObject, Json}
import java.net
import java.net.{URLDecoder, URLEncoder}
import scala.concurrent.Future
import play.api.libs.iteratee.{Enumerator, Iteratee}
import play.api.libs.concurrent.Promise
import com.codahale.jerkson

object Application extends Controller {
  
  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

  def eventSocket = WebSocket.using[String] { request =>
    // Log events to the console
    val in = Iteratee.foreach[String](content =>
      Logger.debug(content)
    ).mapDone { _ =>
      //WE have disconnected
    }

    // Send a single 'Hello!' message
    val out = Enumerator.imperative[String]()
    (in, out)
  }

  def evaluatePosSequence = Action(parse.tolerantText) { implicit request =>
    Logger.debug(URLDecoder.decode(request.body))
    Async {
        try {
          val strokes = jerkson.Json.parse[Seq[Seq[Position]]](request.body)
          latex(strokes) map {s => Ok(s)}
        } catch {
          case e: Exception =>
            Logger.warn(e.toString)
            Promise.pure(BadRequest("None of the strokes provided were valid"))
        }
    }
  }

  case class Position(x: Int, y: Int)

  def latex(strokes: Seq[Seq[Position]]): Future[String] = {
    val url = "http://webdemo.visionobjects.com/webservices/api/myscript/v2.0/equation/doSimpleRecognition.json"
    val cookie = "WebDemoPortalWarning=1; __utma=182204193.640085604.1373662884.1373662884.1373662884.1; __utmb=182204193.3.10.1373662884; __utmc=182204193; __utmz=182204193.1373662884.1.1.utmcsr=google|utmccn=(organic)|utmcmd=organic|utmctr=(not%20provided)"
    val response = WS.url(url)
      .withHeaders(
    "Host" -> "webdemo.visionobjects.com",
    "Connection" -> "keep-alive",
    "Accept" -> "equation/json",
    "Origin" -> "http://webdemo.visionobjects.com",
    "User-Agent" -> "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_8_3) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/27.0.1453.116 Safari/537.36",
    "Referer" -> "http://webdemo.visionobjects.com/",
    "Cookie" -> cookie
    ).post{
      Map("application" -> Seq("webdemo.equation"),
      "equationInput" -> Seq((Json.stringify(Json.obj(
      "resultTypes" -> Json.arr("LATEX"),
      "components" -> makeXYsJson(strokes)
      )))),
      "apiKey" -> Seq("f3469740-d247-11e1-acbf-0025648c5362")
      )
    }
    response map {resp =>
      resp.body
    }
  }

  private def makeXYsJson(strokes: Seq[Seq[Position]]): Seq[JsObject] = strokes map { stroke =>
    val (xs, ys) = stroke.map {pos =>
      (pos.x, pos.y)
    }.unzip
    Json.obj(
      "y" -> ys,
      "x" -> xs,
      "type" -> "stroke"
    )
  }
  
}