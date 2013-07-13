package controllers

import play.api._
import play.api.mvc._
import play.api.libs.ws.WS
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.json.{JsArray, JsObject, Json}
import java.net
import java.net.URLEncoder
import scala.concurrent.Future
import play.api.libs.iteratee.{Enumerator, Iteratee}
import play.api.libs.concurrent.Promise
import com.codahale.jerkson

object Application extends Controller {
  
  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

  def checkPreFlight = Action {
    Ok.withHeaders(
      "Access-Control-Allow-Origin" -> "*",   // Need to add the correct domain in here!!
      "Access-Control-Allow-Methods" -> "POST", // Only allow POST
      "Access-Control-Max-Age" -> "300",  // Cache response for 5 minutes
      "Access-Control-Allow-Headers" -> "Origin, X-Requested-With, Content-Type, Accept" // Ensure this header is also allowed!
    )
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
    Async {
      val bad = Promise.pure(BadRequest())
      val data = jerkson.Json.parse[Seq[Seq[Seq[Int]]]](request.body)
      val positionStokes: Seq[Seq[Position]] = data flatMap {stroke =>
        //x array and y array
        stroke match {
          case x::y::Nil =>
            x.length == y.length match {
              case true =>
                val positions = (x zip y) map {case (x, y) => Position(x, y)}
                Some(positions)
              case false => None
            }
          case _ => None
        }
      }
      bad
    }
  }

  case class Position(x: Int, y: Int)

  def latex(positionStorkes: Seq[Seq[Position]]): Future[String] = {
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
      "components" -> makeXYsJson(positions)
      )))),
      "apiKey" -> Seq("f3469740-d247-11e1-acbf-0025648c5362")
      )
    }
    response map {resp =>
      resp.body
    }
  }

  private def makeXYsJson(positionStorkes: Seq[Seq[Position]]): Seq[JsObject] = positionStorkes map { stroke =>
    val (xs, ys) = stroke map {pos =>
      (pos.x,pos.y)
    }
    Json.obj(
      "y" -> Json.arr(pos.y),
      "x" -> Json.arr(pos.x),
      "type" -> "stroke"
    )
  }}
  
}