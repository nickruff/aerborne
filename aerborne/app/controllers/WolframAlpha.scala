package controllers

/**
 * Created with IntelliJ IDEA.
 * User: bala
 * Date: 7/13/13
 * Time: 4:51 AM
 * To change this template use File | Settings | File Templates.
 */

import com.wolfram.alpha._
import play.api.Logger
import scala.Some
import play.api.libs.json.Json

class WolframAlpha {
  val engine = new WAEngine
  engine.setAppID("LAJ6HJ-E9YQEA8VAG")
  engine.addFormat("plaintext")

  val fail = Seq(ResultPod("Sorry bro", "No one understand what you're saying"))

  implicit val resultPodFmt = Json.format[ResultPod]
  case class ResultPod(title: String, result: String) {
    def toJson = {
      Json.toJson[ResultPod](this)
    }
  }

  def evalLatex(input: String): Seq[ResultPod] = {
    val q = engine.createQuery
    q.setInput(input)
    try {
      Logger.info("Engine URL" + engine.toURL(q))
      val result = engine.performQuery(q)
      result.isError match {
        case true =>
          Logger.warn("QUERY failed, ERROR CODE" + result.getErrorCode + " ERROR MESSAGE" + result.getErrorMessage)
          fail
        case false =>
          result.isSuccess match {
            case true =>
              val podHierarchy = result.getPods flatMap { pod =>
                pod.isError match {
                  case true => None
                  case false =>
                    val content = pod.getSubpods.map {subpod =>
                      subpod.getContents.flatMap {element =>
                        element match {
                          case ele: WAPlainText => Some(ele.getText)
//                          case ele: WAImage => Some("Image : YO : " + ele.getURL)
//                          case ele: WAUnits => Some("Units : YO : " + ele.getImage.getURL)
//                          case ele: WARelatedLink => Some("Related Link : YO : " + ele.getText)
//                          case ele: WASourceInfo => Some("Source Info : YO : " + ele.getFormattedURL)
//                          case ele: WASound => Some("Sound : YO : " + ele.getURL)
//                          case ele: WAInfo => Some("Info : YO : " + ele.getText)
//                          case ele: WAFutureTopic => Some("Future Topic : YO : " + ele.getTopic)
                          case _ => None
                        }
                      }.mkString(" ")
                    }.mkString("\n")
                    Some(ResultPod(pod.getTitle, content))
                }
              }
              podHierarchy toSeq
            case false =>
              Logger.info("Query was not understood; no results available.")
              fail
          }
      }
    } catch {
      case e: WAException =>
        Logger.warn(e.getStackTrace.toString)
        fail
    }
  }
}


