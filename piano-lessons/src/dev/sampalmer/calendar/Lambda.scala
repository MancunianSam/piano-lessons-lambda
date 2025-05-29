package dev.sampalmer.calendar

import dev.sampalmer.calendar.CalendarService.calendarList
import dev.sampalmer.calendar.TimesService.getTimes
import org.encalmo.aws.AwsClient
import org.encalmo.lambda.{LambdaContext, LambdaEnvironment, LambdaRuntime}
import org.encalmo.utils.JsonUtils.*
import upickle.default.*

import java.util.UUID
import scala.annotation.static
import scala.language.experimental.namedTuples

object Lambda {

  @static def main(args: Array[String]): Unit = new Lambda().run()

  val contentTypeHeader: Map[String, String] = Map("Content-Type" -> "text/html")

  case class Config()derives ReadWriter

  case class HtmlResponse(statusCode: Int, body: String, headers: Map[String, String]) derives ReadWriter

  case class Http(method: String) derives ReadWriter

  case class RequestContext(http: Http) derives ReadWriter

  case class QueryParameters(year: Int, month: Int) derives ReadWriter

  case class Request(rawPath: String, queryStringParameters: Option[QueryParameters] = None, requestContext: RequestContext, body: Option[String] = None) derives ReadWriter

}

class Lambda(maybeAwsClient: Option[AwsClient]) extends LambdaRuntime {

  def this() = this(None)

  import Lambda.*

  type ApplicationContext = (config: Config, awsClient: AwsClient)

  override def initialize(using environment: LambdaEnvironment): ApplicationContext = {
    val awsClient = maybeAwsClient
      .getOrElse(AwsClient.initializeWithProperties(environment.maybeGetProperty))

    (Config(), awsClient)
  }

  override inline def handleRequest(
                                     input: String
                                   )(using lambdaConfig: LambdaContext, context: ApplicationContext): String = {
    println(input)
    val request = read[Request](input)
    val response = request.rawPath match {
      case s"/calendar" => calendarList(request.queryStringParameters)
      case s"/times/$date" => getTimes(date)
      case s"/booking-contact/$date/$time" => BookingService.bookingContactPage(request, date, time)
      case s"/booking-summary/$date/$time" => BookingService.bookingSummary(date, time)
    }
    response.writeAsString
  }
}
