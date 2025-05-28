package dev.sampalmer.calendar

import dev.sampalmer.calendar.GoogleService.EventRange
import org.encalmo.aws.AwsClient
import org.encalmo.lambda.{LambdaContext, LambdaEnvironment, LambdaRuntime}
import org.encalmo.utils.JsonUtils.*
import upickle.default.*

import java.time.format.DateTimeFormatter
import java.time.{DayOfWeek, LocalDate, LocalTime, ZoneOffset, ZonedDateTime}
import java.util.{Calendar, Locale}
import scala.annotation.{static, tailrec}
import scala.language.experimental.namedTuples

object Lambda {

  @static def main(args: Array[String]): Unit = new Lambda().run()

  case class Config(greeting: String)derives ReadWriter

  case class HtmlResponse(statusCode: Int, body: String, headers: Map[String, String])derives ReadWriter

  case class QueryParameters(year: Int, month: Int)derives ReadWriter

  case class Request(rawPath: String, queryStringParameters: Option[QueryParameters] = None)derives ReadWriter


}

class Lambda(maybeAwsClient: Option[AwsClient]) extends LambdaRuntime {

  def this() = this(None)

  private val startOfDay = 9

  import Lambda.*

  type ApplicationContext = (config: Config, awsClient: AwsClient)

  override def initialize(using environment: LambdaEnvironment): ApplicationContext = {
    val awsClient = maybeAwsClient
      .getOrElse(AwsClient.initializeWithProperties(environment.maybeGetProperty))

    val greeting = environment
      .maybeGetProperty("LAMBDA_GREETING")
      .getOrElse("Hello <input>!")

    environment.info(
      s"Initializing ${environment.getFunctionName()} with a greeting $greeting"
    )

    val config = Config(greeting)

    (config, awsClient)
  }




  override inline def handleRequest(
                                     input: String
                                   )(using lambdaConfig: LambdaContext, context: ApplicationContext): String = {
    println(input)
    val request = read[Request](input)
    val body = request.rawPath match {
      case s"/calendar/$numOfLessons/$lengthOfLesson" => calendarList(numOfLessons.toInt, lengthOfLesson.toInt)
      case s"/times/$numOfLessons/$lengthOfLesson/$date" => times(numOfLessons.toInt, lengthOfLesson.toInt, date)
    }
    HtmlResponse(200, body, Map("Content-Type" -> "text/html")).writeAsString
  }

  private def isWeekend(date: String): Boolean = {
    val localDate = LocalDate.parse(date)
    List(DayOfWeek.SUNDAY, DayOfWeek.SATURDAY).contains(localDate.getDayOfWeek)
  }

  def times(numOfLessons: Int, lessonLength: Int, date: String): String = {
    val updateLessonLength = if (lessonLength == 0) 30 else lessonLength
    val endHour = if (isWeekend(date)) 12 else 20
    val slots = getAvailableSlots(date, numOfLessons, updateLessonLength, endHour)
    html.times(numOfLessons, lessonLength, date, slots).body
  }

  @tailrec
  private def getAllTimesForDay(increment: Int, allTimes: List[EventRange], endOfDay: Int, dayOfWeek: String): List[EventRange] = {
    val currentTime = allTimes.head.end
    if (dayOfWeek == "SATURDAY") {
      Nil
    } else if (currentTime.getHour == endOfDay) {
      allTimes
    } else {
      getAllTimesForDay(increment, EventRange(currentTime, currentTime.plusMinutes(increment)) :: allTimes, endOfDay, dayOfWeek)
    }
  }

  def getAvailableSlots(date: String, numberOfLessons: Int, lengthOfLesson: Int, endOfDay: Int): List[String] = {
    val googleService = new GoogleService()
    val dayOfWeek = LocalDate.parse(date).getDayOfWeek.name()
    val startTime = LocalTime.of(startOfDay, 0)
    val firstRange = EventRange(startTime, startTime.plusMinutes(lengthOfLesson))
    val allTimes = getAllTimesForDay(lengthOfLesson, firstRange :: Nil, endOfDay, dayOfWeek)
    val calendarId = googleService.getCalendarId
    val allLessonTimes = (0 until numberOfLessons).toList
      .map((plusWeeks: Int) => {
        val now = LocalDate.parse(date).plusWeeks(plusWeeks)
        now -> allTimes
      })
      .toMap
    val timesWithNoEvent = allLessonTimes.map { case (date, times) =>

      val start = date.atStartOfDay(ZoneOffset.UTC).withSecond(0)
      val end = date.plusDays(1).atStartOfDay(ZoneOffset.UTC).withSecond(0)
      val allEvents = googleService
        .listEvents(calendarId, start, end)
      date -> times.filter(time => !allEvents.exists(ev => ev.overlaps(time)))
    }
    val filtered = allTimes.filter(eachTime => timesWithNoEvent.count(_._2.contains(eachTime)) == numberOfLessons).sortBy(_.start)
    filtered.map(_.start.format(DateTimeFormatter.ofPattern("HH:mm")))
  }

  private def calendarList(numberOfLessons: Int, lessonLength: Int, monthOpt: Option[Int] = None, yearOpt: Option[Int] = None): String = {
    val calendar = Calendar.getInstance()
    val thisMonth = calendar.get(Calendar.MONTH)
    val thisYear = calendar.get(Calendar.YEAR)
    val month = monthOpt.getOrElse(thisMonth)
    val year = yearOpt.getOrElse(thisYear)
    calendar.set(Calendar.MONTH, month)
    calendar.set(Calendar.DAY_OF_MONTH, 1)
    val monthName = calendar.getDisplayName(Calendar.MONTH, Calendar.LONG, Locale.getDefault())
    val dayOfWeek = calendar.get(Calendar.DAY_OF_WEEK) - 1
    val maxDay = calendar.getActualMaximum(Calendar.DAY_OF_MONTH)
    val allDays = (1 to maxDay).map(_.toString)
    val days: List[String] = List.fill(dayOfWeek)("") ++ allDays
    html.calendar(numberOfLessons, lessonLength, days, monthName, month, month != thisMonth, year).body
  }

}
