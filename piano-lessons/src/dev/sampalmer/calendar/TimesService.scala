package dev.sampalmer.calendar

import dev.sampalmer.calendar.GoogleService.EventRange
import dev.sampalmer.calendar.Lambda.{HtmlResponse, contentTypeHeader}

import java.time.format.DateTimeFormatter
import java.time.{DayOfWeek, LocalDate, LocalTime, ZoneId, ZoneOffset}
import scala.annotation.tailrec
import scala.language.experimental.namedTuples

object TimesService:

  private val startOfDay = 9

  private def isWeekend(date: String): Boolean = {
    val localDate = LocalDate.parse(date)
    List(DayOfWeek.SUNDAY, DayOfWeek.SATURDAY).contains(localDate.getDayOfWeek)
  }

  def getTimes(date: String): HtmlResponse = {
    val endHour = if (isWeekend(date)) 12 else 20
    val slots = getAvailableSlots(date, endHour)

    val body = html.times(date, slots).body
    HtmlResponse(200, body, contentTypeHeader)
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

  private def getAvailableSlots(date: String, endOfDay: Int): List[String] = {
    val lengthOfLesson = 30
    val zoneId = ZoneId.of("Europe/London")
    val dayOfWeek = LocalDate.parse(date).getDayOfWeek.name()
    val startTime = LocalTime.of(startOfDay, 0)
    val firstRange = EventRange(startTime, startTime.plusMinutes(lengthOfLesson))
    val allTimes = getAllTimesForDay(lengthOfLesson, firstRange :: Nil, endOfDay, dayOfWeek)
    val calendarId = GoogleService.getCalendarId
    val thisDate = LocalDate.parse(date)
    val start = thisDate.atStartOfDay(zoneId)
    val end = thisDate.plusDays(1).atStartOfDay(zoneId)
    val allEvents = GoogleService
      .listEvents(calendarId, start, end)
    val filtered = allTimes.filter(eachTime => !allEvents.exists(_.overlaps(eachTime))).sortBy(_.start)
    filtered.map(_.start.format(DateTimeFormatter.ofPattern("HH:mm")))
  }

