package dev.sampalmer.calendar

import dev.sampalmer.calendar.Lambda.{HtmlResponse, QueryParameters, contentTypeHeader}

import java.util.{Calendar, Locale}

object CalendarService {
  
  def calendarList(queryParameters: Option[QueryParameters]): HtmlResponse = {
    val calendar = Calendar.getInstance()
    val thisMonth = calendar.get(Calendar.MONTH)
    val thisYear = calendar.get(Calendar.YEAR)
    val month = queryParameters.map(_.month).getOrElse(thisMonth)
    val year = queryParameters.map(_.year).getOrElse(thisYear)
    calendar.set(Calendar.MONTH, month)
    calendar.set(Calendar.DAY_OF_MONTH, 1)
    val monthName = calendar.getDisplayName(Calendar.MONTH, Calendar.LONG, Locale.getDefault())
    val dayOfWeek = calendar.get(Calendar.DAY_OF_WEEK) - 1
    val maxDay = calendar.getActualMaximum(Calendar.DAY_OF_MONTH)
    val allDays = (1 to maxDay).map(_.toString)
    val days: List[String] = List.fill(dayOfWeek)("") ++ allDays
    val body = html.calendar(days, monthName, month, month != thisMonth, year).body
    HtmlResponse(200, body, contentTypeHeader)
  }
}
