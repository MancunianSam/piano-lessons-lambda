package dev.sampalmer.calendar

import dev.sampalmer.calendar.EmailService.Order
import dev.sampalmer.calendar.GoogleService.Event
import dev.sampalmer.calendar.Lambda.{HtmlResponse, Request, contentTypeHeader}

import java.net.URLDecoder
import java.time.format.DateTimeFormatter
import java.time.*
import java.util.Base64
import scala.jdk.CollectionConverters.*

object BookingService:

  case class Error(fieldName: String, errorText: String)

  def bookingSummary(date: String, time: String): HtmlResponse = {
    HtmlResponse(200, html.bookingSummary(date, time).body, contentTypeHeader)
  }

  def bookingContactPage(request: Request, date: String, time: String): HtmlResponse = {
    val formData = getFormData(request.body)
    val errors = validateBody(formData)
    val method = request.requestContext.http.method
    if method == "GET" then
      val body = renderBookingPage(date, time, Map.empty)
      HtmlResponse(200, body, contentTypeHeader)
    else if method == "POST" && errors.nonEmpty then
      val body = renderBookingPage(date, time, errors)
      HtmlResponse(200, body, contentTypeHeader)
    else {
      createBooking(date, time, formData)
      HtmlResponse(303, "", Map("Location" -> s"/api/booking-summary/$date/$time"))
    }
  }

  private def renderBookingPage(date: String, time: String, errors: Map[String, String]): String = {
    html.bookingContactDetails(date, time, errors).body
  }

  def decode(str: Option[String]): Option[String] = str.map(s => URLDecoder.decode(s, "utf-8"))
  def decode(str: String): String = URLDecoder.decode(str, "utf-8")

  private def createBooking(date: String, time: String, formData: Map[String, String]): Unit = {
    val email = decode(formData("email"))
    val name = decode(formData("name"))
    val student = decode(formData.get("student"))
    val phone = decode(formData("phone"))
    val level = decode(formData.get("level"))
    val notes = decode(formData.get("notes"))
    val calendarId = GoogleService.getCalendarId
    val localDate = LocalDate.parse(date)
    val localTime = LocalTime.parse(time)
    val start = ZonedDateTime.of(LocalDateTime.of(localDate, localTime), ZoneId.of("Europe/London"))
    val end = start.plusMinutes(30)
    val formattedTime = start.format(DateTimeFormatter.ofPattern("EEEE d LLLL yyyy HH mm"))
    val order = Order(name, student, email, phone, "Thank you for your booking", s"$formattedTime")
    val description =
      s"""
         |Email: $email
         |Name: $name
         |Student Name: ${student.getOrElse("")}
         |Level: ${level.getOrElse("")}
         |Phone: $phone
         |Notes: ${notes.getOrElse("")}
         |""".stripMargin
    GoogleService.addEvent(calendarId, Event(start, end, s"Trial lesson for $email", description))
    EmailService.sendOrderEmail(email, order)
    EmailService.sendOrderEmail("clairelpalmer4@gmail.com", order.copy(title = "You have a new booking"))
  }

  private def getFormData(maybeBody: Option[String]): Map[String, String] = {
    maybeBody.map { body =>
      Base64.getDecoder.decode(body).map(_.toChar).mkString.split("&").flatMap(each => {
        val parts = each.split("=")
        if parts.length == 1 then None else Option(parts.head -> parts.last)
      }).toMap
    }.getOrElse(Map.empty)
  }

  private def validateBody(formData: Map[String, String]): Map[String, String] =
    List("email" -> "Please enter an email", "name" -> "Please enter your name", "phone" -> "Please enter a phone number").flatMap {
      case (fieldName, errorText) => if !formData.contains(fieldName) then Option(fieldName -> errorText) else None
    }.toMap
