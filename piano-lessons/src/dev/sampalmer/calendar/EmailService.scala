package dev.sampalmer.calendar

import dev.sampalmer.calendar.EmailService.*
import sttp.client3.*
import sttp.model.MediaType
import upickle.default.*

object EmailService:
  val backend: SttpBackend[Identity, Any] = HttpClientSyncBackend()

  def sendOrderEmail(emailAddress: String, order: Order): Int = {
    val fromEmail = "noreply@clairepalmerpiano.co.uk"
    val templateId = sys.env("SENDGRID_TEMPLATE")
    val personalisations = Personalisation(List(EmailRecipient(emailAddress)), OrderSummary(order)) :: Nil
    val email = write(Email(personalisations, EmailRecipient(fromEmail), templateId))
    val auth = s"Bearer ${sys.env("SENDGRID_KEY")}"
    val response = basicRequest
      .contentType(MediaType.ApplicationJson)
      .body(email)
      .headers(Map("Authorization" -> auth))
      .post(uri"https://api.sendgrid.com/v3/mail/send")
      .send(backend)
    response.code.code
  }

  case class OrderDates(number: Int, date: String) derives ReadWriter

  case class OrderSummary(order: Order) derives ReadWriter

  case class Order(
                    name: String,
                    studentName: Option[String],
                    email: String,
                    phone: String,
                    title: String,
                    date: String
                  ) derives ReadWriter
  case class EmailRecipient(email: String) derives ReadWriter

  case class Personalisation(to: List[EmailRecipient], dynamic_template_data: OrderSummary) derives ReadWriter

  case class Email(personalizations: List[Personalisation], from: EmailRecipient, template_id: String) derives ReadWriter