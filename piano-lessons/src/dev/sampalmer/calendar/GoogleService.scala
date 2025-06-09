package dev.sampalmer.calendar

import com.auth0.jwt.JWT
import com.auth0.jwt.algorithms.Algorithm
import sttp.client3.*
import upickle.default.{ReadWriter, write}

import java.security.interfaces.RSAPrivateKey
import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalTime, ZonedDateTime}
import java.util.Base64

object GoogleService {

  val backend: SttpBackend[Identity, Any] = HttpClientSyncBackend()

  def getCalendarId: String = {
    val accessToken: String = createAccessToken
    val resp = basicRequest
      .get(uri"https://www.googleapis.com/calendar/v3/users/me/calendarList")
      .header("Authorization", s"Bearer $accessToken")
      .response(asStringAlways)
      .send(backend)
    ujson.read(resp.body)("items").arr.toList
      .filter(_.apply("id").str.startsWith("clairelpalmer4"))
      .map(_.apply("id").str).head
  }

  def listEvents(calendarId: String, startTime: ZonedDateTime, endTime: ZonedDateTime): List[EventRange] = {
    val accessToken = createAccessToken
    val url = s"https://www.googleapis.com/calendar/v3/calendars/$calendarId/events"
    val response = basicRequest
      .get(uri"https://www.googleapis.com/calendar/v3/calendars/$calendarId/events"
        .addParam("timeMin", startTime.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME))
        .addParam("timeMax", endTime.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME))
      )
      .header("Authorization", s"Bearer $accessToken")
      .response(asStringAlways)
      .send(backend)


    val eventsJson = ujson.read(response.body)
    val events = eventsJson("items").arr

    // Print event summaries and start times
    events.flatMap { event =>
      val summary = event.obj.get("summary").map(_.str).getOrElse("(no title)")
      if event("start").obj.contains("dateTime") && event("end").obj.contains("dateTime") then
        val start = ZonedDateTime.parse(event("start")("dateTime").str).toLocalTime // or .obj.get("date").getOrElse(...)
        val end = ZonedDateTime.parse(event("end")("dateTime").str).toLocalTime // or .obj.get("date").getOrElse(...)
        Option(EventRange(start, end))
      else
        None
    }.toList
  }

  def addEvent(calendarId: String, event: Event): Unit =
    val url = uri"https://www.googleapis.com/calendar/v3/calendars/$calendarId/events"

    val accessToken = createAccessToken

    val eventSummary = EventSummary(
      summary = event.summary,
      description = event.description,
      start = EventDateTime(event.start.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)),
      end = EventDateTime(event.end.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME))
    )

    val request = basicRequest
      .post(url)
      .header("Authorization", s"Bearer $accessToken")
      .contentType("application/json")
      .body(write(eventSummary))
      .response(asStringAlways)

    val response = request.send(backend)
    println(response)



  private def createAccessToken = {
    val serviceAccountJson = ujson.read(sys.env("SERVICE_ACCOUNT_CONFIG"))
    val clientEmail = serviceAccountJson("client_email").str
    val privateKeyPem = serviceAccountJson("private_key").str

    // Parse PEM private key
    def loadPrivateKey(pem: String): RSAPrivateKey = {
      import java.security.KeyFactory
      import java.security.spec.PKCS8EncodedKeySpec

      val cleaned = pem
        .replace("-----BEGIN PRIVATE KEY-----", "")
        .replace("-----END PRIVATE KEY-----", "")
        .replaceAll("\\s", "")

      val decoded = Base64.getDecoder.decode(cleaned)
      val keySpec = new PKCS8EncodedKeySpec(decoded)
      val kf = KeyFactory.getInstance("RSA")
      kf.generatePrivate(keySpec).asInstanceOf[RSAPrivateKey]
    }

    val privateKey = loadPrivateKey(privateKeyPem)

    // Create signed JWT
    val now = Instant.now.getEpochSecond
    val jwt = JWT.create()
      .withIssuer(clientEmail)
      .withSubject(clientEmail)
      .withAudience("https://oauth2.googleapis.com/token")
      .withClaim("scope", "https://www.googleapis.com/auth/calendar")
      .withIssuedAt(java.util.Date.from(Instant.ofEpochSecond(now)))
      .withExpiresAt(java.util.Date.from(Instant.ofEpochSecond(now + 3600)))
      .sign(Algorithm.RSA256(null, privateKey))

    // Exchange JWT for access token
    val tokenResp = basicRequest
      .post(uri"https://oauth2.googleapis.com/token")
      .body(Map(
        "grant_type" -> "urn:ietf:params:oauth:grant-type:jwt-bearer",
        "assertion" -> jwt
      ))
      .response(asStringAlways)
      .send(backend)

    val accessToken = ujson.read(tokenResp.body)("access_token").str
    accessToken
  }

  case class Event(start: ZonedDateTime, end: ZonedDateTime, summary: String, description: String)

  case class EventRange(start: LocalTime, end: LocalTime) {
    def overlaps(range: EventRange): Boolean = {
      range.start.isBefore(end) && start.isBefore(range.end)
    }
  }

  case class EventDateTime(dateTime: String) derives ReadWriter

  case class EventSummary(
                           summary: String,
                           description: String,
                           start: EventDateTime,
                           end: EventDateTime
                         ) derives ReadWriter
}
