package dev.sampalmer.calendar

import com.auth0.jwt.JWT
import com.auth0.jwt.algorithms.Algorithm
import dev.sampalmer.calendar.GoogleService.EventRange
import sttp.client3.*
import ujson.*

import java.security.interfaces.RSAPrivateKey
import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalTime, ZonedDateTime}
import java.util
import java.util.Base64

class GoogleService {

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
        .addParam("timeZone", "UTC")
      )
      .header("Authorization", s"Bearer $accessToken")
      .response(asStringAlways)
      .send(backend)


    val eventsJson = ujson.read(response.body)
    val events = eventsJson("items").arr

    // Print event summaries and start times
    events.map { event =>
      val summary = event.obj.get("summary").map(_.str).getOrElse("(no title)")
      val start = ZonedDateTime.parse(event("start")("dateTime").str).toLocalTime // or .obj.get("date").getOrElse(...)
      val end = ZonedDateTime.parse(event("end")("dateTime").str).toLocalTime // or .obj.get("date").getOrElse(...)
      EventRange(start, end)
    }.toList
  }
  //
  //  def addEvent(calendarId: String, event: Event): Event =
  //    calendarApi.events().insert(calendarId, event).execute()

  //  private def calendarApi: Calendar = {
  //    credentials.refresh()
  //    val adapter = new HttpCredentialsAdapter(credentials)
  //    new Calendar.Builder(transport, jsonFactory, adapter)
  //      .setApplicationName("Piano Lessons")
  //      .build()
  //  }

  private def createAccessToken = {
//    val serviceAccountJson = ujson.read(Files.readString(Paths.get("/opt/google-service-account.json")))
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
      .withClaim("scope", "https://www.googleapis.com/auth/calendar.readonly")
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
}
object GoogleService {
  case class Event(start: LocalTime, end: LocalTime)

  case class EventRange(start: LocalTime, end: LocalTime) {
    def overlaps(range: EventRange): Boolean = {
      range.start.isBefore(end) && start.isBefore(range.end)
    }
  }
}
