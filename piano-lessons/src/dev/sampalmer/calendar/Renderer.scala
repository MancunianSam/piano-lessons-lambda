package dev.sampalmer.calendar

import play.twirl.api.HtmlFormat
import software.amazon.awssdk.core.sync.RequestBody
import software.amazon.awssdk.services.s3.S3Client
import software.amazon.awssdk.services.s3.model.PutObjectRequest

import java.nio.file.{Files, Path}

val client = S3Client.builder().build()

object Renderer extends App {
  BookingService.createBooking("2025-05-29", "10:00", Map("email" -> "test@test.com", "name" -> "bob", "phone" -> "1234"))
//  uploadHtml(html.index(), "index.html")
//  uploadHtml(html.policies(), "policies")
//  uploadHtml(html.whyPiano(), "why-piano")
//  uploadHtml(html.contact(sys.env("CONTACT_NUMBER")), "contact")
//  uploadAssets("images", "images/jpeg")
//  uploadAssets("stylesheets", "text/css")


  def uploadAssets(directory: String, contentType: String): Unit =
    Files.list(Path.of(s"piano-lessons/$directory")).forEach { file =>
      uploadObject(Files.readAllBytes(file), s"$directory/${file.getFileName}", contentType)
    }

  private def uploadHtml(htmlFormat: HtmlFormat.Appendable, key: String): Unit = uploadObject(htmlFormat.body.getBytes, key, "text/html")

  private def uploadObject(body: Array[Byte], key: String, contentType: String): Unit =
    val requestBody = RequestBody.fromBytes(body)
    val putObjectRequest = PutObjectRequest.builder()
      .contentType(contentType)
      .bucket("piano-lessons-news-feed-images")
      .key(key)
      .build()
    client.putObject(putObjectRequest, requestBody)
}