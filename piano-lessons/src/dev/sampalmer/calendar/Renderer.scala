package dev.sampalmer.calendar

import play.twirl.api.HtmlFormat
import software.amazon.awssdk.core.sync.RequestBody
import software.amazon.awssdk.services.s3.S3Client
import software.amazon.awssdk.services.s3.model.PutObjectRequest

import java.net.URLDecoder
import java.nio.file.{Files, Path}

val client = S3Client.builder().build()

object Renderer extends App {
  val a = "sam.palmer2%40gmail.com"
  val b = "this+dog+really+smells"
  val c = URLDecoder.decode(a, "utf-8")
  val d = URLDecoder.decode(b, "utf-8")
  print(d)
//  TimesService.getTimes("2025-06-18")
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