import io.circe.parser.decode

import java.net.URI
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import scala.util.Try
import io.circe.*
import io.circe.generic.auto.*
import io.circe.parser.*
import io.circe.syntax.*

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

package github:
  case class Project(name: String, full_name: String)

  class Github(GITHUB_TOKEN: String):
    given ec: scala.concurrent.ExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(50))

    private def get(url: String): HttpResponse[String] = {
      println(s"Fetching $url")
      val client = HttpClient.newHttpClient()
      val request = HttpRequest.newBuilder()
        .GET()
        .uri(URI.create(url))
        .header("Authorization", s"Bearer ${GITHUB_TOKEN}")
        .header("Accept", "application/vnd.github.v3+json")
        .build()

      val x = client.send(request, HttpResponse.BodyHandlers.ofString())
      println(s"Done fetching [$url]")
      x
    }


    def projects(organisation: String): Seq[Project] = {
      val url = s"https://api.github.com/orgs/$organisation/repos"


      val pages = Seq.unfold(Option(url)) {
        pageUrl =>
          pageUrl.map(u =>
            val res = get(u)
            val Link = """.*<(.+)>;\s+rel="next".*""".r
            val link = res.headers().firstValue("Link").orElse("") match
              case Link(link) => Some(link)
              case _ => None
            println(link)
            (res.body(), link)
          )
      }

      val all = pages.flatMap(p => decode[List[Project]](p).getOrElse(List.empty))
      all
    }

    def file(organisation: String, project: String, path: String): Future[String] = Future{
      val url = s"https://raw.githubusercontent.com/$organisation/$project/master/$path"
      val res = get(url)
      require(res.statusCode() == 200, s"Failed to fetch $url - ${res.statusCode()}")
      res.body
    }