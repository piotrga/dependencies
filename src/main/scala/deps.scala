import com.sun.tools.classfile.Dependency

import java.nio.file.{Files, Path}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Try
import scala.xml.{Elem, XML}



case class ModuleId(groupId: String, artifactId: String, version: String):
  override def toString: String = s"$groupId:$artifactId:$version"
  def noVersion: String = s"$groupId:$artifactId"

case class Module(githubOrg: String, githubProject: String, id: ModuleId , dependencies: Seq[ModuleId]):
  def transitiveDependencies(all:Seq[Module]) : Seq[ModuleId] =
    println(s"Resolving deps for: ${id.noVersion}")
    dependencies ++ dependencies.flatMap(d => all.find(_.id.noVersion == d.noVersion)).flatMap(_.transitiveDependencies(all.filter(x => x.id.noVersion != id.noVersion)))

case class GitProject(organisation: String, name: String, modules: Seq[Module])

object Pom {
  given ec : scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
  def apply(githubOrg: String, githubProject: String, pom_xml: String, dependencyVersions: Map[(String, String), String] = Map())(using github: Github) : Future[Seq[Module]] =
    val doc = XML.loadString(pom_xml)

    val resolvedDeps = extractDependencies(doc, dependencyVersions)
    if ((doc \ "modules").isEmpty)
      Future(Seq(Module(githubOrg, githubProject, ModuleId((doc \ "groupId").headOption.getOrElse(doc \ "parent" \ "groupId").text, (doc \ "artifactId").text, (doc \ "version").text.trim), resolvedDeps)))
    else {
      val depsVersions = resolvedDeps.map(d => (d.groupId, d.artifactId) -> d.version).toMap
      Future.sequence(
        (doc \ "modules" \ "module")
          .map(module => {
            github
              .file(githubOrg, githubProject, s"${module.text}/pom.xml")
              .flatMap(Pom(githubOrg, githubProject, _, depsVersions))
              .recover(e =>
                println(s"Failed to resolve module ${githubOrg}/${githubProject}${module.text}: " + e.getMessage)
                None
              )
          })
      ).map(x =>x.flatten)
    }

  private def extractDependencies(doc: Elem, dependencyVersions: Map[(String, String), String] = Map()) = {
    val deps = (doc \\ "dependencies" \ "dependency")
      .map(x => ModuleId((x \ "groupId").text, (x \ "artifactId").text, (x \ "version").text.trim))
    val props: Map[String, String] = (doc \ "properties" \ "_").map(x => (x.label, x.text)).toMap
    val Exp = """\$\{(.+)\}""".r
    val resolvedDeps = deps.map(dep => dep.version match
      case Exp(prop) if props.contains(prop) => dep.copy(version = props(prop))
      case "" => dep.copy(version = dependencyVersions.getOrElse((dep.groupId, dep.artifactId), "???"))
      case _ => dep
    )
    resolvedDeps
  }
}

given ec: ExecutionContext = scala.concurrent.ExecutionContext.global
import scala.concurrent.duration._

@main
def main(organisation: String, GITHUB_TOKEN: String): Unit =
  given github : Github  = Github(GITHUB_TOKEN)

  val projects: Seq[GitProject] = Await.result(
    Future.sequence(
      github
        .projects(organisation)
        .map(project =>
          github
            .file(organisation, project.name, "pom.xml")
            .flatMap(pom_xml =>
              Pom(organisation, project.name, pom_xml).map(ms => Some(GitProject(organisation, project.name, ms)))
            )
            .recover(e =>
              println(s"Failed to resolve project ${project.full_name}: " + e.getMessage)
              None
            )
        )
    )
    , 1.minute).collect({case Some(p) => p})
  Files.writeString(Path.of(s"$organisation.html"), graphPage(projects))
  println("DONE")

//  println(g.projects(organisation).mkString("\n"))


def graphPage(graph: Seq[GitProject]) = s"""<html>
                                        |  <body>
                                        |    Here is one mermaid diagram:
                                        |    <pre class="mermaid">
                                        |graph LR
                                        |${graph.map(project(_, graph.flatMap(_.modules))).mkString("\n")}
                                        |    </pre>
                                        |    <script type="module">
                                        |      import mermaid from 'https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.esm.min.mjs';
                                        |      mermaid.initialize({ startOnLoad: true, maxTextSize: 100000 });
                                        |    </script>
                                        |  </body>
                                        |</html>""".stripMargin

def project(project: GitProject, all: Seq[Module]) =
  s"""|subgraph ${project.name}.git
      |${project.modules.map(m => s"    ${m.id.noVersion}").mkString("\n")}
      |end
     |${(project.modules.flatMap(_.transitiveDependencies(all)).flatMap(m => all.find(_.id == m)) ++ project.modules)
    .flatMap(m =>
      m.dependencies
        .filter(m => m.groupId.startsWith("tech.stage") || m.groupId.startsWith("com.iceservices"))
        .map(d => s"${m.id.noVersion}[${m.id.artifactId}] --> ${d.noVersion}[${d.artifactId}]")).mkString("\n")
  }""".stripMargin
