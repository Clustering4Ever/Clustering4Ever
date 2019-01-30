import sbt._
import Keys._

val sparkVersion = "2.3.0"

lazy val mergeStrategyC4E = assemblyMergeStrategy in assembly := {
	case PathList("org", "xmlpull", xs @ _*) => MergeStrategy.last
	case PathList("META-INF", "io.netty", xs @ _*) => MergeStrategy.last
    case x if x.endsWith("io.netty.versions.properties") => MergeStrategy.last
    case x => val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}

lazy val sparkDeps = libraryDependencies ++= Seq(
	   	"org.apache.spark" %% "spark-core" % sparkVersion % "provided",
		"org.apache.spark"  %% "spark-mllib"  % sparkVersion % "provided"
)

lazy val coreDeps = libraryDependencies ++= Seq(
		"org.scalanlp" %% "breeze-natives" % "0.13.2",//exclude("com.github.fommil.netlib", "core") exclude("org.apache.commons", "commons-math3"),
		"org.scalanlp" %% "breeze" % "0.13.2",//exclude("com.github.fommil.netlib", "core") exclude("org.apache.commons", "commons-math3"),
		"org.typelevel" %% "spire" % "0.16.0",
		"com.chuusai" %% "shapeless" % "2.3.3",
		compilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
	  	// "com.beachape" %% "enumeratum" % "1.5.13"
)

lazy val commonCredentialsAndResolvers = Seq(
		resolvers += Resolver.sonatypeRepo("releases"),
		resolvers += "Spark Packages Repo" at "https://dl.bintray.com/spark-packages/maven",
		resolvers += "Sbt plugins" at "https://dl.bintray.com/sbt/sbt-plugin-releases",
		resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
		credentials += Credentials(Path.userHome / ".sbt" / "credentials"),
		credentials += Credentials(Path.userHome / ".sbt" / "1.0" / "sonatype_credential"),
		credentials += Credentials(Path.userHome / ".sbt" / "1.0" / "pgp.credentials"),
		assemblyOption in assembly := (assemblyOption in assembly).value.copy(cacheUnzip = true),
		assemblyOption in assembly := (assemblyOption in assembly).value.copy(cacheOutput = false)
)

lazy val commonSettingsC4E = Seq(
		organization := "org.clustering4ever",
		bintrayRepository := "C4E",
	 	version := "0.8.5-SNAPSHOT",
		scalaVersion := "2.11.12",
		conflictManager := ConflictManager.all,
		autoAPIMappings := true,
		licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0.html")),
		bintrayOrganization := Some("clustering4ever"),
		credentials += Credentials(Path.userHome / ".bintray" / ".credentials"),
		scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
)

lazy val core = (project in file("core"))
	.settings(commonSettingsC4E:_*)
	.settings(mergeStrategyC4E)
	.settings(coreDeps)

lazy val clusteringScala = (project in file("clustering/scala"))
	.settings(commonSettingsC4E:_*)
	.settings(mergeStrategyC4E)
	.dependsOn(core)

lazy val clusteringSpark = (project in file("clustering/spark"))
	.settings(commonSettingsC4E:_*)
	.settings(mergeStrategyC4E)
	.settings(sparkDeps)
	.dependsOn(clusteringScala)

lazy val clustering4ever = (project in file("clustering4ever"))
	.settings(commonSettingsC4E: _*)
  	.settings(name := "Clustering4Ever")
	.enablePlugins(ScalaUnidocPlugin)
	.dependsOn(clusteringSpark)
	.aggregate(core, clusteringScala, clusteringSpark)

// Sonatype deployment
// useGpg := true

publishTo := sonatypePublishTo.value

ThisBuild / organization := "org.clustering4ever"
ThisBuild / organizationName := "Clustering4Ever"
ThisBuild / organizationHomepage := Some(url("https://github.com/Clustering4Ever/Clustering4Ever/"))

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/Clustering4Ever/Clustering4Ever"),
    "scm:git@github.com:Clustering4Ever/Clustering4Ever.git"
  )
)
ThisBuild / developers := List(
  Developer(
    id    = "beckgael",
    name  = "Beck Gaël",
    email = "beck.gael@gmail.com",
    url   = url("http://www.beckgael.fr")
  )
)

ThisBuild / description := "Let's cluster the univers !"
ThisBuild / licenses := List("Apache-2.0" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / homepage := Some(url("https://github.com/Clustering4Ever/Clustering4Ever"))

// Remove all additional repository other than Maven Central from POM
ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
ThisBuild / publishMavenStyle := true