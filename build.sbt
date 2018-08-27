import sbt._
import Keys._

val sparkVersion = "2.1.1"

lazy val mergeStrategyC4E = assemblyMergeStrategy in assembly := {
	case PathList("org", "xmlpull", xs @ _*) => MergeStrategy.last
	case PathList("META-INF", "io.netty", xs @ _*) => MergeStrategy.last
    case x if x.endsWith("io.netty.versions.properties") => MergeStrategy.last
    case x => val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
	}

lazy val sparkDeps = libraryDependencies ++= Seq(
	   	"org.apache.spark" %% "spark-core" % sparkVersion % "provided",
		"org.apache.spark" %% "spark-sql" % sparkVersion % "provided",
		"org.apache.spark"  %% "spark-mllib"  % sparkVersion % "provided"
	)
lazy val scalaDeps = libraryDependencies ++= Seq(
	  //"org.scalanlp" %% "breeze" % "0.13.2",
	  //"org.scalanlp" %% "breeze-natives" % "0.13.2",
	  "org.apache.commons" % "commons-math3" % "3.6.1"
	)

lazy val commonCredentialsAndResolvers = Seq(
		resolvers += Resolver.sonatypeRepo("releases"),
		resolvers += "Spark Packages Repo" at "https://dl.bintray.com/spark-packages/maven",
		resolvers += "Sbt plugins" at "https://dl.bintray.com/sbt/sbt-plugin-releases",
		resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
		credentials += Credentials(Path.userHome / ".sbt" / "credentials"),
		assemblyOption in assembly := (assemblyOption in assembly).value.copy(cacheUnzip = true),
		assemblyOption in assembly := (assemblyOption in assembly).value.copy(cacheOutput = false)
	)

lazy val commonSettingsC4E = Seq(
		organization := "clustering4ever",
		bintrayRepository := "Clustering4Ever",
	 	version := "soma-SNAPSHOT",
		scalaVersion := "2.11.12",
		autoAPIMappings := true,
		licenses += ("Apache-2.0", url("https://www.apache.org/licenses/LICENSE-2.0.html")),
		bintrayOrganization := Some("clustering4ever"),
		credentials += Credentials(Path.userHome / ".bintray" / ".credentials")
	)

lazy val core = (project in file("core"))
	.settings(commonSettingsC4E:_*)
	.settings(mergeStrategyC4E)

lazy val clusteringScala = (project in file("clustering/scala"))
	.settings(commonSettingsC4E:_*)
	.settings(mergeStrategyC4E)
	.settings(scalaDeps)
	.dependsOn(core)

lazy val clusteringSpark = (project in file("clustering/spark"))
	.settings(commonSettingsC4E:_*)
	.settings(mergeStrategyC4E)
	.settings(sparkDeps)
	.dependsOn(core, clusteringScala)

lazy val documentation = (project in file("Documentation"))
	.settings(commonSettingsC4E: _*)
  	.settings( name := "documentation" )
	.enablePlugins(ScalaUnidocPlugin)
	.aggregate(core, clusteringScala, clusteringSpark)

lazy val clustering4ever = (project in file("Clustering4Ever"))
	.settings(commonSettingsC4E: _*)
  	.settings(name := "Clustering4Ever")
	.dependsOn(core, clusteringScala, clusteringSpark)
	.aggregate(core, clusteringScala, clusteringSpark)