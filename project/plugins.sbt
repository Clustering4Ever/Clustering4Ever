addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.5")
addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.4.1")
addSbtPlugin("org.foundweekends" % "sbt-bintray" % "0.5.3")
//addSbtPlugin("com.typesafe.sbt" % "sbt-site" % "1.2.0")
//addSbtPlugin("com.typesafe.sbt" % "sbt-ghpages" % "0.6.0")
//resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

resolvers += Resolver.url("bintray-sbt-plugin-releases", url("http://dl.bintray.com/content/sbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)
