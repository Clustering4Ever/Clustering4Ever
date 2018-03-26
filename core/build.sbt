val coreName = "core"

name := coreName

exportJars := true

artifactName := { (sv: ScalaVersion, module: ModuleID, artifact: Artifact) => coreName + "." + artifact.extension }