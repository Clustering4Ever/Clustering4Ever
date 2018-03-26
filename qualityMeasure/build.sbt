val qualityMeasureName = "qualityMeasure"

name := qualityMeasureName

exportJars := true

artifactName := { (sv: ScalaVersion, module: ModuleID, artifact: Artifact) => qualityMeasureName + "." + artifact.extension }