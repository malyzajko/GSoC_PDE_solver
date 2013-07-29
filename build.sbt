name := "GSOC Scala PDE solver"

version := "0.1"

// scala version used for this project
scalaVersion := "2.10.2"

scalaSource in Compile <<= baseDirectory(_ / "src/main")

scalaSource in Test <<= baseDirectory(_ / "src/test")

scalacOptions += "-deprecation"

fork in Test := true


