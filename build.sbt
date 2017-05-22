name := "scalua"

version := "0.0.1"

scalaVersion := "2.12.2"

lazy val macros = project.in(file("library")).settings()
lazy val demo = project.in(file("demo")).dependsOn(macros).settings()
