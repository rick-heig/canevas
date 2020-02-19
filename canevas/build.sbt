import Dependencies._

ThisBuild / scalaVersion     := "2.13.0"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.cnv"
ThisBuild / organizationName := "cnv"
// ThisBuild / scalacOptions    := Seq("-deprecation")

lazy val root = (project in file("."))
  .settings(
    resolvers += Resolver.mavenLocal,
    name := "Canevas",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "org.apache.commons" % "commons-lang3" % "3.0",
    // https://biojava.org/
    // https://mvnrepository.com/artifact/org.biojava/biojava-core
    // https://mvnrepository.com/artifact/org.biojava/biojava-alignment
    libraryDependencies += "org.biojava" % "biojava-core" % "5.3.0",
    libraryDependencies += "org.biojava" % "biojava-alignment" % "5.3.0",
    // https://mvnrepository.com/artifact/org.graphstream/gs-core
    libraryDependencies += "org.graphstream" % "gs-core" % "1.3",
    // https://mvnrepository.com/artifact/org.scalaj/scalaj-http
    libraryDependencies += "org.scalaj" %% "scalaj-http" % "2.4.2",
    // https://mvnrepository.com/artifact/com.github.samtools/htsjdk
    libraryDependencies += "com.github.samtools" % "htsjdk" % "2.21.1",
    // https://mvnrepository.com/artifact/org.apache.commons/commons-math3
    libraryDependencies += "org.apache.commons" % "commons-math3" % "3.6.1"
  )
